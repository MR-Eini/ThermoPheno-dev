#' DWD access helpers (via rdwd)
#'
#' These functions access DWD Open Data through the `rdwd` package and cache
#' downloaded files in a user-defined local directory.
#' @keywords internal
NULL

# Internal wrappers to make tests mockable.
.tp_rdwd_select <- function(...) rdwd::selectDWD(...)
.tp_rdwd_data <- function(...) rdwd::dataDWD(...)
.tp_rdwd_read <- function(...) rdwd::readDWD(...)

assert_rdwd_available <- function() {
  if (!requireNamespace("rdwd", quietly = TRUE)) {
    stop("Package 'rdwd' is required for DWD data access. Install with install.packages('rdwd').", call. = FALSE)
  }
}

#' Prepare a local cache directory for DWD downloads
#' @param cache_dir Local directory outside the package repository.
#' @return Normalized path to cache directory.
#' @keywords internal
prepare_dwd_cache <- function(cache_dir) {
  if (missing(cache_dir) || !nzchar(cache_dir)) {
    stop("Please provide a non-empty cache_dir.", call. = FALSE)
  }
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
}

#' Download DWD daily temperature data for one station
#'
#' Uses `rdwd::selectDWD()`, `rdwd::dataDWD()`, and `rdwd::readDWD()` to fetch
#' and parse daily climate (`var = 'kl'`) data.
#'
#' @param station_id DWD station ID (e.g., "00386").
#' @param start_year First year to retain.
#' @param end_year Last year to retain.
#' @param cache_dir User cache path (outside the package directory).
#' @param period DWD period selection (`"historical"`, `"recent"`, or both).
#' @return Data frame with parsed DWD daily temperature records.
#' @keywords internal
get_dwd_daily_temperature <- function(station_id,
                                      start_year,
                                      end_year,
                                      cache_dir,
                                      period = c("historical", "recent")) {
  assert_rdwd_available()
  cache_dir <- prepare_dwd_cache(cache_dir)

  period <- match.arg(period, several.ok = TRUE)
  links <- .tp_rdwd_select(
    id = station_id,
    res = "daily",
    var = "kl",
    per = period,
    exactmatch = TRUE,
    failempty = TRUE
  )

  files <- .tp_rdwd_data(links, dir = cache_dir, read = FALSE, force = FALSE)
  if (length(files) == 0) {
    stop("No DWD temperature files were downloaded.", call. = FALSE)
  }

  tables <- lapply(files, .tp_rdwd_read)
  out <- dplyr::bind_rows(tables)

  date_col <- intersect(c("MESS_DATUM", "Datum", "date", "DATE"), names(out))
  if (length(date_col) == 0) stop("Could not find a date column in DWD temperature data.", call. = FALSE)

  out$date <- as.Date(as.character(out[[date_col[1]]]), format = "%Y%m%d")
  out$year <- as.integer(format(out$date, "%Y"))

  out <- dplyr::filter(out, !is.na(date), year >= start_year, year <= end_year)
  out
}

#' Download DWD crop phenology observations
#'
#' Downloads phenology ZIP files from DWD Open Data and reads them via
#' `rdwd::readDWD()` when possible.
#'
#' @param crop_pattern Regex to filter crop names/files (case-insensitive).
#' @param start_year First year to retain.
#' @param end_year Last year to retain.
#' @param cache_dir User cache path (outside package repository).
#' @param reporter_type Either `"annual_reporters"` or `"immediate_reporters"`.
#' @param period DWD period selection (`"historical"` or `"recent"`).
#' @return Data frame of phenology observations (best-effort standardization).
#' @keywords internal
get_dwd_crop_phenology <- function(crop_pattern,
                                   start_year,
                                   end_year,
                                   cache_dir,
                                   reporter_type = c("annual_reporters", "immediate_reporters"),
                                   period = c("historical", "recent")) {
  assert_rdwd_available()
  cache_dir <- prepare_dwd_cache(cache_dir)
  reporter_type <- match.arg(reporter_type)
  period <- match.arg(period)

  base_url <- sprintf(
    "https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/%s/crops/%s/",
    reporter_type,
    period
  )

  html <- paste(readLines(base_url, warn = FALSE), collapse = "\n")
  hrefs <- unlist(regmatches(html, gregexpr('href="[^"]+\\.zip"', html)))
  zips <- gsub('^href="|"$', '', hrefs)
  zips <- unique(zips[grepl(crop_pattern, zips, ignore.case = TRUE)])

  if (length(zips) == 0) {
    stop("No crop phenology files matched crop_pattern at DWD path.", call. = FALSE)
  }

  links <- paste0(base_url, zips)
  files <- .tp_rdwd_data(links, dir = cache_dir, read = FALSE, force = FALSE)

  tables <- lapply(files, function(f) {
    out <- try(.tp_rdwd_read(f), silent = TRUE)
    if (inherits(out, "try-error")) {
      txt <- utils::unzip(f, list = TRUE)
      first_txt <- txt$Name[grepl("\\.txt$", txt$Name, ignore.case = TRUE)][1]
      if (is.na(first_txt)) return(data.frame())
      extracted <- utils::unzip(f, files = first_txt, exdir = cache_dir, overwrite = FALSE)
      return(readr::read_delim(extracted, delim = ";", show_col_types = FALSE))
    }
    out
  })

  out <- dplyr::bind_rows(tables)

  if (nrow(out) == 0) return(out)

  year_col <- intersect(c("Jahr", "year", "YEAR", "JAHR"), names(out))
  if (length(year_col) > 0) {
    out$year <- as.integer(out[[year_col[1]]])
    out <- dplyr::filter(out, !is.na(year), year >= start_year, year <= end_year)
  }

  out
}

#' Build validation table and summary metrics
#'
#' @param observed Data frame with observed phenology dates.
#' @param simulated Data frame with simulated dates.
#' @param by Join key columns.
#' @param observed_date_col Column in `observed` with observed date.
#' @param simulated_date_col Column in `simulated` with simulated date.
#' @return A list with `table` and `metrics`.
#' @keywords internal
build_dwd_validation_table <- function(observed,
                                       simulated,
                                       by = c("year"),
                                       observed_date_col,
                                       simulated_date_col) {
  joined <- dplyr::inner_join(observed, simulated, by = by, suffix = c("_obs", "_sim"))

  obs <- as.Date(joined[[observed_date_col]])
  sim <- as.Date(joined[[simulated_date_col]])
  err <- as.numeric(sim - obs)

  out_table <- dplyr::mutate(
    joined,
    observed_date = obs,
    simulated_date = sim,
    error_days = err
  )

  valid <- out_table[!is.na(out_table$error_days), , drop = FALSE]
  mae <- if (nrow(valid) > 0) mean(abs(valid$error_days)) else NA_real_
  rmse <- if (nrow(valid) > 0) sqrt(mean(valid$error_days^2)) else NA_real_
  bias <- if (nrow(valid) > 0) mean(valid$error_days) else NA_real_

  r2 <- NA_real_
  if (nrow(valid) > 1 && stats::sd(as.numeric(valid$observed_date)) > 0 && stats::sd(as.numeric(valid$simulated_date)) > 0) {
    r <- stats::cor(as.numeric(valid$observed_date), as.numeric(valid$simulated_date), use = "complete.obs")
    r2 <- r^2
  }

  metrics <- data.frame(
    n = nrow(valid),
    MAE_days = mae,
    RMSE_days = rmse,
    bias_days = bias,
    R2 = r2
  )

  list(table = out_table, metrics = metrics)
}

#' Reproducible minimal DWD workflow example
#'
#' @param cache_dir Local cache directory chosen by the user.
#' @return List with temperature and phenology tables.
#' @keywords internal
example_dwd_workflow <- function(cache_dir) {
  temp <- get_dwd_daily_temperature(
    station_id = "00386",
    start_year = 2018,
    end_year = 2020,
    cache_dir = cache_dir,
    period = "historical"
  )

  pheno <- get_dwd_crop_phenology(
    crop_pattern = "hafer|oat",
    start_year = 2018,
    end_year = 2020,
    cache_dir = cache_dir,
    reporter_type = "annual_reporters",
    period = "historical"
  )

  list(temperature = temp, phenology = pheno)
}
