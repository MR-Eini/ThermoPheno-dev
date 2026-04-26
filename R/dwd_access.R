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

.find_dwd_column <- function(x, candidates) {
  normalized <- tolower(gsub("[^[:alnum:]]+", "_", names(x)))
  candidates <- tolower(gsub("[^[:alnum:]]+", "_", candidates))
  hit <- match(candidates, normalized, nomatch = 0)
  if (!any(hit > 0)) return(NA_character_)
  names(x)[hit[hit > 0][1]]
}

.parse_dwd_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  chr <- trimws(as.character(x))
  chr[chr %in% c("", "NA", "-999")] <- NA_character_
  chr <- sub("\\.0$", "", chr)

  out <- as.Date(chr, format = "%Y%m%d")
  missing <- is.na(out) & !is.na(chr)
  if (any(missing)) {
    out[missing] <- as.Date(chr[missing])
  }
  out
}

.dwd_numeric <- function(x) {
  out <- suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)))))
  out[out <= -999] <- NA_real_
  out
}

.format_dwd_station_id <- function(x) {
  chr <- trimws(as.character(x))
  chr[chr %in% c("", "NA")] <- NA_character_
  numeric_id <- grepl("^[0-9]+$", chr)
  chr[numeric_id] <- sprintf("%05d", as.integer(chr[numeric_id]))
  chr
}

.filter_optional <- function(df, column, values) {
  if (is.null(values) || length(values) == 0) return(df)
  if (!column %in% names(df)) {
    stop(sprintf("Cannot filter by `%s`; column was not found after DWD standardization.", column), call. = FALSE)
  }
  df[df[[column]] %in% values, , drop = FALSE]
}

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
  if (missing(cache_dir) || length(cache_dir) != 1 || !nzchar(cache_dir)) {
    stop("Please provide a non-empty cache_dir.", call. = FALSE)
  }
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
}

#' Standardize DWD daily climate columns for ThermoPheno
#' @param df Raw table returned by `rdwd::readDWD()` for daily `kl` data.
#' @return `df` with stable `date`, `year`, `station_id`, `tmin`, `tmax`, and
#'   `tmean` columns when available.
#' @keywords internal
standardize_dwd_daily_temperature <- function(df) {
  date_col <- .find_dwd_column(df, c("MESS_DATUM", "Datum", "date", "DATE"))
  if (is.na(date_col)) stop("Could not find a date column in DWD temperature data.", call. = FALSE)

  station_col <- .find_dwd_column(df, c("STATIONS_ID", "Stations_id", "station_id", "station"))
  tmin_col <- .find_dwd_column(df, c("TNK", "tmin", "temp_min", "temperature_min"))
  tmax_col <- .find_dwd_column(df, c("TXK", "tmax", "temp_max", "temperature_max"))
  tmean_col <- .find_dwd_column(df, c("TMK", "tmean", "temp_mean", "temperature_mean"))

  if (is.na(tmin_col) || is.na(tmax_col)) {
    stop("Could not find DWD daily minimum/maximum temperature columns (`TNK`/`TXK`).", call. = FALSE)
  }

  df$date <- .parse_dwd_date(df[[date_col]])
  df$year <- as.integer(format(df$date, "%Y"))
  if (!is.na(station_col)) df$station_id <- .format_dwd_station_id(df[[station_col]])
  df$tmin <- .dwd_numeric(df[[tmin_col]])
  df$tmax <- .dwd_numeric(df[[tmax_col]])
  df$tmean <- if (!is.na(tmean_col)) .dwd_numeric(df[[tmean_col]]) else (df$tmin + df$tmax) / 2
  df
}

#' Standardize DWD crop phenology columns for validation joins
#' @param df Raw phenology table returned by `rdwd::readDWD()`.
#' @param source_file Optional source filename used for traceability.
#' @return `df` with stable `station_id`, `year`, `crop_id`, `phenophase_id`,
#'   `observed_date`, and `observed_doy` columns when available.
#' @keywords internal
standardize_dwd_crop_phenology <- function(df, source_file = NA_character_) {
  if (nrow(df) == 0) return(df)

  station_col <- .find_dwd_column(df, c("Stations_id", "STATIONS_ID", "station_id", "station"))
  year_col <- .find_dwd_column(df, c("Referenzjahr", "Jahr", "year", "YEAR", "JAHR"))
  crop_col <- .find_dwd_column(df, c("Objekt_id", "object_id", "crop_id"))
  phase_col <- .find_dwd_column(df, c("Phase_id", "phenophase_id", "phase_id", "phase"))
  date_col <- .find_dwd_column(df, c("Eintrittsdatum", "entry_date", "observed_date", "date"))
  doy_col <- .find_dwd_column(df, c("Jultag", "doy", "day_of_year"))

  if (!is.na(station_col)) df$station_id <- .format_dwd_station_id(df[[station_col]])
  if (!is.na(year_col)) df$year <- as.integer(.dwd_numeric(df[[year_col]]))
  if (!is.na(crop_col)) df$crop_id <- as.integer(.dwd_numeric(df[[crop_col]]))
  if (!is.na(phase_col)) df$phenophase_id <- as.integer(.dwd_numeric(df[[phase_col]]))
  if (!is.na(date_col)) df$observed_date <- .parse_dwd_date(df[[date_col]])
  if (!is.na(doy_col)) df$observed_doy <- as.integer(.dwd_numeric(df[[doy_col]]))
  df$source_file <- basename(source_file)
  df
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

  read_args <- list(file = files)
  if (length(files) > 1) read_args$hr <- 4
  out <- do.call(.tp_rdwd_read, read_args)
  if (is.list(out) && !is.data.frame(out)) out <- dplyr::bind_rows(out)
  out <- standardize_dwd_daily_temperature(out)

  out <- out[!is.na(out$date) & out$year >= start_year & out$year <= end_year, , drop = FALSE]
  if ("station_id" %in% names(out)) {
    out <- out[out$station_id == .format_dwd_station_id(station_id), , drop = FALSE]
  }
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
#' @param station_id Optional DWD phenology station ID(s) to retain.
#' @param phase_id Optional DWD phenological phase ID(s) to retain.
#' @param object_id Optional DWD crop/object ID(s) to retain.
#' @return Data frame of phenology observations (best-effort standardization).
#' @keywords internal
get_dwd_crop_phenology <- function(crop_pattern,
                                   start_year,
                                   end_year,
                                   cache_dir,
                                   reporter_type = c("annual_reporters", "immediate_reporters"),
                                   period = c("historical", "recent"),
                                   station_id = NULL,
                                   phase_id = NULL,
                                   object_id = NULL) {
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
      out <- readr::read_delim(extracted, delim = ";", show_col_types = FALSE)
    }
    standardize_dwd_crop_phenology(out, source_file = f)
  })

  out <- dplyr::bind_rows(tables)

  if (nrow(out) == 0) return(out)

  if (!"year" %in% names(out)) {
    stop("Could not find a reference year column in DWD phenology data.", call. = FALSE)
  }
  out <- out[!is.na(out$year) & out$year >= start_year & out$year <= end_year, , drop = FALSE]
  out <- .filter_optional(out, "station_id", .format_dwd_station_id(station_id))
  out <- .filter_optional(out, "phenophase_id", as.integer(phase_id))
  out <- .filter_optional(out, "crop_id", as.integer(object_id))

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
  if (!observed_date_col %in% names(observed)) {
    stop(sprintf("Column `%s` was not found in observed data.", observed_date_col), call. = FALSE)
  }
  if (!simulated_date_col %in% names(simulated)) {
    stop(sprintf("Column `%s` was not found in simulated data.", simulated_date_col), call. = FALSE)
  }
  joined <- dplyr::inner_join(observed, simulated, by = by, suffix = c("_obs", "_sim"))

  obs <- .parse_dwd_date(joined[[observed_date_col]])
  sim <- .parse_dwd_date(joined[[simulated_date_col]])
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
