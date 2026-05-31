# DWD access helpers. These are internal utilities and require the suggested package `rdwd`.

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
  if (any(missing)) out[missing] <- as.Date(chr[missing])
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

prepare_dwd_cache <- function(cache_dir) {
  if (missing(cache_dir) || length(cache_dir) != 1 || !nzchar(cache_dir)) {
    stop("Please provide a non-empty cache_dir.", call. = FALSE)
  }
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
}

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

get_dwd_daily_temperature <- function(station_id, start_year, end_year, cache_dir, period = c("historical", "recent")) {
  assert_rdwd_available()
  cache_dir <- prepare_dwd_cache(cache_dir)
  period <- match.arg(period, several.ok = TRUE)
  links <- .tp_rdwd_select(id = station_id, res = "daily", var = "kl", per = period, exactmatch = TRUE, failempty = TRUE)
  files <- .tp_rdwd_data(links, dir = cache_dir, read = FALSE, force = FALSE)
  if (length(files) == 0) stop("No DWD temperature files were downloaded.", call. = FALSE)
  read_args <- list(file = files)
  if (length(files) > 1) read_args$hr <- 4
  out <- do.call(.tp_rdwd_read, read_args)
  if (is.list(out) && !is.data.frame(out)) out <- do.call(rbind, out)
  out <- standardize_dwd_daily_temperature(out)
  out <- out[!is.na(out$date) & out$year >= start_year & out$year <= end_year, , drop = FALSE]
  if ("station_id" %in% names(out)) out <- out[out$station_id == .format_dwd_station_id(station_id), , drop = FALSE]
  out
}

build_dwd_validation_table <- function(observed, simulated, by = c("year"), observed_date_col, simulated_date_col) {
  if (!observed_date_col %in% names(observed)) stop(sprintf("Column `%s` was not found in observed data.", observed_date_col), call. = FALSE)
  if (!simulated_date_col %in% names(simulated)) stop(sprintf("Column `%s` was not found in simulated data.", simulated_date_col), call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required for this helper.", call. = FALSE)
  joined <- dplyr::inner_join(observed, simulated, by = by, suffix = c("_obs", "_sim"))
  obs <- .parse_dwd_date(joined[[observed_date_col]])
  sim <- .parse_dwd_date(joined[[simulated_date_col]])
  err <- as.numeric(sim - obs)
  out_table <- joined
  out_table$observed_date <- obs
  out_table$simulated_date <- sim
  out_table$error_days <- err
  valid <- out_table[!is.na(out_table$error_days), , drop = FALSE]
  mae <- if (nrow(valid) > 0) mean(abs(valid$error_days)) else NA_real_
  rmse <- if (nrow(valid) > 0) sqrt(mean(valid$error_days^2)) else NA_real_
  bias <- if (nrow(valid) > 0) mean(valid$error_days) else NA_real_
  r2 <- NA_real_
  if (nrow(valid) > 1 && stats::sd(as.numeric(valid$observed_date)) > 0 && stats::sd(as.numeric(valid$simulated_date)) > 0) {
    r <- stats::cor(as.numeric(valid$observed_date), as.numeric(valid$simulated_date), use = "complete.obs")
    r2 <- r^2
  }
  metrics <- data.frame(n = nrow(valid), MAE_days = mae, RMSE_days = rmse, bias_days = bias, R2 = r2)
  list(table = out_table, metrics = metrics)
}
