#' Validate weather input against ThermoPheno and DWD-like conventions
#'
#' Performs structural checks for weather tables used by ThermoPheno and
#' adds practical quality checks inspired by typical Deutscher Wetterdienst
#' (DWD) daily temperature datasets.
#'
#' @param df A data frame containing at least `date`, `tmin`, and `tmax`.
#' @param strict If `TRUE`, stop on validation errors. If `FALSE`, return
#'   issues in the output list.
#'
#' @return A list with `ok` (logical), `errors` (character), and
#'   `warnings` (character).
#' @keywords internal
validate_weather_dwd <- function(df, strict = FALSE) {
  errors <- character()
  warnings <- character()

  names(df) <- tolower(names(df))
  required <- c("date", "tmin", "tmax")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  if (length(errors) == 0) {
    parsed_dates <- as.Date(df$date)
    if (any(is.na(parsed_dates))) {
      errors <- c(errors, "Column `date` contains unparseable values (expected YYYY-MM-DD).")
    }

    if (!is.numeric(df$tmin) || !is.numeric(df$tmax)) {
      errors <- c(errors, "Columns `tmin` and `tmax` must be numeric.")
    } else {
      bad_order <- which(df$tmin > df$tmax)
      if (length(bad_order) > 0) {
        errors <- c(errors, sprintf("%d rows have tmin > tmax.", length(bad_order)))
      }

      extreme_min <- sum(df$tmin < -60, na.rm = TRUE)
      extreme_max <- sum(df$tmax > 60, na.rm = TRUE)
      if (extreme_min > 0 || extreme_max > 0) {
        warnings <- c(
          warnings,
          sprintf("Detected potentially unrealistic temperatures (tmin < -60: %d rows, tmax > 60: %d rows).", extreme_min, extreme_max)
        )
      }
    }

    if (length(parsed_dates) > 1 && all(!is.na(parsed_dates))) {
      date_dup <- any(duplicated(parsed_dates))
      if (date_dup) {
        warnings <- c(warnings, "Duplicate dates found; daily records should be unique per station/site.")
      }
      if (!all(order(parsed_dates) == seq_along(parsed_dates))) {
        warnings <- c(warnings, "Dates are not sorted; data will be reordered before simulation.")
      }
    }
  }

  ok <- length(errors) == 0
  if (strict && !ok) {
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }

  list(ok = ok, errors = errors, warnings = warnings)
}
