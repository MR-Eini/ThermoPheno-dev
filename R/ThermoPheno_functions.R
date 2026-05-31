#' Prepare daily weather data for ThermoPheno
#'
#' Standardises column names and adds `year`, `doy`, and `tmean` columns.
#'
#' @param df Data frame with at least `date`, `tmin`, and `tmax` columns.
#' @return A data frame sorted by date.
#' @export
prepare_weather <- function(df) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  names(df) <- tolower(names(df))
  required <- c("date", "tmin", "tmax")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required weather columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  df$date <- as.Date(df$date)
  if (any(is.na(df$date))) stop("`date` contains values that could not be converted to Date.", call. = FALSE)

  df$tmin <- suppressWarnings(as.numeric(df$tmin))
  df$tmax <- suppressWarnings(as.numeric(df$tmax))
  if (any(is.na(df$tmin)) || any(is.na(df$tmax))) {
    stop("`tmin` and `tmax` must be numeric and cannot contain NA after conversion.", call. = FALSE)
  }

  df$year <- as.integer(format(df$date, "%Y"))
  df$doy <- as.integer(format(df$date, "%j"))
  df$tmean <- (df$tmin + df$tmax) / 2
  df[order(df$date), , drop = FALSE]
}

clamp_year_day <- function(year, mmdd) {
  as.Date(paste0(as.integer(year), "-", mmdd))
}

#' Calculate daily thermal time
#'
#' @param tmin Daily minimum temperature in degrees Celsius.
#' @param tmax Daily maximum temperature in degrees Celsius.
#' @param t_base Base temperature.
#' @param t_opt Optimum temperature; required for `capped` and `triangular` modes.
#' @param t_max_cut Upper cutoff temperature; required for `triangular` mode.
#' @param mode One of `simple`, `capped`, or `triangular`.
#' @return Numeric vector of daily thermal time values.
#' @export
calc_daily_tt <- function(tmin, tmax, t_base, t_opt = NA_real_, t_max_cut = NA_real_,
                          mode = c("simple", "capped", "triangular")) {
  mode <- match.arg(mode)
  tmean <- (as.numeric(tmin) + as.numeric(tmax)) / 2

  if (mode == "simple") {
    return(pmax(tmean - t_base, 0))
  }

  if (mode == "capped") {
    if (is.na(t_opt)) stop("For capped mode, `t_opt` must be provided.", call. = FALSE)
    return(pmax(pmin(tmean, t_opt) - t_base, 0))
  }

  if (is.na(t_opt) || is.na(t_max_cut)) {
    stop("For triangular mode, both `t_opt` and `t_max_cut` must be provided.", call. = FALSE)
  }
  if (t_opt <= t_base) stop("`t_opt` must be greater than `t_base`.", call. = FALSE)
  if (t_max_cut <= t_opt) stop("`t_max_cut` must be greater than `t_opt`.", call. = FALSE)

  tt <- numeric(length(tmean))
  idx1 <- tmean <= t_base
  idx2 <- tmean > t_base & tmean <= t_opt
  idx3 <- tmean > t_opt & tmean < t_max_cut
  idx4 <- tmean >= t_max_cut

  tt[idx1] <- 0
  tt[idx2] <- tmean[idx2] - t_base
  peak <- t_opt - t_base
  tt[idx3] <- peak * (t_max_cut - tmean[idx3]) / (t_max_cut - t_opt)
  tt[idx4] <- 0
  tt[tt < 0] <- 0
  tt
}

#' Estimate required thermal time from a reference baseline period
#'
#' @param weather Prepared weather data returned by [prepare_weather()].
#' @param baseline_years Integer vector of baseline years.
#' @param planting_mmdd Reference planting date in `MM-DD` format.
#' @param days_to_maturity Reference number of days from planting to maturity.
#' @param t_base Base temperature.
#' @param t_opt Optimum temperature.
#' @param t_max_cut Upper cutoff temperature.
#' @param tt_mode Thermal-time method.
#' @param crop_type `summer` or `winter`.
#' @param winter_dormancy_temp Dormancy threshold for winter crops.
#' @param vernalization_required Logical; whether vernalization is required.
#' @param vernalization_temp_min Minimum temperature for vernalization days.
#' @param vernalization_temp_max Maximum temperature for vernalization days.
#' @param vernalization_days_required Required number of vernalization days.
#' @param spring_regrowth_temp Spring regrowth threshold.
#' @return A list with yearly thermal-time values and the mean required thermal time.
#' @export
estimate_required_tt <- function(weather,
                                 baseline_years,
                                 planting_mmdd,
                                 days_to_maturity,
                                 t_base,
                                 t_opt = NA_real_,
                                 t_max_cut = NA_real_,
                                 tt_mode = "simple",
                                 crop_type = c("summer", "winter"),
                                 winter_dormancy_temp = 0,
                                 vernalization_required = FALSE,
                                 vernalization_temp_min = 0,
                                 vernalization_temp_max = 10,
                                 vernalization_days_required = 30,
                                 spring_regrowth_temp = 5) {
  crop_type <- match.arg(crop_type)
  if (!"year" %in% names(weather) || !"tmean" %in% names(weather)) weather <- prepare_weather(weather)

  yrs <- sort(unique(weather$year))
  yrs <- yrs[yrs %in% baseline_years]
  if (length(yrs) == 0) stop("No overlap between baseline years and weather years.", call. = FALSE)

  out <- lapply(yrs, function(yr) {
    plant_year <- if (crop_type == "winter") yr else yr
    plant_date <- clamp_year_day(plant_year, planting_mmdd)
    end_date <- plant_date + as.integer(days_to_maturity - 1)
    sub <- weather[weather$date >= plant_date & weather$date <= end_date, , drop = FALSE]

    if (nrow(sub) < days_to_maturity) {
      return(data.frame(year = yr, required_tt = NA_real_, stringsAsFactors = FALSE))
    }

    if (crop_type == "summer") {
      tt <- calc_daily_tt(sub$tmin, sub$tmax, t_base, t_opt, t_max_cut, mode = tt_mode)
      return(data.frame(year = yr, required_tt = sum(tt, na.rm = TRUE), stringsAsFactors = FALSE))
    }

    vernal_days <- 0
    vern_sat <- !isTRUE(vernalization_required)
    cum_tt <- 0

    for (i in seq_len(nrow(sub))) {
      tmean_i <- sub$tmean[i]
      if (isTRUE(vernalization_required) && !vern_sat) {
        if (tmean_i >= vernalization_temp_min && tmean_i <= vernalization_temp_max) {
          vernal_days <- vernal_days + 1
        }
        if (vernal_days >= vernalization_days_required) vern_sat <- TRUE
      }

      if (!vern_sat) {
        tt_i <- 0
      } else {
        tt_i <- calc_daily_tt(sub$tmin[i], sub$tmax[i], t_base, t_opt, t_max_cut, mode = tt_mode)
        if (tmean_i <= winter_dormancy_temp) tt_i <- 0
      }
      cum_tt <- cum_tt + tt_i
    }

    data.frame(year = yr, required_tt = cum_tt, stringsAsFactors = FALSE)
  })

  yearly <- do.call(rbind, out)
  valid <- yearly[is.finite(yearly$required_tt), , drop = FALSE]
  if (nrow(valid) == 0) stop("Could not estimate required thermal time from baseline period.", call. = FALSE)

  list(
    yearly_required_tt = yearly,
    required_tt = mean(valid$required_tt, na.rm = TRUE)
  )
}

#' Find the first planting date within an allowed planting window
#'
#' @param weather_window Prepared weather data covering the planting window.
#' @param earliest_planting_date Earliest allowed planting date.
#' @param latest_planting_date Latest allowed planting date.
#' @param crop_type `summer` or `winter`.
#' @param min_mean_temp_plant Minimum daily mean temperature for summer-crop planting.
#' @param winter_plant_temp_min Minimum daily mean temperature for winter-crop planting.
#' @param winter_plant_temp_max Maximum daily mean temperature for winter-crop planting.
#' @return A list with `found` and `planting_date`.
#' @export
find_planting_date <- function(weather_window,
                               earliest_planting_date,
                               latest_planting_date,
                               crop_type = c("summer", "winter"),
                               min_mean_temp_plant = -999,
                               winter_plant_temp_min = 5,
                               winter_plant_temp_max = 15) {
  crop_type <- match.arg(crop_type)
  if (!"tmean" %in% names(weather_window)) weather_window <- prepare_weather(weather_window)

  candidates <- weather_window[weather_window$date >= earliest_planting_date & weather_window$date <= latest_planting_date, , drop = FALSE]
  candidates <- candidates[order(candidates$date), , drop = FALSE]

  if (nrow(candidates) == 0) return(list(found = FALSE, planting_date = as.Date(NA)))

  for (i in seq_len(nrow(candidates))) {
    tmean_today <- candidates$tmean[i]
    condition <- if (crop_type == "summer") {
      tmean_today >= min_mean_temp_plant
    } else {
      tmean_today >= winter_plant_temp_min && tmean_today <= winter_plant_temp_max
    }
    if (isTRUE(condition)) return(list(found = TRUE, planting_date = candidates$date[i]))
  }

  list(found = FALSE, planting_date = as.Date(NA))
}

#' Simulate one crop season
#'
#' @inheritParams estimate_required_tt
#' @param sim_year Harvest year to simulate.
#' @param required_tt Required thermal time.
#' @param earliest_planting_mmdd Earliest planting date in `MM-DD` format.
#' @param latest_planting_mmdd Latest planting date in `MM-DD` format.
#' @param latest_harvest_mmdd Latest harvest date in `MM-DD` format.
#' @param forced_harvest_allowed Logical; whether immature forced harvest is allowed.
#' @param min_fraction_tt_for_forced_harvest Minimum maturity fraction for forced harvest.
#' @param winter_plant_temp_min Minimum winter-crop planting temperature.
#' @param winter_plant_temp_max Maximum winter-crop planting temperature.
#' @param crop_name Crop label.
#' @return One-row data frame describing the simulated season.
#' @export
simulate_one_year <- function(weather,
                              sim_year,
                              required_tt,
                              earliest_planting_mmdd,
                              latest_planting_mmdd,
                              latest_harvest_mmdd,
                              t_base,
                              t_opt = NA_real_,
                              t_max_cut = NA_real_,
                              tt_mode = "simple",
                              crop_type = c("summer", "winter"),
                              min_mean_temp_plant = 0,
                              forced_harvest_allowed = TRUE,
                              min_fraction_tt_for_forced_harvest = 0,
                              winter_dormancy_temp = 0,
                              vernalization_required = FALSE,
                              vernalization_temp_min = 0,
                              vernalization_temp_max = 10,
                              vernalization_days_required = 30,
                              spring_regrowth_temp = 5,
                              winter_plant_temp_min = 5,
                              winter_plant_temp_max = 15,
                              crop_name = NA_character_) {
  crop_type <- match.arg(crop_type)
  if (!"year" %in% names(weather) || !"tmean" %in% names(weather)) weather <- prepare_weather(weather)

  yr <- as.integer(sim_year)
  if (crop_type == "summer") {
    weather_window <- weather[weather$year == yr, , drop = FALSE]
    earliest_planting_date <- clamp_year_day(yr, earliest_planting_mmdd)
    latest_planting_date <- clamp_year_day(yr, latest_planting_mmdd)
    latest_harvest_date <- clamp_year_day(yr, latest_harvest_mmdd)
  } else {
    weather_window <- weather[weather$date >= clamp_year_day(yr - 1, earliest_planting_mmdd) &
                                weather$date <= clamp_year_day(yr, latest_harvest_mmdd), , drop = FALSE]
    earliest_planting_date <- clamp_year_day(yr - 1, earliest_planting_mmdd)
    latest_planting_date <- clamp_year_day(yr - 1, latest_planting_mmdd)
    latest_harvest_date <- clamp_year_day(yr, latest_harvest_mmdd)
  }

  plant_result <- find_planting_date(
    weather_window = weather_window,
    earliest_planting_date = earliest_planting_date,
    latest_planting_date = latest_planting_date,
    crop_type = crop_type,
    min_mean_temp_plant = min_mean_temp_plant,
    winter_plant_temp_min = winter_plant_temp_min,
    winter_plant_temp_max = winter_plant_temp_max
  )

  base_row <- function(status, planting_date = as.Date(NA), maturity_date = as.Date(NA),
                       harvest_date = as.Date(NA), season_length_days = NA_integer_,
                       accumulated_tt = 0, maturity_fraction = 0,
                       forced_harvest = FALSE, regrowth_started = FALSE,
                       vernalization_satisfied = NA, vernalization_days = NA_real_) {
    data.frame(
      crop_name = crop_name,
      crop_type = crop_type,
      row_role = "harvest_year",
      year = yr,
      planting_date = as.Date(planting_date),
      maturity_date = as.Date(maturity_date),
      harvest_date = as.Date(harvest_date),
      season_length_days = season_length_days,
      accumulated_tt = accumulated_tt,
      required_tt = required_tt,
      maturity_fraction = maturity_fraction,
      status = status,
      forced_harvest = forced_harvest,
      regrowth_started = regrowth_started,
      vernalization_satisfied = vernalization_satisfied,
      vernalization_days = vernalization_days,
      stringsAsFactors = FALSE
    )
  }

  if (!plant_result$found) {
    return(base_row(
      status = "not_planted",
      vernalization_satisfied = if (crop_type == "winter" && isTRUE(vernalization_required)) FALSE else NA
    ))
  }

  planting_date <- plant_result$planting_date
  sim_period <- weather_window[weather_window$date >= planting_date & weather_window$date <= latest_harvest_date, , drop = FALSE]
  sim_period <- sim_period[order(sim_period$date), , drop = FALSE]

  if (nrow(sim_period) == 0) {
    return(base_row(
      status = "failed_after_planting",
      planting_date = planting_date,
      vernalization_satisfied = if (crop_type == "winter" && isTRUE(vernalization_required)) FALSE else NA
    ))
  }

  vernal_days <- 0
  vern_sat <- !(crop_type == "winter" && isTRUE(vernalization_required))
  regrowth_started <- FALSE
  cum_tt <- 0
  cum_vec <- numeric(nrow(sim_period))

  for (i in seq_len(nrow(sim_period))) {
    tmean_i <- sim_period$tmean[i]

    if (crop_type == "winter" && isTRUE(vernalization_required) && !vern_sat) {
      if (tmean_i >= vernalization_temp_min && tmean_i <= vernalization_temp_max) {
        vernal_days <- vernal_days + 1
      }
      if (vernal_days >= vernalization_days_required) vern_sat <- TRUE
    }

    if (crop_type == "winter" && vern_sat && !regrowth_started && tmean_i >= spring_regrowth_temp) {
      regrowth_started <- TRUE
    }

    if (crop_type == "winter" && !vern_sat) {
      tt_i <- 0
    } else {
      tt_i <- calc_daily_tt(sim_period$tmin[i], sim_period$tmax[i], t_base, t_opt, t_max_cut, mode = tt_mode)
      if (crop_type == "winter" && tmean_i <= winter_dormancy_temp) tt_i <- 0
    }

    cum_tt <- cum_tt + tt_i
    cum_vec[i] <- cum_tt
  }

  accumulated_tt <- cum_tt
  maturity_fraction <- if (isTRUE(required_tt > 0)) accumulated_tt / required_tt else NA_real_
  mature_idx <- which(cum_vec >= required_tt)

  if (length(mature_idx) > 0) {
    idx <- min(mature_idx)
    maturity_date <- sim_period$date[idx]
    accumulated_tt <- cum_vec[idx]
    maturity_fraction <- accumulated_tt / required_tt
    return(base_row(
      status = "mature",
      planting_date = planting_date,
      maturity_date = maturity_date,
      harvest_date = maturity_date,
      season_length_days = as.integer(maturity_date - planting_date) + 1,
      accumulated_tt = accumulated_tt,
      maturity_fraction = maturity_fraction,
      forced_harvest = FALSE,
      regrowth_started = regrowth_started,
      vernalization_satisfied = if (crop_type == "winter" && isTRUE(vernalization_required)) vern_sat else NA,
      vernalization_days = if (crop_type == "winter" && isTRUE(vernalization_required)) vernal_days else NA_real_
    ))
  }

  if (isTRUE(forced_harvest_allowed) && is.finite(maturity_fraction) &&
      maturity_fraction >= min_fraction_tt_for_forced_harvest) {
    return(base_row(
      status = "forced_harvest_immature",
      planting_date = planting_date,
      harvest_date = latest_harvest_date,
      season_length_days = as.integer(latest_harvest_date - planting_date) + 1,
      accumulated_tt = accumulated_tt,
      maturity_fraction = maturity_fraction,
      forced_harvest = TRUE,
      regrowth_started = regrowth_started,
      vernalization_satisfied = if (crop_type == "winter" && isTRUE(vernalization_required)) vern_sat else NA,
      vernalization_days = if (crop_type == "winter" && isTRUE(vernalization_required)) vernal_days else NA_real_
    ))
  }

  final_status <- if (crop_type == "winter" && isTRUE(vernalization_required) && !vern_sat) {
    "insufficient_vernalization"
  } else {
    "failed_to_mature"
  }

  base_row(
    status = final_status,
    planting_date = planting_date,
    season_length_days = as.integer(latest_harvest_date - planting_date) + 1,
    accumulated_tt = accumulated_tt,
    maturity_fraction = maturity_fraction,
    forced_harvest = FALSE,
    regrowth_started = regrowth_started,
    vernalization_satisfied = if (crop_type == "winter" && isTRUE(vernalization_required)) vern_sat else NA,
    vernalization_days = if (crop_type == "winter" && isTRUE(vernalization_required)) vernal_days else NA_real_
  )
}

#' Run ThermoPheno simulations across all years in a weather data set
#'
#' @inheritParams simulate_one_year
#' @return Data frame with one row per simulated harvest year.
#' @export
run_simulation <- function(weather,
                           crop_name,
                           required_tt,
                           earliest_planting_mmdd,
                           latest_planting_mmdd,
                           latest_harvest_mmdd,
                           t_base,
                           t_opt = NA_real_,
                           t_max_cut = NA_real_,
                           tt_mode = "simple",
                           crop_type = c("summer", "winter"),
                           min_mean_temp_plant = 0,
                           forced_harvest_allowed = TRUE,
                           min_fraction_tt_for_forced_harvest = 0,
                           winter_dormancy_temp = 0,
                           vernalization_required = FALSE,
                           vernalization_temp_min = 0,
                           vernalization_temp_max = 10,
                           vernalization_days_required = 30,
                           spring_regrowth_temp = 5,
                           winter_plant_temp_min = 5,
                           winter_plant_temp_max = 15) {
  crop_type <- match.arg(crop_type)
  if (!"year" %in% names(weather) || !"tmean" %in% names(weather)) weather <- prepare_weather(weather)
  yrs <- sort(unique(weather$year))
  if (crop_type == "winter") yrs <- yrs[yrs > min(yrs)]

  rows <- lapply(yrs, function(yr) {
    simulate_one_year(
      weather = weather,
      sim_year = yr,
      required_tt = required_tt,
      earliest_planting_mmdd = earliest_planting_mmdd,
      latest_planting_mmdd = latest_planting_mmdd,
      latest_harvest_mmdd = latest_harvest_mmdd,
      t_base = t_base,
      t_opt = t_opt,
      t_max_cut = t_max_cut,
      tt_mode = tt_mode,
      crop_type = crop_type,
      min_mean_temp_plant = min_mean_temp_plant,
      forced_harvest_allowed = forced_harvest_allowed,
      min_fraction_tt_for_forced_harvest = min_fraction_tt_for_forced_harvest,
      winter_dormancy_temp = winter_dormancy_temp,
      vernalization_required = vernalization_required,
      vernalization_temp_min = vernalization_temp_min,
      vernalization_temp_max = vernalization_temp_max,
      vernalization_days_required = vernalization_days_required,
      spring_regrowth_temp = spring_regrowth_temp,
      winter_plant_temp_min = winter_plant_temp_min,
      winter_plant_temp_max = winter_plant_temp_max,
      crop_name = crop_name
    )
  })

  do.call(rbind, rows)
}

#' Default ThermoPheno crop parameters
#'
#' @param crop_type `summer` or `winter`.
#' @return A named list of default crop parameters.
#' @export
default_parameters <- function(crop_type = c("summer", "winter")) {
  crop_type <- match.arg(crop_type)

  if (crop_type == "summer") {
    return(list(
      crop_name = "Maize",
      days_to_maturity = 140,
      t_base = 8,
      t_opt = 25,
      t_max_cut = 35,
      baseline_planting_mmdd = "04-15",
      earliest_planting_mmdd = "03-15",
      latest_planting_mmdd = "05-31",
      latest_harvest_mmdd = "10-01",
      min_mean_temp_plant = 8,
      forced_harvest_allowed = TRUE,
      min_fraction_tt_for_forced_harvest = 0.8,
      winter_dormancy_temp = 0,
      vernalization_required = FALSE,
      vernalization_days_required = 0,
      spring_regrowth_temp = 5,
      winter_plant_temp_min = 5,
      winter_plant_temp_max = 15
    ))
  }

  list(
    crop_name = "Winter wheat",
    days_to_maturity = 300,
    t_base = 0,
    t_opt = 18,
    t_max_cut = 30,
    baseline_planting_mmdd = "10-01",
    earliest_planting_mmdd = "09-15",
    latest_planting_mmdd = "11-15",
    latest_harvest_mmdd = "08-01",
    min_mean_temp_plant = 8,
    forced_harvest_allowed = TRUE,
    min_fraction_tt_for_forced_harvest = 0.8,
    winter_dormancy_temp = 0,
    vernalization_required = TRUE,
    vernalization_days_required = 45,
    spring_regrowth_temp = 5,
    winter_plant_temp_min = 5,
    winter_plant_temp_max = 15
  )
}

#' Compare observed and simulated phenological dates
#'
#' @param df Data frame containing observed and simulated date columns.
#' @param observed_col Name of observed date column.
#' @param simulated_col Name of simulated date column.
#' @return One-row data frame with validation metrics.
#' @export
compare_validation_metrics <- function(df, observed_col = "observed_date", simulated_col = "simulated_date") {
  if (!all(c(observed_col, simulated_col) %in% names(df))) {
    stop("Observed and simulated columns are not present in `df`.", call. = FALSE)
  }
  obs <- as.Date(df[[observed_col]])
  sim <- as.Date(df[[simulated_col]])
  ok <- !is.na(obs) & !is.na(sim)
  if (!any(ok)) {
    return(data.frame(n = 0, mae = NA_real_, rmse = NA_real_, bias = NA_real_, medae = NA_real_,
                      within_7_days_pct = NA_real_, within_14_days_pct = NA_real_, r2 = NA_real_))
  }
  e <- as.numeric(sim[ok] - obs[ok])
  r2 <- if (sum(ok) >= 2 && stats::sd(as.numeric(obs[ok])) > 0 && stats::sd(as.numeric(sim[ok])) > 0) {
    stats::cor(as.numeric(obs[ok]), as.numeric(sim[ok]))^2
  } else NA_real_
  data.frame(
    n = length(e),
    mae = mean(abs(e)),
    rmse = sqrt(mean(e^2)),
    bias = mean(e),
    medae = stats::median(abs(e)),
    within_7_days_pct = mean(abs(e) <= 7) * 100,
    within_14_days_pct = mean(abs(e) <= 14) * 100,
    r2 = r2
  )
}

#' Estimate required thermal time from observed planting and harvest dates
#'
#' @param weather Prepared weather data from prepare_weather().
#' @param observed_calendar Data frame with observed planting and harvest dates.
#' @param calibration_years Years used for thermal-time calibration.
#' @param planting_col Name of observed planting-date column.
#' @param harvest_col Name of observed harvest-date column.
#' @param year_col Name of crop-year column.
#' @param t_base Base temperature.
#' @param t_opt Optimum temperature.
#' @param t_max_cut Upper temperature cutoff.
#' @param tt_mode Thermal-time method: simple, capped, or triangular.
#' @param summary_fun Summary method for annual TT values: median or mean.
#' @param min_weather_coverage Minimum fraction of daily weather records required.
#'
#' @return A list containing yearly observed TT values and the estimated required TT.
#' @export
estimate_required_tt_from_observed <- function(
    weather,
    observed_calendar,
    calibration_years,
    planting_col = "observed_planting_date",
    harvest_col = "observed_harvest_date",
    year_col = "crop_year",
    t_base,
    t_opt = NA_real_,
    t_max_cut = NA_real_,
    tt_mode = "simple",
    summary_fun = c("median", "mean"),
    min_weather_coverage = 0.95
) {
  summary_fun <- match.arg(summary_fun)

  if (!is.data.frame(weather)) {
    stop("`weather` must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(observed_calendar)) {
    stop("`observed_calendar` must be a data frame.", call. = FALSE)
  }

  if (!all(c("date", "tmin", "tmax") %in% names(weather))) {
    stop("weather must contain `date`, `tmin`, and `tmax` columns.", call. = FALSE)
  }

  if (!inherits(weather$date, "Date") || !"tmean" %in% names(weather)) {
    weather <- prepare_weather(weather)
  }

  required_cols <- c(planting_col, harvest_col, year_col)
  missing_cols <- setdiff(required_cols, names(observed_calendar))

  if (length(missing_cols) > 0) {
    stop(
      "observed_calendar is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  obs <- observed_calendar |>
    dplyr::mutate(
      observed_planting_tmp = as.Date(.data[[planting_col]]),
      observed_harvest_tmp  = as.Date(.data[[harvest_col]]),
      crop_year_tmp         = as.integer(.data[[year_col]])
    ) |>
    dplyr::filter(
      crop_year_tmp %in% calibration_years,
      !is.na(observed_planting_tmp),
      !is.na(observed_harvest_tmp),
      observed_harvest_tmp >= observed_planting_tmp
    )

  if (nrow(obs) == 0) {
    stop("No valid observed planting-harvest pairs in calibration years.", call. = FALSE)
  }

  yearly_tt <- lapply(seq_len(nrow(obs)), function(i) {
    plant_date <- obs$observed_planting_tmp[i]
    harvest_date <- obs$observed_harvest_tmp[i]

    sub <- weather |>
      dplyr::filter(date >= plant_date, date <= harvest_date) |>
      dplyr::arrange(date)

    expected_days <- as.integer(harvest_date - plant_date) + 1
    coverage <- if (expected_days > 0) nrow(sub) / expected_days else 0

    if (nrow(sub) == 0 || coverage < min_weather_coverage) {
      return(data.frame(
        crop_year = obs$crop_year_tmp[i],
        observed_planting_date = plant_date,
        observed_harvest_date = harvest_date,
        expected_days = expected_days,
        available_weather_days = nrow(sub),
        weather_coverage = coverage,
        observed_required_tt = NA_real_
      ))
    }

    tt <- calc_daily_tt(
      tmin = sub$tmin,
      tmax = sub$tmax,
      t_base = t_base,
      t_opt = t_opt,
      t_max_cut = t_max_cut,
      mode = tt_mode
    )

    data.frame(
      crop_year = obs$crop_year_tmp[i],
      observed_planting_date = plant_date,
      observed_harvest_date = harvest_date,
      expected_days = expected_days,
      available_weather_days = nrow(sub),
      weather_coverage = coverage,
      observed_required_tt = sum(tt, na.rm = TRUE)
    )
  }) |>
    dplyr::bind_rows()

  valid_tt <- yearly_tt |>
    dplyr::filter(is.finite(observed_required_tt))

  if (nrow(valid_tt) == 0) {
    stop("Could not estimate observed thermal-time requirement.", call. = FALSE)
  }

  required_tt <- if (summary_fun == "median") {
    stats::median(valid_tt$observed_required_tt, na.rm = TRUE)
  } else {
    mean(valid_tt$observed_required_tt, na.rm = TRUE)
  }

  list(
    yearly_required_tt = yearly_tt,
    required_tt = required_tt,
    calibration_method = "observed_calendar",
    summary_fun = summary_fun,
    min_weather_coverage = min_weather_coverage,
    n_valid_years = nrow(valid_tt)
  )
}