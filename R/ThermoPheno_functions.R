library(dplyr)
library(lubridate)

# Convert date-like inputs safely to Date objects.
safe_as_date <- function(x) as.Date(x)

# Create a Date from year and MM-DD text (e.g., 2020 + '04-15').
clamp_year_day <- function(year, mmdd) {
  as.Date(paste0(year, "-", mmdd))
}

# Standardize and prepare daily weather inputs for simulation.
# Expected columns: date, tmin, tmax (case-insensitive).
prepare_weather <- function(df) {
  names(df) <- tolower(names(df))
  required <- c("date", "tmin", "tmax")
  miss <- setdiff(required, names(df))
  if (length(miss) > 0) {
    stop(paste("Missing required weather columns:", paste(miss, collapse = ", ")))
  }

  validation <- validate_weather_dwd(df, strict = FALSE)
  if (!validation$ok) {
    stop(paste(validation$errors, collapse = '\n'))
  }

  df %>%
    mutate(
      date = safe_as_date(date),
      year = year(date),
      doy = yday(date),
      tmean = (tmin + tmax) / 2
    ) %>%
    arrange(date)
}

# Calculate daily thermal time (degree-days) using one of three methods.
calc_daily_tt <- function(tmin, tmax, t_base, t_opt = NA, t_max_cut = NA,
                          mode = c("simple", "capped", "triangular")) {
  mode <- match.arg(mode)
  tmean <- (tmin + tmax) / 2

  if (mode == "simple") {
    tt <- pmax(tmean - t_base, 0)
  } else if (mode == "capped") {
    if (is.na(t_opt)) t_opt <- 999
    tt <- pmax(pmin(tmean, t_opt) - t_base, 0)
  } else {
    if (is.na(t_opt) || is.na(t_max_cut)) {
      stop("For triangular mode, both t_opt and t_max_cut must be provided.")
    }
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
  }

  tt
}

# Estimate required thermal time from a baseline period.
estimate_required_tt <- function(weather,
                                 baseline_years,
                                 planting_mmdd,
                                 days_to_maturity,
                                 t_base,
                                 t_opt = NA,
                                 t_max_cut = NA,
                                 tt_mode = "simple",
                                 crop_type = c("summer", "winter"),
                                 winter_dormancy_temp = 0,
                                 vernalization_required = FALSE,
                                 vernalization_temp_min = 0,
                                 vernalization_temp_max = 10,
                                 vernalization_days_required = 30,
                                 spring_regrowth_temp = 5) {
  crop_type <- match.arg(crop_type)
  yrs <- sort(unique(weather$year))
  yrs <- yrs[yrs %in% baseline_years]
  if (length(yrs) == 0) stop("No overlap between baseline years and weather years.")

  if (crop_type == "summer") {
    yearly <- lapply(yrs, function(yr) {
      plant_date <- clamp_year_day(yr, planting_mmdd)
      end_date <- plant_date + days(days_to_maturity - 1)
      sub <- weather %>% filter(date >= plant_date, date <= end_date)

      if (nrow(sub) < days_to_maturity) {
        return(data.frame(year = yr, required_tt = NA_real_))
      }

      tt <- calc_daily_tt(sub$tmin, sub$tmax, t_base, t_opt, t_max_cut, mode = tt_mode)
      data.frame(year = yr, required_tt = sum(tt, na.rm = TRUE))
    }) %>% bind_rows()
  } else {
    yearly <- lapply(yrs, function(yr) {
      plant_date <- clamp_year_day(yr, planting_mmdd)
      end_date <- plant_date + days(days_to_maturity - 1)
      sub <- weather %>% filter(date >= plant_date, date <= end_date) %>% arrange(date)

      if (nrow(sub) < days_to_maturity) {
        return(data.frame(year = yr, required_tt = NA_real_))
      }

      vernal_days <- 0
      vern_sat <- ifelse(vernalization_required, FALSE, TRUE)
      regrowth_started <- FALSE
      cum_tt <- 0

      for (i in seq_len(nrow(sub))) {
        tmean_i <- sub$tmean[i]

        if (vernalization_required && !vern_sat) {
          if (tmean_i >= vernalization_temp_min && tmean_i <= vernalization_temp_max) {
            vernal_days <- vernal_days + 1
          }
          if (vernal_days >= vernalization_days_required) {
            vern_sat <- TRUE
          }
        }

        if (vern_sat && !regrowth_started && tmean_i >= spring_regrowth_temp) {
          regrowth_started <- TRUE
        }

        if (!vern_sat) {
          tt_i <- 0
        } else {
          tt_i <- calc_daily_tt(sub$tmin[i], sub$tmax[i], t_base, t_opt, t_max_cut, mode = tt_mode)
          if (tmean_i <= winter_dormancy_temp) {
            tt_i <- 0
          }
        }

        cum_tt <- cum_tt + tt_i
      }

      data.frame(year = yr, required_tt = cum_tt)
    }) %>% bind_rows()
  }

  valid <- yearly %>% filter(is.finite(required_tt))
  if (nrow(valid) == 0) stop("Could not estimate required thermal time from baseline period.")

  list(
    yearly_required_tt = yearly,
    required_tt = mean(valid$required_tt, na.rm = TRUE)
  )
}

# Find first valid planting day inside user-defined window.
find_planting_date <- function(weather_window,
                               earliest_planting_date,
                               latest_planting_date,
                               crop_type = c("summer", "winter"),
                               min_mean_temp_plant = -999,
                               winter_plant_temp_min = 5,
                               winter_plant_temp_max = 15) {
  crop_type <- match.arg(crop_type)

  candidates <- weather_window %>%
    filter(date >= earliest_planting_date, date <= latest_planting_date) %>%
    arrange(date)

  if (nrow(candidates) == 0) {
    return(list(found = FALSE, planting_date = as.Date(NA)))
  }

  for (i in seq_len(nrow(candidates))) {
    this_date <- candidates$date[i]
    tmean_today <- candidates$tmean[i]

    if (crop_type == "summer") {
      condition <- tmean_today >= min_mean_temp_plant
    } else {
      condition <- tmean_today >= winter_plant_temp_min &&
        tmean_today <= winter_plant_temp_max
    }

    if (condition) {
      return(list(found = TRUE, planting_date = this_date))
    }
  }

  list(found = FALSE, planting_date = as.Date(NA))
}

# Simulate one harvest year and return season-level outcomes.
simulate_one_year <- function(weather,
                              sim_year,
                              required_tt,
                              earliest_planting_mmdd,
                              latest_planting_mmdd,
                              latest_harvest_mmdd,
                              t_base,
                              t_opt = NA,
                              t_max_cut = NA,
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
  yr <- sim_year

  if (crop_type == "summer") {
    weather_window <- weather %>% filter(year == yr)
    earliest_planting_date <- clamp_year_day(yr, earliest_planting_mmdd)
    latest_planting_date <- clamp_year_day(yr, latest_planting_mmdd)
    latest_harvest_date <- clamp_year_day(yr, latest_harvest_mmdd)
  } else {
    weather_window <- weather %>%
      filter(date >= clamp_year_day(yr - 1, earliest_planting_mmdd),
             date <= clamp_year_day(yr, latest_harvest_mmdd))
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

  if (!plant_result$found) {
    return(data.frame(
      crop_name = crop_name,
      crop_type = crop_type,
      row_role = "harvest_year",
      year = yr,
      planting_date = as.Date(NA),
      maturity_date = as.Date(NA),
      harvest_date = as.Date(NA),
      season_length_days = NA_integer_,
      accumulated_tt = 0,
      required_tt = required_tt,
      maturity_fraction = 0,
      status = "not_planted",
      forced_harvest = FALSE,
      regrowth_started = FALSE,
      vernalization_satisfied = ifelse(crop_type == "winter" && vernalization_required, FALSE, NA),
      vernalization_days = NA_real_
    ))
  }

  planting_date <- plant_result$planting_date
  sim_period <- weather_window %>%
    filter(date >= planting_date, date <= latest_harvest_date) %>%
    arrange(date)

  if (nrow(sim_period) == 0) {
    return(data.frame(
      crop_name = crop_name,
      crop_type = crop_type,
      row_role = "harvest_year",
      year = yr,
      planting_date = planting_date,
      maturity_date = as.Date(NA),
      harvest_date = as.Date(NA),
      season_length_days = NA_integer_,
      accumulated_tt = 0,
      required_tt = required_tt,
      maturity_fraction = 0,
      status = "failed_after_planting",
      forced_harvest = FALSE,
      regrowth_started = FALSE,
      vernalization_satisfied = ifelse(crop_type == "winter" && vernalization_required, FALSE, NA),
      vernalization_days = NA_real_
    ))
  }

  sim_period$daily_tt <- 0
  sim_period$cum_tt <- 0

  vernal_days <- 0
  vern_sat <- ifelse(crop_type == "winter" && vernalization_required, FALSE, TRUE)
  regrowth_started <- FALSE
  cum_tt <- 0

  for (i in seq_len(nrow(sim_period))) {
    tmean_i <- sim_period$tmean[i]

    if (crop_type == "winter" && vernalization_required && !vern_sat) {
      if (tmean_i >= vernalization_temp_min && tmean_i <= vernalization_temp_max) {
        vernal_days <- vernal_days + 1
      }
      if (vernal_days >= vernalization_days_required) {
        vern_sat <- TRUE
      }
    }

    if (crop_type == "winter" && vern_sat && !regrowth_started && tmean_i >= spring_regrowth_temp) {
      regrowth_started <- TRUE
    }

    if (crop_type == "winter" && !vern_sat) {
      tt_i <- 0
    } else {
      tt_i <- calc_daily_tt(
        tmin = sim_period$tmin[i],
        tmax = sim_period$tmax[i],
        t_base = t_base,
        t_opt = t_opt,
        t_max_cut = t_max_cut,
        mode = tt_mode
      )
      if (crop_type == "winter" && tmean_i <= winter_dormancy_temp) {
        tt_i <- 0
      }
    }

    cum_tt <- cum_tt + tt_i
    sim_period$daily_tt[i] <- tt_i
    sim_period$cum_tt[i] <- cum_tt
  }

  accumulated_tt <- dplyr::last(sim_period$cum_tt)
  maturity_fraction <- accumulated_tt / required_tt
  mature_idx <- which(sim_period$cum_tt >= required_tt)

  if (length(mature_idx) > 0) {
    maturity_date <- sim_period$date[min(mature_idx)]
    harvest_date <- maturity_date
    accumulated_tt <- sim_period$cum_tt[min(mature_idx)]
    maturity_fraction <- accumulated_tt / required_tt

    return(data.frame(
      crop_name = crop_name,
      crop_type = crop_type,
      row_role = "harvest_year",
      year = yr,
      planting_date = planting_date,
      maturity_date = maturity_date,
      harvest_date = harvest_date,
      season_length_days = as.integer(harvest_date - planting_date) + 1,
      accumulated_tt = accumulated_tt,
      required_tt = required_tt,
      maturity_fraction = maturity_fraction,
      status = "mature",
      forced_harvest = FALSE,
      regrowth_started = regrowth_started,
      vernalization_satisfied = ifelse(crop_type == "winter" && vernalization_required, vern_sat, NA),
      vernalization_days = ifelse(crop_type == "winter" && vernalization_required, vernal_days, NA)
    ))
  }

  if (forced_harvest_allowed && maturity_fraction >= min_fraction_tt_for_forced_harvest) {
    harvest_date <- latest_harvest_date
    return(data.frame(
      crop_name = crop_name,
      crop_type = crop_type,
      row_role = "harvest_year",
      year = yr,
      planting_date = planting_date,
      maturity_date = as.Date(NA),
      harvest_date = harvest_date,
      season_length_days = as.integer(harvest_date - planting_date) + 1,
      accumulated_tt = accumulated_tt,
      required_tt = required_tt,
      maturity_fraction = maturity_fraction,
      status = "forced_harvest_immature",
      forced_harvest = TRUE,
      regrowth_started = regrowth_started,
      vernalization_satisfied = ifelse(crop_type == "winter" && vernalization_required, vern_sat, NA),
      vernalization_days = ifelse(crop_type == "winter" && vernalization_required, vernal_days, NA)
    ))
  }

  data.frame(
    crop_name = crop_name,
    crop_type = crop_type,
    row_role = "harvest_year",
    year = yr,
    planting_date = planting_date,
    maturity_date = as.Date(NA),
    harvest_date = as.Date(NA),
    season_length_days = as.integer(latest_harvest_date - planting_date) + 1,
    accumulated_tt = accumulated_tt,
    required_tt = required_tt,
    maturity_fraction = maturity_fraction,
    status = ifelse(crop_type == "winter" && vernalization_required && !vern_sat,
                    "insufficient_vernalization", "failed_to_mature"),
    forced_harvest = FALSE,
    regrowth_started = regrowth_started,
    vernalization_satisfied = ifelse(crop_type == "winter" && vernalization_required, vern_sat, NA),
    vernalization_days = ifelse(crop_type == "winter" && vernalization_required, vernal_days, NA)
  )
}

# Run simulations for all years in prepared weather data.
run_simulation <- function(weather,
                           crop_name,
                           required_tt,
                           earliest_planting_mmdd,
                           latest_planting_mmdd,
                           latest_harvest_mmdd,
                           t_base,
                           t_opt = NA,
                           t_max_cut = NA,
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
  yrs <- sort(unique(weather$year))

  if (crop_type == "summer") {
    out <- bind_rows(lapply(yrs, function(yr) {
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
    }))
  } else {
    first_year <- min(yrs)
    last_year <- max(yrs)

    planting_rows <- bind_rows(lapply(yrs[yrs < last_year], function(yr) {
      weather_window <- weather %>%
        filter(date >= clamp_year_day(yr, earliest_planting_mmdd),
               date <= clamp_year_day(yr, latest_planting_mmdd))

      plant_result <- find_planting_date(
        weather_window = weather_window,
        earliest_planting_date = clamp_year_day(yr, earliest_planting_mmdd),
        latest_planting_date = clamp_year_day(yr, latest_planting_mmdd),
        crop_type = crop_type,
        min_mean_temp_plant = min_mean_temp_plant,
        winter_plant_temp_min = winter_plant_temp_min,
        winter_plant_temp_max = winter_plant_temp_max
      )

      data.frame(
        crop_name = crop_name,
        crop_type = crop_type,
        row_role = "planting_year",
        year = yr,
        planting_date = if (plant_result$found) plant_result$planting_date else as.Date(NA),
        maturity_date = as.Date(NA),
        harvest_date = as.Date(NA),
        season_length_days = NA_integer_,
        accumulated_tt = NA_real_,
        required_tt = required_tt,
        maturity_fraction = NA_real_,
        status = if (plant_result$found) "planted_for_next_year" else "not_planted",
        forced_harvest = FALSE,
        regrowth_started = FALSE,
        vernalization_satisfied = NA,
        vernalization_days = NA_real_
      )
    }))

    harvest_rows <- bind_rows(lapply(yrs[yrs > first_year], function(yr) {
      full_res <- simulate_one_year(
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
      full_res$planting_date <- as.Date(NA)
      full_res$row_role <- "harvest_year"
      full_res
    }))

    out <- bind_rows(planting_rows, harvest_rows) %>% arrange(year, row_role)
  }

  out
}

# Provide default parameter presets by crop type.
default_parameters <- function(crop_type = c("summer", "winter")) {
  crop_type <- match.arg(crop_type)

  if (crop_type == "summer") {
    list(
      crop_name = "Maize",
      days_to_maturity = 140,
      t_base = 8,
      t_opt = 25,
      t_max_cut = 35,
      baseline_planting_mmdd = "04-15",
      earliest_planting_mmdd = "03-15",
      latest_planting_mmdd = "05-31",
      latest_harvest_mmdd = "10-01"
    )
  } else {
    list(
      crop_name = "Winter wheat",
      days_to_maturity = 300,
      t_base = 0,
      t_opt = 18,
      t_max_cut = 30,
      baseline_planting_mmdd = "10-01",
      earliest_planting_mmdd = "09-15",
      latest_planting_mmdd = "11-15",
      latest_harvest_mmdd = "08-01"
    )
  }
}
