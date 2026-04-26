packages <- c(
  "shiny", "bslib", "dplyr", "lubridate", "ggplot2", 
  "DT", "readr", "ggridges", "RColorBrewer", "tidyr", "tibble"
)
missing_pkgs <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(packages, library, character.only = TRUE))

# ============================================================
# CORE PHENOLOGY FUNCTIONS (Embedded to ensure sync with UI)
# ============================================================

safe_as_date <- function(x) as.Date(x)

validate_weather_dwd <- function(df, strict = FALSE) {
  errors <- character()
  warnings <- character()

  names(df) <- tolower(names(df))
  required <- c("date", "tmin", "tmax")
  miss <- setdiff(required, names(df))
  if (length(miss) > 0) errors <- c(errors, paste("Missing required weather columns:", paste(miss, collapse = ", ")))

  if (length(errors) == 0) {
    d <- as.Date(df$date)
    if (any(is.na(d))) errors <- c(errors, "Unparseable values found in `date` column.")
    if (!is.numeric(df$tmin) || !is.numeric(df$tmax)) errors <- c(errors, "`tmin` and `tmax` must be numeric.")
    if (is.numeric(df$tmin) && is.numeric(df$tmax) && any(df$tmin > df$tmax, na.rm = TRUE)) {
      errors <- c(errors, "At least one row has tmin > tmax.")
    }
    if (any(df$tmin < -60, na.rm = TRUE) || any(df$tmax > 60, na.rm = TRUE)) {
      warnings <- c(warnings, "Potentially unrealistic temperature values detected.")
    }
  }

  ok <- length(errors) == 0
  if (strict && !ok) stop(paste(errors, collapse = "\n"), call. = FALSE)
  list(ok = ok, errors = errors, warnings = warnings)
}

options(shiny.maxRequestSize = 200 * 1024^2)

clamp_year_day <- function(year, mmdd) {
  as.Date(paste0(year, "-", mmdd))
}

prepare_weather <- function(df) {
  names(df) <- tolower(names(df))
  required <- c("date", "tmin", "tmax")
  miss <- setdiff(required, names(df))
  if (length(miss) > 0) stop(paste("Missing required weather columns:", paste(miss, collapse = ", ")))
  validation <- validate_weather_dwd(df, strict = FALSE)
  if (!validation$ok) stop(paste(validation$errors, collapse = "\n"))

  df %>%
    mutate(
      date = safe_as_date(date),
      year = year(date),
      doy = yday(date),
      tmean = (tmin + tmax) / 2
    ) %>% arrange(date)
}

calc_daily_tt <- function(tmin, tmax, t_base, t_opt = NA, t_max_cut = NA, mode = c("simple", "capped", "triangular")) {
  mode <- match.arg(mode)
  tmean <- (tmin + tmax) / 2
  if (mode == "simple") {
    tt <- pmax(tmean - t_base, 0)
  } else if (mode == "capped") {
    if (is.na(t_opt)) t_opt <- 999
    tt <- pmax(pmin(tmean, t_opt) - t_base, 0)
  } else {
    if (is.na(t_opt) || is.na(t_max_cut)) stop("For triangular mode, both t_opt and t_max_cut must be provided.")
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

estimate_required_tt <- function(weather, baseline_years, planting_mmdd, days_to_maturity, t_base, t_opt = NA, t_max_cut = NA, tt_mode = "simple", crop_type = c("summer", "winter"), winter_dormancy_temp = 0, vernalization_required = FALSE, vernalization_temp_min = 0, vernalization_temp_max = 10, vernalization_days_required = 30, spring_regrowth_temp = 5) {
  crop_type <- match.arg(crop_type)
  yrs <- sort(unique(weather$year))
  yrs <- yrs[yrs %in% baseline_years]
  if (length(yrs) == 0) stop("No overlap between baseline years and weather years.")
  
  if (crop_type == "summer") {
    yearly <- lapply(yrs, function(yr) {
      plant_date <- clamp_year_day(yr, planting_mmdd)
      end_date <- plant_date + days(days_to_maturity - 1)
      sub <- weather %>% filter(date >= plant_date, date <= end_date)
      if (nrow(sub) < days_to_maturity) return(data.frame(year = yr, required_tt = NA_real_))
      tt <- calc_daily_tt(sub$tmin, sub$tmax, t_base, t_opt, t_max_cut, mode = tt_mode)
      data.frame(year = yr, required_tt = sum(tt, na.rm = TRUE))
    }) %>% bind_rows()
  } else {
    yearly <- lapply(yrs, function(yr) {
      plant_date <- clamp_year_day(yr, planting_mmdd)
      end_date <- plant_date + days(days_to_maturity - 1)
      sub <- weather %>% filter(date >= plant_date, date <= end_date) %>% arrange(date)
      if (nrow(sub) < days_to_maturity) return(data.frame(year = yr, required_tt = NA_real_))
      vernal_days <- 0
      vern_sat <- !vernalization_required
      regrowth_started <- FALSE
      cum_tt <- 0
      for (i in seq_len(nrow(sub))) {
        tmean_i <- sub$tmean[i]
        if (vernalization_required && !vern_sat) {
          if (tmean_i >= vernalization_temp_min && tmean_i <= vernalization_temp_max) vernal_days <- vernal_days + 1
          if (vernal_days >= vernalization_days_required) vern_sat <- TRUE
        }
        if (vern_sat && !regrowth_started && tmean_i >= spring_regrowth_temp) regrowth_started <- TRUE
        if (!vern_sat) {
          tt_i <- 0
        } else {
          tt_i <- calc_daily_tt(sub$tmin[i], sub$tmax[i], t_base, t_opt, t_max_cut, mode = tt_mode)
          if (tmean_i <= winter_dormancy_temp) tt_i <- 0
        }
        cum_tt <- cum_tt + tt_i
      }
      data.frame(year = yr, required_tt = cum_tt)
    }) %>% bind_rows()
  }
  valid <- yearly %>% filter(is.finite(required_tt))
  if (nrow(valid) == 0) stop("Could not estimate required thermal time from baseline period.")
  list(yearly_required_tt = yearly, required_tt = mean(valid$required_tt, na.rm = TRUE))
}

find_planting_date <- function(weather_window, earliest_planting_date, latest_planting_date, crop_type = c("summer", "winter"), min_mean_temp_plant = -999, winter_plant_temp_min = 5, winter_plant_temp_max = 15) {
  crop_type <- match.arg(crop_type)
  candidates <- weather_window %>% filter(date >= earliest_planting_date, date <= latest_planting_date) %>% arrange(date)
  if (nrow(candidates) == 0) return(list(found = FALSE, planting_date = as.Date(NA)))
  for (i in seq_len(nrow(candidates))) {
    this_date <- candidates$date[i]
    tmean_today <- candidates$tmean[i]
    condition <- if (crop_type == "summer") tmean_today >= min_mean_temp_plant else (tmean_today >= winter_plant_temp_min && tmean_today <= winter_plant_temp_max)
    if (condition) return(list(found = TRUE, planting_date = this_date))
  }
  list(found = FALSE, planting_date = as.Date(NA))
}

simulate_one_year <- function(weather, sim_year, required_tt, earliest_planting_mmdd, latest_planting_mmdd, latest_harvest_mmdd, t_base, t_opt = NA, t_max_cut = NA, tt_mode = "simple", crop_type = c("summer", "winter"), min_mean_temp_plant = 0, forced_harvest_allowed = TRUE, min_fraction_tt_for_forced_harvest = 0, winter_dormancy_temp = 0, vernalization_required = FALSE, vernalization_temp_min = 0, vernalization_temp_max = 10, vernalization_days_required = 30, spring_regrowth_temp = 5, winter_plant_temp_min = 5, winter_plant_temp_max = 15, crop_name = NA_character_) {
  crop_type <- match.arg(crop_type)
  yr <- sim_year
  
  if (crop_type == "summer") {
    weather_window <- weather %>% filter(year == yr)
    earliest_planting_date <- clamp_year_day(yr, earliest_planting_mmdd)
    latest_planting_date <- clamp_year_day(yr, latest_planting_mmdd)
    latest_harvest_date <- clamp_year_day(yr, latest_harvest_mmdd)
  } else {
    weather_window <- weather %>% filter(date >= clamp_year_day(yr - 1, earliest_planting_mmdd), date <= clamp_year_day(yr, latest_harvest_mmdd))
    earliest_planting_date <- clamp_year_day(yr - 1, earliest_planting_mmdd)
    latest_planting_date <- clamp_year_day(yr - 1, latest_planting_mmdd)
    latest_harvest_date <- clamp_year_day(yr, latest_harvest_mmdd)
  }
  
  plant_result <- find_planting_date(weather_window, earliest_planting_date, latest_planting_date, crop_type, min_mean_temp_plant, winter_plant_temp_min, winter_plant_temp_max)
  
  if (!plant_result$found) {
    return(data.frame(crop_name = crop_name, crop_type = crop_type, row_role = "harvest_year", year = yr, planting_date = as.Date(NA), maturity_date = as.Date(NA), harvest_date = as.Date(NA), season_length_days = NA_integer_, accumulated_tt = 0, required_tt = required_tt, maturity_fraction = 0, status = "not_planted", forced_harvest = FALSE, regrowth_started = FALSE, vernalization_satisfied = if(crop_type == "winter" && vernalization_required) FALSE else NA, vernalization_days = NA_real_))
  }
  
  planting_date <- plant_result$planting_date
  sim_period <- weather_window %>% filter(date >= planting_date, date <= latest_harvest_date) %>% arrange(date)
  
  if (nrow(sim_period) == 0) {
    return(data.frame(crop_name = crop_name, crop_type = crop_type, row_role = "harvest_year", year = yr, planting_date = planting_date, maturity_date = as.Date(NA), harvest_date = as.Date(NA), season_length_days = NA_integer_, accumulated_tt = 0, required_tt = required_tt, maturity_fraction = 0, status = "failed_after_planting", forced_harvest = FALSE, regrowth_started = FALSE, vernalization_satisfied = if(crop_type == "winter" && vernalization_required) FALSE else NA, vernalization_days = NA_real_))
  }
  
  sim_period$daily_tt <- 0
  sim_period$cum_tt <- 0
  vernal_days <- 0
  vern_sat <- !vernalization_required
  regrowth_started <- FALSE
  cum_tt <- 0
  
  for (i in seq_len(nrow(sim_period))) {
    tmean_i <- sim_period$tmean[i]
    if (crop_type == "winter" && vernalization_required && !vern_sat) {
      if (tmean_i >= vernalization_temp_min && tmean_i <= vernalization_temp_max) vernal_days <- vernal_days + 1
      if (vernal_days >= vernalization_days_required) vern_sat <- TRUE
    }
    if (crop_type == "winter" && vern_sat && !regrowth_started && tmean_i >= spring_regrowth_temp) regrowth_started <- TRUE
    if (crop_type == "winter" && !vern_sat) {
      tt_i <- 0
    } else {
      tt_i <- calc_daily_tt(sim_period$tmin[i], sim_period$tmax[i], t_base, t_opt, t_max_cut, mode = tt_mode)
      if (crop_type == "winter" && tmean_i <= winter_dormancy_temp) tt_i <- 0
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
    return(data.frame(crop_name = crop_name, crop_type = crop_type, row_role = "harvest_year", year = yr, planting_date = planting_date, maturity_date = maturity_date, harvest_date = harvest_date, season_length_days = as.integer(harvest_date - planting_date) + 1, accumulated_tt = accumulated_tt, required_tt = required_tt, maturity_fraction = accumulated_tt / required_tt, status = "mature", forced_harvest = FALSE, regrowth_started = regrowth_started, vernalization_satisfied = if(crop_type == "winter" && vernalization_required) vern_sat else NA, vernalization_days = if(crop_type == "winter" && vernalization_required) vernal_days else NA))
  }
  
  if (forced_harvest_allowed && maturity_fraction >= min_fraction_tt_for_forced_harvest) {
    harvest_date <- latest_harvest_date
    return(data.frame(crop_name = crop_name, crop_type = crop_type, row_role = "harvest_year", year = yr, planting_date = planting_date, maturity_date = as.Date(NA), harvest_date = harvest_date, season_length_days = as.integer(harvest_date - planting_date) + 1, accumulated_tt = accumulated_tt, required_tt = required_tt, maturity_fraction = maturity_fraction, status = "forced_harvest_immature", forced_harvest = TRUE, regrowth_started = regrowth_started, vernalization_satisfied = if(crop_type == "winter" && vernalization_required) vern_sat else NA, vernalization_days = if(crop_type == "winter" && vernalization_required) vernal_days else NA))
  }
  
  status_val <- if (crop_type == "winter" && vernalization_required && !vern_sat) "insufficient_vernalization" else "failed_to_mature"
  data.frame(crop_name = crop_name, crop_type = crop_type, row_role = "harvest_year", year = yr, planting_date = planting_date, maturity_date = as.Date(NA), harvest_date = as.Date(NA), season_length_days = as.integer(latest_harvest_date - planting_date) + 1, accumulated_tt = accumulated_tt, required_tt = required_tt, maturity_fraction = maturity_fraction, status = status_val, forced_harvest = FALSE, regrowth_started = regrowth_started, vernalization_satisfied = if(crop_type == "winter" && vernalization_required) vern_sat else NA, vernalization_days = if(crop_type == "winter" && vernalization_required) vernal_days else NA)
}

run_simulation <- function(weather, crop_name, required_tt, earliest_planting_mmdd, latest_planting_mmdd, latest_harvest_mmdd, t_base, t_opt = NA, t_max_cut = NA, tt_mode = "simple", crop_type = c("summer", "winter"), min_mean_temp_plant = 0, forced_harvest_allowed = TRUE, min_fraction_tt_for_forced_harvest = 0, winter_dormancy_temp = 0, vernalization_required = FALSE, vernalization_temp_min = 0, vernalization_temp_max = 10, vernalization_days_required = 30, spring_regrowth_temp = 5, winter_plant_temp_min = 5, winter_plant_temp_max = 15) {
  crop_type <- match.arg(crop_type)
  yrs <- sort(unique(weather$year))
  
  if (crop_type == "summer") {
    out <- bind_rows(lapply(yrs, function(yr) {
      simulate_one_year(weather, yr, required_tt, earliest_planting_mmdd, latest_planting_mmdd, latest_harvest_mmdd, t_base, t_opt, t_max_cut, tt_mode, crop_type, min_mean_temp_plant, forced_harvest_allowed, min_fraction_tt_for_forced_harvest, winter_dormancy_temp, vernalization_required, vernalization_temp_min, vernalization_temp_max, vernalization_days_required, spring_regrowth_temp, winter_plant_temp_min, winter_plant_temp_max, crop_name)
    }))
  } else {
    first_year <- min(yrs)
    last_year <- max(yrs)
    
    planting_rows <- bind_rows(lapply(yrs[yrs < last_year], function(yr) {
      weather_window <- weather %>% filter(date >= clamp_year_day(yr, earliest_planting_mmdd), date <= clamp_year_day(yr, latest_planting_mmdd))
      plant_result <- find_planting_date(weather_window, clamp_year_day(yr, earliest_planting_mmdd), clamp_year_day(yr, latest_planting_mmdd), crop_type, min_mean_temp_plant, winter_plant_temp_min, winter_plant_temp_max)
      data.frame(crop_name = crop_name, crop_type = crop_type, row_role = "planting_year", year = yr, planting_date = if (plant_result$found) plant_result$planting_date else as.Date(NA), maturity_date = as.Date(NA), harvest_date = as.Date(NA), season_length_days = NA_integer_, accumulated_tt = NA_real_, required_tt = required_tt, maturity_fraction = NA_real_, status = if (plant_result$found) "planted_for_next_year" else "not_planted", forced_harvest = FALSE, regrowth_started = FALSE, vernalization_satisfied = NA, vernalization_days = NA_real_)
    }))
    
    harvest_rows <- bind_rows(lapply(yrs[yrs > first_year], function(yr) {
      full_res <- simulate_one_year(weather, yr, required_tt, earliest_planting_mmdd, latest_planting_mmdd, latest_harvest_mmdd, t_base, t_opt, t_max_cut, tt_mode, crop_type, min_mean_temp_plant, forced_harvest_allowed, min_fraction_tt_for_forced_harvest, winter_dormancy_temp, vernalization_required, vernalization_temp_min, vernalization_temp_max, vernalization_days_required, spring_regrowth_temp, winter_plant_temp_min, winter_plant_temp_max, crop_name)
      full_res$planting_date <- as.Date(NA)
      full_res$row_role <- "harvest_year"
      full_res
    }))
    out <- bind_rows(planting_rows, harvest_rows) %>% arrange(year, row_role)
  }
  out
}

default_parameters <- function(crop_type = c("summer", "winter")) {
  crop_type <- match.arg(crop_type)
  if (crop_type == "summer") {
    list(crop_name = "Maize", days_to_maturity = 140, t_base = 8, t_opt = 25, t_max_cut = 35, baseline_planting_mmdd = "04-15", earliest_planting_mmdd = "03-15", latest_planting_mmdd = "05-31", latest_harvest_mmdd = "10-01", min_mean_temp_plant = 8, winter_plant_temp_min = 5, winter_plant_temp_max = 15)
  } else {
    list(crop_name = "Winter wheat", days_to_maturity = 300, t_base = 0, t_opt = 18, t_max_cut = 30, baseline_planting_mmdd = "10-01", earliest_planting_mmdd = "09-15", latest_planting_mmdd = "11-15", latest_harvest_mmdd = "08-01", min_mean_temp_plant = 8, winter_plant_temp_min = 5, winter_plant_temp_max = 15)
  }
}

# ============================================================
# APP PLOTTING & SUMMARY FUNCTIONS
# ============================================================

build_events_plot_data <- function(sim_results) {
  if (nrow(sim_results) == 0) return(list(events_plot = NULL, bx = NULL, crop_lab = NULL))
  x <- sim_results
  if (!"dataset" %in% names(x)) x$dataset <- "Historical"
  if (!"scenario" %in% names(x)) x$scenario <- NA_character_
  if (!"model" %in% names(x)) x$model <- NA_character_
  
  x <- x %>% mutate(Scenario = case_when(dataset == "Historical" ~ "Historical", !is.na(scenario) & !is.na(model) ~ paste0(scenario, " | ", model), !is.na(scenario) ~ scenario, !is.na(model) ~ model, TRUE ~ dataset))
  events <- bind_rows(x %>% filter(!is.na(planting_date)) %>% transmute(crop = crop_name, Scenario, operation = factor("PLANT", levels = c("PLANT", "HARV/KILL")), date = planting_date, doy = yday(planting_date)), x %>% filter(!is.na(harvest_date)) %>% transmute(crop = crop_name, Scenario, operation = factor("HARV/KILL", levels = c("PLANT", "HARV/KILL")), date = harvest_date, doy = yday(harvest_date)))
  if (nrow(events) == 0) return(list(events_plot = NULL, bx = NULL, crop_lab = NULL))
  
  events <- events %>% mutate(crop = factor(crop, levels = unique(crop)), Scenario = factor(Scenario, levels = unique(Scenario)), operation = factor(operation, levels = c("PLANT", "HARV/KILL")))
  events_clean <- events %>% group_by(crop, Scenario, operation) %>% mutate(q1 = quantile(doy, 0.25, na.rm = TRUE), q3 = quantile(doy, 0.75, na.rm = TRUE), iqr = q3 - q1, lower = q1 - 1.5 * iqr, upper = q3 + 1.5 * iqr) %>% ungroup() %>% filter(doy >= lower, doy <= upper)
  events_plot <- events_clean %>% mutate(doy_plot = doy, Scenario = droplevels(Scenario), Scenario_y = as.numeric(Scenario), op_off = if_else(operation == "PLANT", 0.18, -0.18), y_ridge = Scenario_y + op_off)
  bx <- events_plot %>% group_by(crop, Scenario, Scenario_y, operation, op_off) %>% summarise(q1 = quantile(doy_plot, 0.25, na.rm = TRUE), q2 = quantile(doy_plot, 0.50, na.rm = TRUE), q3 = quantile(doy_plot, 0.75, na.rm = TRUE), iqr = q3 - q1, low = min(doy_plot[doy_plot >= (q1 - 1.5 * iqr)], na.rm = TRUE), up = max(doy_plot[doy_plot <= (q3 + 1.5 * iqr)], na.rm = TRUE), .groups = "drop") %>% mutate(y_box = Scenario_y + op_off - 0.12)
  crop_lab <- setNames(as.character(unique(events_plot$crop)), unique(events_plot$crop))
  list(events_plot = events_plot, bx = bx, crop_lab = crop_lab)
}

plot_growing_season_ridges <- function(sim_results) {
  obj <- build_events_plot_data(sim_results)
  if (is.null(obj$events_plot) || nrow(obj$events_plot) == 0) return(ggplot() + theme_void() + annotate("text", x = 0, y = 0, label = "No valid event dates to plot."))
  
  event_cols <- brewer.pal(3, "Pastel1")[1:2]
  names(event_cols) <- c("PLANT", "HARV/KILL")
  month_dates <- ymd(paste0("2001-", sprintf("%02d", 1:12), "-01"))
  
  ggplot(obj$events_plot, aes(x = doy_plot, y = y_ridge, fill = operation, group = interaction(Scenario, operation, crop))) +
    ggridges::geom_density_ridges(alpha = 0.88, scale = 1.12, rel_min_height = 0.01, linewidth = 0.35, colour = "grey35", bandwidth = 4) +
    geom_segment(data = obj$bx, aes(x = low, xend = q1, y = y_box, yend = y_box), inherit.aes = FALSE, linewidth = 0.35, colour = "grey35") +
    geom_segment(data = obj$bx, aes(x = q3, xend = up, y = y_box, yend = y_box), inherit.aes = FALSE, linewidth = 0.35, colour = "grey35") +
    geom_segment(data = obj$bx, aes(x = low, xend = low, y = y_box - 0.03, yend = y_box + 0.03), inherit.aes = FALSE, linewidth = 0.35, colour = "grey35") +
    geom_segment(data = obj$bx, aes(x = up, xend = up, y = y_box - 0.03, yend = y_box + 0.03), inherit.aes = FALSE, linewidth = 0.35, colour = "grey35") +
    geom_rect(data = obj$bx, aes(xmin = q1, xmax = q3, ymin = y_box - 0.055, ymax = y_box + 0.055, fill = operation), inherit.aes = FALSE, alpha = 0.95, colour = "grey40", linewidth = 0.35) +
    geom_segment(data = obj$bx, aes(x = q2, xend = q2, y = y_box - 0.055, yend = y_box + 0.055), inherit.aes = FALSE, linewidth = 0.45, colour = "grey20") +
    facet_wrap(~ crop, ncol = 1, labeller = labeller(crop = obj$crop_lab)) +
    scale_x_continuous("Calendar month", breaks = yday(month_dates), labels = month(month_dates, label = TRUE, abbr = TRUE), limits = c(1, 365), expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_continuous("Dataset / scenario", breaks = sort(unique(obj$events_plot$Scenario_y)), labels = levels(obj$events_plot$Scenario), expand = expansion(mult = c(0.10, 0.12))) +
    scale_fill_manual(values = event_cols, breaks = c("PLANT", "HARV/KILL"), labels = c("Planting", "Harvest"), name = "Field operation") +
    labs(title = "Growing Season Timing", subtitle = "Distribution of planting and harvest dates") +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), strip.background = element_rect(fill = "grey95", colour = NA), strip.text = element_text(face = "bold"), legend.position = "top", plot.title = element_text(face = "bold"))
}

compare_summary <- function(combined_results) {
  x <- combined_results
  if (nrow(x) == 0) return(data.frame())
  if (!"dataset" %in% names(x)) x$dataset <- "Historical"
  if (!"scenario" %in% names(x)) x$scenario <- NA_character_
  if (!"model" %in% names(x)) x$model <- NA_character_
  
  x %>%
    mutate(group_label = case_when(dataset == "Historical" ~ "Historical", !is.na(scenario) & !is.na(model) ~ paste0(scenario, " | ", model), !is.na(scenario) ~ scenario, !is.na(model) ~ model, TRUE ~ dataset)) %>%
    filter(row_role == "harvest_year") %>% group_by(group_label) %>%
    summarise(n_years = n(), mature_pct = round(mean(status == "mature", na.rm = TRUE) * 100, 1), forced_harvest_pct = round(mean(status == "forced_harvest_immature", na.rm = TRUE) * 100, 1), failed_pct = round(mean(status %in% c("failed_to_mature", "insufficient_vernalization"), na.rm = TRUE) * 100, 1), median_harvest_doy = round(median(yday(harvest_date), na.rm = TRUE), 1), median_maturity_doy = round(median(yday(maturity_date), na.rm = TRUE), 1), mean_maturity_fraction = round(mean(maturity_fraction, na.rm = TRUE), 3), median_season_length = round(median(season_length_days, na.rm = TRUE), 1), .groups = "drop")
}

prepare_temperature_plot_data <- function(hist_weather, climate_weather = NULL) {
  hw <- hist_weather %>% mutate(dataset = "Historical", group_label = "Historical", month = month(date, label = TRUE, abbr = TRUE), month_num = month(date))
  if (is.null(climate_weather)) return(hw)
  cw <- climate_weather
  if (!"scenario" %in% names(cw)) cw$scenario <- NA_character_
  if (!"model" %in% names(cw)) cw$model <- NA_character_
  cw <- cw %>% mutate(dataset = "Climate", group_label = case_when(!is.na(scenario) & !is.na(model) ~ paste0(scenario, " | ", model), !is.na(scenario) ~ scenario, !is.na(model) ~ model, TRUE ~ "Climate"), month = month(date, label = TRUE, abbr = TRUE), month_num = month(date))
  bind_rows(hw, cw)
}

plot_monthly_temperature_cycle <- function(hist_weather, climate_weather = NULL) {
  df <- prepare_temperature_plot_data(hist_weather, climate_weather)
  monthly <- df %>% group_by(group_label, dataset, month, month_num) %>% summarise(mean_tmean = mean(tmean, na.rm = TRUE), q10 = quantile(tmean, 0.10, na.rm = TRUE), q90 = quantile(tmean, 0.90, na.rm = TRUE), .groups = "drop")
  
  ggplot(monthly, aes(x = month_num, y = mean_tmean, group = group_label, color = group_label, fill = group_label)) +
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.12, linewidth = 0, show.legend = FALSE) +
    geom_line(linewidth = 0.95) + geom_point(size = 1.8) + scale_x_continuous(breaks = 1:12, labels = month.abb) +
    labs(title = "Seasonal Temperature Cycle", y = "Mean Daily Temp (°C)", x = NULL, color = "Scenario", fill = "Scenario") +
    theme_minimal(base_size = 14) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
}

plot_annual_temperature_boxplot <- function(hist_weather, climate_weather = NULL) {
  df <- prepare_temperature_plot_data(hist_weather, climate_weather)
  annual <- df %>% group_by(group_label, year) %>% summarise(annual_mean_tmean = mean(tmean, na.rm = TRUE), .groups = "drop")
  
  ggplot(annual, aes(x = reorder(group_label, annual_mean_tmean, FUN = median), y = annual_mean_tmean, fill = group_label)) +
    geom_boxplot(alpha = 0.85, outlier.alpha = 0.6) + coord_flip() +
    labs(title = "Annual Temperature Distribution", subtitle = "Spread of yearly mean temperatures", x = NULL, y = "Annual Mean Temp (°C)", fill = "Scenario") +
    theme_minimal(base_size = 14) + theme(legend.position = "none", panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
}

# ============================================================
# UI DEFINITION (bslib)
# ============================================================

  ui <- page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
    
    # REPLACE THE OLD TITLE WITH THIS:
    title = tags$span(
      tags$img(src = "logo.png", height = "100px", style = "margin-right: 15px; margin-bottom: 5px;"),
      "ThermoPheno: Climate & Phenology"
    ),
    
    fillable = FALSE, 

  # 1. ADD THE JAVASCRIPT FOR FULLSCREEN
  tags$head(
    tags$script(HTML("
      function toggleFullScreen() {
        if (!document.fullscreenElement) {
          document.documentElement.requestFullscreen();
        } else {
          if (document.exitFullscreen) {
            document.exitFullscreen();
          }
        }
      }
    "))
  ),
  
  # 2. ADD THE BUTTON TO THE HEADER
  header = div(
    style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
    actionButton("btn_fullscreen", "Fullscreen", icon = icon("expand"), 
                 onclick = "toggleFullScreen();", class = "btn-sm btn-outline-secondary")
  ),
  
  sidebar = sidebar(
    width = 350,
    accordion(
      open = "Data & Scenarios",
      accordion_panel("Data & Scenarios", icon = icon("cloud-sun"),
                      fileInput("weather_file", "1. Historical Weather (CSV)", accept = ".csv"),
                      hr(), p(tags$b("2. Explore Climate Change")),
                      checkboxInput("use_synthetic_cc", "Simulate Synthetic Scenario", value = FALSE),
                      conditionalPanel(condition = "input.use_synthetic_cc == true",
                                       sliderInput("synthetic_temp_delta", "Temperature Increase (°C)", min = 0, max = 5, value = 2.0, step = 0.5)),
                      hr(),
                      fileInput("climate_file", "Or Upload Climate Projections (CSV)", accept = ".csv"),
                      tags$small("Required columns: date, tmin, tmax.", class = "text-muted")
      ),
      accordion_panel("Crop Settings", icon = icon("leaf"),
                      textInput("crop_name", "Crop name", value = "Maize"),
                      selectInput("crop_type", "Crop type", choices = c("summer", "winter"), selected = "summer"),
                      numericInput("days_to_maturity", "Reference days to maturity", value = 140, min = 1),
                      textInput("baseline_planting_mmdd", "Reference planting date (MM-DD)", value = "04-15"),
                      textInput("earliest_planting_mmdd", "Earliest planting date (MM-DD)", value = "03-15"),
                      textInput("latest_planting_mmdd", "Latest planting date (MM-DD)", value = "05-31"),
                      textInput("latest_harvest_mmdd", "Latest harvest date (MM-DD)", value = "10-01")
      ),
      accordion_panel("Thermal Parameters", icon = icon("temperature-half"),
                      selectInput("tt_mode", "Thermal time method", choices = c("simple", "capped", "triangular"), selected = "triangular"),
                      numericInput("t_base", "Base temperature (°C)", value = 8),
                      numericInput("t_opt", "Optimum temperature (°C)", value = 25),
                      numericInput("t_max_cut", "Upper cutoff (°C)", value = 35)
      ),
      accordion_panel("Advanced & Winter Rules", icon = icon("sliders"),
                      numericInput("min_mean_temp_plant", "Summer planting threshold (°C)", value = 8),
                      numericInput("winter_plant_temp_min", "Winter planting min (°C)", value = 5),
                      numericInput("winter_plant_temp_max", "Winter planting max (°C)", value = 15),
                      checkboxInput("forced_harvest_allowed", "Allow forced harvest", value = TRUE),
                      numericInput("min_fraction_tt_for_forced_harvest", "Min fraction TT for forced harvest", value = 0.8, min = 0, max = 1, step = 0.05),
                      conditionalPanel(condition = "input.crop_type == 'winter'", hr(), h6("Winter Logic"),
                                       checkboxInput("vernalization_required", "Require vernalization", value = TRUE),
                                       numericInput("vernalization_days_required", "Vernalization days", value = 45),
                                       numericInput("winter_dormancy_temp", "Dormancy temp (°C)", value = 0),
                                       numericInput("spring_regrowth_temp", "Regrowth temp (°C)", value = 5))
      )
    ),
    br(), actionButton("run_model", "Run Simulation", class = "btn-primary btn-lg w-100", icon = icon("play")), br(),
    card(class = "bg-light mt-3", card_header("Export Data"),
         downloadButton("dl_summary_csv", "Summary (CSV)", class = "btn-sm btn-outline-secondary mb-2"),
         downloadButton("dl_results_csv", "Full Results (CSV)", class = "btn-sm btn-outline-secondary"))
  ),
  
  navset_card_underline(
    nav_panel("Dashboard", icon = icon("chart-line"),
              layout_columns(col_widths = c(12, 12),
                             card(card_header(class = "bg-primary text-white", "Simulation Summary"), tableOutput("comparison_summary_table")),
                             card(card_header("Run Parameters & Definitions"), tableOutput("run_parameters_table"))
              )
    ),
    nav_panel("Plots", icon = icon("chart-bar"),
              # Cards are placed sequentially (no layout_columns wrapper) to allow vertical stacking
              card(full_screen = TRUE, card_header("Growing Season Timing", downloadButton("dl_plot_ridge", "Save PNG", class = "btn-sm float-end")), plotOutput("plot_ridge_boxinline", height = "900px")),
              card(full_screen = TRUE, card_header("Temperature Cycle Comparison", downloadButton("dl_plot_monthly_temp", "Save PNG", class = "btn-sm float-end")), plotOutput("plot_monthly_temperature_cycle", height = "600px")),
              card(full_screen = TRUE, card_header("Annual Temperature Variability", downloadButton("dl_plot_annual_temp", "Save PNG", class = "btn-sm float-end")), plotOutput("plot_annual_temperature_boxplot", height = "700px"))
    ),
    nav_panel("Data Viewer", icon = icon("table"), 
              card(
                full_screen = TRUE, 
                min_height = "85vh", # <--- Forces the card to fill 85% of your screen height
                card_header("Year-by-Year Results"),
                div(style = "padding: 10px 15px; background-color: #f8f9fa; border-bottom: 1px solid #dee2e6; margin-bottom: 0;", 
                    radioButtons("table_filter", "Select Data to View:", 
                                 choices = c("All Data" = "All", "Historical Data Only" = "Historical", "Climate Change Data Only" = "Climate"), 
                                 inline = TRUE)
                ),
                DTOutput("results_table")
              )
    ),
    nav_panel("Calibration Details", icon = icon("calculator"), card(card_header("Baseline Reference Period"), verbatimTextOutput("baseline_years_text")), card(card_header("Estimated Thermal Time Requirement"), verbatimTextOutput("required_tt_text")))
  )
)

# ============================================================
# SERVER LOGIC
# ============================================================

server <- function(input, output, session) {
  
  observeEvent(input$crop_type, {
    p <- default_parameters(input$crop_type)
    updateTextInput(session, "crop_name", value = p$crop_name)
    updateNumericInput(session, "days_to_maturity", value = p$days_to_maturity)
    updateNumericInput(session, "t_base", value = p$t_base)
    updateNumericInput(session, "t_opt", value = p$t_opt)
    updateNumericInput(session, "t_max_cut", value = p$t_max_cut)
    updateTextInput(session, "baseline_planting_mmdd", value = p$baseline_planting_mmdd)
    updateTextInput(session, "earliest_planting_mmdd", value = p$earliest_planting_mmdd)
    updateTextInput(session, "latest_planting_mmdd", value = p$latest_planting_mmdd)
    updateTextInput(session, "latest_harvest_mmdd", value = p$latest_harvest_mmdd)
  }, ignoreInit = TRUE)
  
  weather_data <- reactive({
    req(input$weather_file)
    raw <- read_csv(input$weather_file$datapath, show_col_types = FALSE)
    checks <- validate_weather_dwd(raw, strict = FALSE)
    if (length(checks$warnings) > 0) showNotification(paste(checks$warnings, collapse = "\n"), type = "warning", duration = 8)
    prepare_weather(raw)
  })
  
  climate_data_combined <- reactive({
    res <- list()
    if (isTruthy(input$climate_file)) {
      raw_climate <- read_csv(input$climate_file$datapath, show_col_types = FALSE)
      checks <- validate_weather_dwd(raw_climate, strict = FALSE)
      if (length(checks$warnings) > 0) showNotification(paste(checks$warnings, collapse = "\n"), type = "warning", duration = 8)
      res[[1]] <- prepare_weather(raw_climate)
    }
    if (isTruthy(input$use_synthetic_cc) && isTruthy(input$weather_file)) {
      syn <- weather_data()
      syn$tmin <- syn$tmin + input$synthetic_temp_delta
      syn$tmax <- syn$tmax + input$synthetic_temp_delta
      syn$tmean <- (syn$tmin + syn$tmax) / 2
      syn$scenario <- paste0("Synthetic +", input$synthetic_temp_delta, "\u00B0C")
      syn$model <- "User Baseline"
      res[[2]] <- syn
    }
    if (length(res) > 0) return(bind_rows(res))
    return(NULL)
  })
  
  baseline_years_auto <- reactive({
    req(weather_data())
    seq(min(weather_data()$year), max(weather_data()$year))
  })
  
  winter_params <- reactive({
    c_type <- input$crop_type
    list(
      wdt   = if (c_type == "winter" && length(input$winter_dormancy_temp) > 0) input$winter_dormancy_temp else 0,
      vreq  = if (c_type == "winter" && length(input$vernalization_required) > 0) input$vernalization_required else FALSE,
      vdays = if (c_type == "winter" && length(input$vernalization_days_required) > 0) input$vernalization_days_required else 0,
      srt   = if (c_type == "winter" && length(input$spring_regrowth_temp) > 0) input$spring_regrowth_temp else 5
    )
  })
  
  output$baseline_years_text <- renderPrint({ req(weather_data()); yrs <- baseline_years_auto(); cat("Baseline years automatically detected:", min(yrs), "-", max(yrs), "\n") })
  
  required_tt_result <- eventReactive(input$run_model, {
    req(input$run_model > 0)
    wp <- winter_params()
    estimate_required_tt(
      weather = weather_data(), baseline_years = baseline_years_auto(),
      planting_mmdd = input$baseline_planting_mmdd, days_to_maturity = input$days_to_maturity,
      t_base = input$t_base, t_opt = input$t_opt, t_max_cut = input$t_max_cut, tt_mode = input$tt_mode,
      crop_type = input$crop_type, winter_dormancy_temp = wp$wdt,
      vernalization_required = wp$vreq, vernalization_temp_min = 0, vernalization_temp_max = 10,
      vernalization_days_required = wp$vdays, spring_regrowth_temp = wp$srt
    )
  })
  
  historical_results <- eventReactive(input$run_model, {
    req(input$run_model > 0)
    wp <- winter_params()
    run_simulation(
      weather = weather_data(), crop_name = input$crop_name, required_tt = required_tt_result()$required_tt,
      earliest_planting_mmdd = input$earliest_planting_mmdd, latest_planting_mmdd = input$latest_planting_mmdd, latest_harvest_mmdd = input$latest_harvest_mmdd,
      t_base = input$t_base, t_opt = input$t_opt, t_max_cut = input$t_max_cut, tt_mode = input$tt_mode, crop_type = input$crop_type, 
      min_mean_temp_plant = input$min_mean_temp_plant, forced_harvest_allowed = input$forced_harvest_allowed, min_fraction_tt_for_forced_harvest = input$min_fraction_tt_for_forced_harvest,
      winter_dormancy_temp = wp$wdt, vernalization_required = wp$vreq, vernalization_temp_min = 0, vernalization_temp_max = 10,
      vernalization_days_required = wp$vdays, spring_regrowth_temp = wp$srt, winter_plant_temp_min = input$winter_plant_temp_min, winter_plant_temp_max = input$winter_plant_temp_max
    ) %>% mutate(dataset = "Historical", scenario = NA_character_, model = NA_character_)
  })
  
  future_results <- eventReactive(input$run_model, {
    req(input$run_model > 0)
    cd <- climate_data_combined()
    if (is.null(cd)) return(NULL)
    wp <- winter_params()
    grouping_cols <- intersect(c("scenario", "model", "period", "station"), names(cd))
    
    if (length(grouping_cols) == 0) {
      run_simulation(
        weather = cd, crop_name = input$crop_name, required_tt = required_tt_result()$required_tt,
        earliest_planting_mmdd = input$earliest_planting_mmdd, latest_planting_mmdd = input$latest_planting_mmdd, latest_harvest_mmdd = input$latest_harvest_mmdd,
        t_base = input$t_base, t_opt = input$t_opt, t_max_cut = input$t_max_cut, tt_mode = input$tt_mode, crop_type = input$crop_type,
        min_mean_temp_plant = input$min_mean_temp_plant, forced_harvest_allowed = input$forced_harvest_allowed, min_fraction_tt_for_forced_harvest = input$min_fraction_tt_for_forced_harvest, 
        winter_dormancy_temp = wp$wdt, vernalization_required = wp$vreq, vernalization_temp_min = 0, vernalization_temp_max = 10, 
        vernalization_days_required = wp$vdays, spring_regrowth_temp = wp$srt, winter_plant_temp_min = input$winter_plant_temp_min, winter_plant_temp_max = input$winter_plant_temp_max
      ) %>% mutate(dataset = "Climate", scenario = NA_character_, model = NA_character_)
    } else {
      cd %>% group_by(across(all_of(grouping_cols))) %>% group_split() %>% lapply(function(df_group) {
        meta <- df_group[1, grouping_cols, drop = FALSE]
        res <- run_simulation(
          weather = df_group, crop_name = input$crop_name, required_tt = required_tt_result()$required_tt,
          earliest_planting_mmdd = input$earliest_planting_mmdd, latest_planting_mmdd = input$latest_planting_mmdd, latest_harvest_mmdd = input$latest_harvest_mmdd,
          t_base = input$t_base, t_opt = input$t_opt, t_max_cut = input$t_max_cut, tt_mode = input$tt_mode, crop_type = input$crop_type,
          min_mean_temp_plant = input$min_mean_temp_plant, forced_harvest_allowed = input$forced_harvest_allowed, min_fraction_tt_for_forced_harvest = input$min_fraction_tt_for_forced_harvest, 
          winter_dormancy_temp = wp$wdt, vernalization_required = wp$vreq, vernalization_temp_min = 0, vernalization_temp_max = 10, 
          vernalization_days_required = wp$vdays, spring_regrowth_temp = wp$srt, winter_plant_temp_min = input$winter_plant_temp_min, winter_plant_temp_max = input$winter_plant_temp_max
        )
        bind_cols(res, meta) %>% mutate(dataset = "Climate")
      }) %>% bind_rows()
    }
  })
  
  combined_results <- reactive({
    req(historical_results())
    fut <- future_results()
    if (!is.null(fut)) bind_rows(historical_results(), fut) else historical_results()
  })
  
  output$required_tt_text <- renderPrint({
    req(required_tt_result())
    cat("Crop:", input$crop_name, "| Type:", input$crop_type, "\n")
    cat("Estimated Required Thermal Time:", round(required_tt_result()$required_tt, 2), "°C-days\n\n")
    print(head(required_tt_result()$yearly_required_tt, 10))
    cat("... (showing first 10 years)")
  })
  
  output$run_parameters_table <- renderTable({
    yrs <- "Not detected yet"
    if(isTruthy(input$weather_file)) {
      w <- weather_data()
      yrs <- paste0(min(w$year), " - ", max(w$year))
    }
    
    data.frame(
      Parameter = c("Crop name", "Crop type", "Days to maturity", "Thermal time method",
                    "Base temperature (°C)", "Optimum temperature (°C)", "Upper cutoff (°C)", 
                    "Baseline reference years", "Reference planting date", 
                    "Earliest planting date", "Latest planting date", "Latest harvest date"),
      Value = c(input$crop_name, input$crop_type, as.character(input$days_to_maturity), input$tt_mode,
                as.character(input$t_base), as.character(input$t_opt), as.character(input$t_max_cut), 
                yrs, input$baseline_planting_mmdd, 
                input$earliest_planting_mmdd, input$latest_planting_mmdd, input$latest_harvest_mmdd),
      Definition = c(
        "The crop being simulated.",
        "Indicates whether the crop is treated as a summer crop or a winter crop.",
        "Reference number of calendar days used to estimate the crop's required thermal time.",
        "Mathematical rule used to convert daily temperature into daily crop development.",
        "Lower threshold for thermal development.",
        "Temperature at which thermal development is assumed to be most effective.",
        "Temperature above which thermal development no longer increases effectively.",
        "Historical period used to estimate the crop's required thermal time.",
        "Planting date used in the baseline historical period to estimate required thermal time.",
        "The earliest calendar date planting can occur in simulations.",
        "The latest calendar date planting can occur in simulations.",
        "The absolute latest calendar date harvest can occur in simulations."
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$comparison_summary_table <- renderTable({ req(combined_results()); compare_summary(combined_results()) })
  output$results_table <- renderDT({ 
    req(combined_results())
    df <- combined_results()
    
    # Filter the data based on the radio buttons
    if (input$table_filter == "Historical") {
      df <- df %>% filter(dataset == "Historical")
    } else if (input$table_filter == "Climate") {
      df <- df %>% filter(dataset == "Climate")
    }
    
    # Prettify column names (e.g., "maturity_fraction" becomes "Maturity Fraction")
    display_names <- tools::toTitleCase(gsub("_", " ", names(df)))
    
    datatable(
      df, 
      colnames = display_names,
      options = list(
        pageLength = 50,      
        scrollX = TRUE,
        # Responsive height: fills the screen minus the headers/menus above it
        scrollY = "calc(100vh - 350px)", 
        scrollCollapse = TRUE,
        fixedHeader = TRUE,   
        dom = 'Bfrtip'
      ), 
      rownames = FALSE,
      # Added 'nowrap' here to stop text wrapping and FORCE the horizontal scrollbar!
      class = 'cell-border stripe hover compact nowrap' 
    ) %>%
      formatRound(columns = c("accumulated_tt", "required_tt", "vernalization_days"), digits = 1) %>%
      formatRound(columns = "maturity_fraction", digits = 3) %>%
      formatStyle(
        "status",
        color = styleEqual(
          c("mature", "planted_for_next_year", "forced_harvest_immature", 
            "failed_to_mature", "failed_after_planting", "insufficient_vernalization", "not_planted"),
          c("#198754", "#0dcaf0", "#fd7e14", 
            "#dc3545", "#dc3545", "#dc3545", "#6c757d")
        ),
        fontWeight = "bold"
      )
  })
  
  ridge_plot_obj <- reactive({ req(combined_results()); plot_growing_season_ridges(combined_results()) })
  monthly_temp_plot_obj <- reactive({ req(weather_data()); plot_monthly_temperature_cycle(weather_data(), climate_data_combined()) })
  annual_temp_plot_obj <- reactive({ req(weather_data()); plot_annual_temperature_boxplot(weather_data(), climate_data_combined()) })
  
  output$plot_ridge_boxinline <- renderPlot({ ridge_plot_obj() })
  output$plot_monthly_temperature_cycle <- renderPlot({ monthly_temp_plot_obj() })
  output$plot_annual_temperature_boxplot <- renderPlot({ annual_temp_plot_obj() })
  
  output$dl_results_csv <- downloadHandler(filename = function() { paste0("pheno_results_", Sys.Date(), ".csv") }, content = function(file) { write.csv(combined_results(), file, row.names = FALSE) })
  output$dl_summary_csv <- downloadHandler(filename = function() { paste0("pheno_summary_", Sys.Date(), ".csv") }, content = function(file) { write.csv(compare_summary(combined_results()), file, row.names = FALSE) })
  output$dl_plot_ridge <- downloadHandler(filename = function() { "ridge_plot.png" }, content = function(file) { ggsave(file, plot = ridge_plot_obj(), width = 10, height = 7, bg = "white") })
  output$dl_plot_monthly_temp <- downloadHandler(filename = function() { "monthly_temp.png" }, content = function(file) { ggsave(file, plot = monthly_temp_plot_obj(), width = 10, height = 6, bg = "white") })
  output$dl_plot_annual_temp <- downloadHandler(filename = function() { "annual_temp.png" }, content = function(file) { ggsave(file, plot = annual_temp_plot_obj(), width = 10, height = 6, bg = "white") })
}

shinyApp(ui, server)
