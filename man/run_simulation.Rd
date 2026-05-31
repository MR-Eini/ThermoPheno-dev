\name{run_simulation}
\alias{run_simulation}
\title{Run ThermoPheno simulations across years}
\usage{
run_simulation(weather, crop_name, required_tt, earliest_planting_mmdd,
  latest_planting_mmdd, latest_harvest_mmdd, t_base, t_opt = NA_real_,
  t_max_cut = NA_real_, tt_mode = "simple", crop_type = c("summer", "winter"),
  min_mean_temp_plant = 0, forced_harvest_allowed = TRUE,
  min_fraction_tt_for_forced_harvest = 0, winter_dormancy_temp = 0,
  vernalization_required = FALSE, vernalization_temp_min = 0,
  vernalization_temp_max = 10, vernalization_days_required = 30,
  spring_regrowth_temp = 5, winter_plant_temp_min = 5,
  winter_plant_temp_max = 15)
}
\arguments{
  \item{weather}{Prepared weather data.}
  \item{crop_name}{Crop label.}
  \item{required_tt}{Required thermal time.}
  \item{earliest_planting_mmdd}{Earliest planting date.}
  \item{latest_planting_mmdd}{Latest planting date.}
  \item{latest_harvest_mmdd}{Latest harvest date.}
  \item{t_base}{Base temperature.}
  \item{t_opt}{Optimum temperature.}
  \item{t_max_cut}{Upper cutoff temperature.}
  \item{tt_mode}{Thermal-time method.}
  \item{crop_type}{Crop type.}
  \item{min_mean_temp_plant}{Summer planting threshold.}
  \item{forced_harvest_allowed}{Whether forced harvest is allowed.}
  \item{min_fraction_tt_for_forced_harvest}{Minimum maturity fraction for forced harvest.}
  \item{winter_dormancy_temp}{Winter dormancy threshold.}
  \item{vernalization_required}{Whether vernalization is required.}
  \item{vernalization_temp_min}{Minimum vernalization temperature.}
  \item{vernalization_temp_max}{Maximum vernalization temperature.}
  \item{vernalization_days_required}{Required vernalization days.}
  \item{spring_regrowth_temp}{Spring regrowth threshold.}
  \item{winter_plant_temp_min}{Minimum winter planting temperature.}
  \item{winter_plant_temp_max}{Maximum winter planting temperature.}
}
\value{A data frame with one row per simulated harvest year.}
\description{Run ThermoPheno simulations across years}
