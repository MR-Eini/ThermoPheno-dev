\name{estimate_required_tt}
\alias{estimate_required_tt}
\title{Estimate required thermal time from a reference baseline period}
\usage{
estimate_required_tt(weather, baseline_years, planting_mmdd, days_to_maturity,
  t_base, t_opt = NA_real_, t_max_cut = NA_real_, tt_mode = "simple",
  crop_type = c("summer", "winter"), winter_dormancy_temp = 0,
  vernalization_required = FALSE, vernalization_temp_min = 0,
  vernalization_temp_max = 10, vernalization_days_required = 30,
  spring_regrowth_temp = 5)
}
\arguments{
  \item{weather}{Prepared weather data.}
  \item{baseline_years}{Integer vector of baseline years.}
  \item{planting_mmdd}{Reference planting date in MM-DD format.}
  \item{days_to_maturity}{Reference days from planting to maturity.}
  \item{t_base}{Base temperature.}
  \item{t_opt}{Optimum temperature.}
  \item{t_max_cut}{Upper cutoff temperature.}
  \item{tt_mode}{Thermal-time method.}
  \item{crop_type}{Crop type: summer or winter.}
  \item{winter_dormancy_temp}{Dormancy threshold for winter crops.}
  \item{vernalization_required}{Whether vernalization is required.}
  \item{vernalization_temp_min}{Minimum vernalization temperature.}
  \item{vernalization_temp_max}{Maximum vernalization temperature.}
  \item{vernalization_days_required}{Required number of vernalization days.}
  \item{spring_regrowth_temp}{Spring regrowth threshold.}
}
\value{A list containing yearly thermal-time values and the mean required thermal time.}
\description{Estimate required thermal time from a reference baseline period}
