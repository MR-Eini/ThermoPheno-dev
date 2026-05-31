\name{estimate_required_tt_from_observed}
\alias{estimate_required_tt_from_observed}
\title{Estimate required thermal time from observed dates}
\usage{
estimate_required_tt_from_observed(weather, observed_calendar, calibration_years,
  planting_col = "observed_planting_date", harvest_col = "observed_harvest_date",
  year_col = "crop_year", t_base, t_opt = NA_real_, t_max_cut = NA_real_,
  tt_mode = "simple", summary_fun = c("median", "mean"),
  min_weather_coverage = 0.95)
}
\arguments{
  \item{weather}{Prepared weather data.}
  \item{observed_calendar}{Observed planting-harvest calendar.}
  \item{calibration_years}{Years used for calibration.}
  \item{planting_col}{Observed planting-date column.}
  \item{harvest_col}{Observed harvest-date column.}
  \item{year_col}{Crop-year column.}
  \item{t_base}{Base temperature.}
  \item{t_opt}{Optimum temperature.}
  \item{t_max_cut}{Upper cutoff temperature.}
  \item{tt_mode}{Thermal-time method.}
  \item{summary_fun}{Summary method.}
  \item{min_weather_coverage}{Minimum weather-data coverage.}
}
\value{A list containing yearly observed TT values and the estimated required TT.}
\description{Estimate required thermal time from observed dates}
