\name{find_planting_date}
\alias{find_planting_date}
\title{Find the first planting date within an allowed window}
\usage{
find_planting_date(weather_window, earliest_planting_date, latest_planting_date,
  crop_type = c("summer", "winter"), min_mean_temp_plant = -999,
  winter_plant_temp_min = 5, winter_plant_temp_max = 15)
}
\arguments{
  \item{weather_window}{Weather data covering the planting window.}
  \item{earliest_planting_date}{Earliest allowed planting date.}
  \item{latest_planting_date}{Latest allowed planting date.}
  \item{crop_type}{Crop type.}
  \item{min_mean_temp_plant}{Summer crop planting temperature threshold.}
  \item{winter_plant_temp_min}{Minimum winter planting temperature.}
  \item{winter_plant_temp_max}{Maximum winter planting temperature.}
}
\value{A list with \code{found} and \code{planting_date}.}
\description{Find the first planting date within an allowed window}
