\name{prepare_weather}
\alias{prepare_weather}
\title{Prepare daily weather data for ThermoPheno}
\usage{
prepare_weather(df)
}
\arguments{
  \item{df}{Data frame with at least \code{date}, \code{tmin}, and \code{tmax}.}
}
\value{A data frame sorted by date with derived \code{year}, \code{doy}, and \code{tmean} columns.}
\description{Prepare daily weather data for ThermoPheno}
