\name{validate_weather_dwd}
\alias{validate_weather_dwd}
\title{Validate weather input against DWD-like conventions}
\usage{
validate_weather_dwd(df, strict = FALSE)
}
\arguments{
  \item{df}{Data frame containing at least date, tmin, and tmax.}
  \item{strict}{If TRUE, stop on validation errors.}
}
\value{A list with ok, errors, and warnings.}
\description{Validate weather input against DWD-like conventions}
