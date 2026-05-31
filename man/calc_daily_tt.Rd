\name{calc_daily_tt}
\alias{calc_daily_tt}
\title{Calculate daily thermal time}
\usage{
calc_daily_tt(tmin, tmax, t_base, t_opt = NA_real_, t_max_cut = NA_real_,
  mode = c("simple", "capped", "triangular"))
}
\arguments{
  \item{tmin}{Daily minimum temperature.}
  \item{tmax}{Daily maximum temperature.}
  \item{t_base}{Base temperature.}
  \item{t_opt}{Optimum temperature.}
  \item{t_max_cut}{Upper cutoff temperature.}
  \item{mode}{Thermal-time method.}
}
\value{Numeric vector of daily thermal-time values.}
\description{Calculate daily thermal time}
