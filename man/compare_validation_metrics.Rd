\name{compare_validation_metrics}
\alias{compare_validation_metrics}
\title{Compare observed and simulated phenological dates}
\usage{
compare_validation_metrics(df, observed_col = "observed_date",
  simulated_col = "simulated_date")
}
\arguments{
  \item{df}{Data frame containing observed and simulated date columns.}
  \item{observed_col}{Name of observed date column.}
  \item{simulated_col}{Name of simulated date column.}
}
\value{A one-row data frame with validation metrics.}
\description{Compare observed and simulated phenological dates}
