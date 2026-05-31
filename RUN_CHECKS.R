message("Running ThermoPheno package checks...")

if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("rcmdcheck", quietly = TRUE)) install.packages("rcmdcheck")
if (!requireNamespace("roxygen2", quietly = TRUE)) install.packages("roxygen2")
if (!requireNamespace("pkgdown", quietly = TRUE)) install.packages("pkgdown")

roxygen2::roxygenise()
devtools::test()
rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "warning")
pkgdown::build_site()
