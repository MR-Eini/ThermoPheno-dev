# Run from the package root
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("rcmdcheck", quietly = TRUE)) install.packages("rcmdcheck")

devtools::load_all()
devtools::document()
devtools::test()
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))
