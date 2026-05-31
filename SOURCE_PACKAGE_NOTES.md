# ThermoPheno source package notes

This ZIP was assembled as a corrected source-package distribution of ThermoPheno 0.1.0.

The package includes:

- R source functions in `R/`
- Shiny application in `inst/app/`
- synthetic example weather and scenario CSV files in `inst/extdata/`
- unit tests in `tests/testthat/`
- manual documentation in `man/`
- GitHub Actions workflow templates in `.github/workflows/`

Because the build environment used to assemble this ZIP has no R installation, the package was not checked with `R CMD check` in this environment. Run `RUN_CHECKS.R` locally after extracting the ZIP.
