# ThermoPheno <img src="man/figures/logo.png" align="right" height="110" alt="ThermoPheno logo" />

ThermoPheno is an R package and Shiny application for simulating crop phenology using thermal-time (GDD) rules under historical and climate-scenario temperature series.

## What ThermoPheno does

- Simulates **planting, maturity, and harvest timing** from daily `tmin` / `tmax`.
- Supports **summer and winter crop workflows** (including optional vernalization logic).
- Estimates **required thermal time** from a user baseline period.
- Compares historical and projected climate runs in one interface.

## Package structure

```text
ThermoPheno-dev/
├── R/                        # Core package functions
│   ├── ThermoPheno_functions.R
│   ├── dwd_validation.R      # Input validation module
│   ├── app_launcher.R
│   └── zzz.R
├── inst/
│   ├── app/                  # Shiny application
│   │   └── app.R
│   └── extdata/              # Example data
├── tests/testthat/           # Unit tests
├── man/                      # Rd docs
├── .github/workflows/        # CI and docs pipelines
└── _pkgdown.yml              # pkgdown site config
```

## Installation

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("MR-Eini/ThermoPheno-dev")
```

## Run the app

```r
library(ThermoPheno)
ThermoPheno()
```

Or from the repository:

```r
shiny::runApp("inst/app/app.R")
```

## Input data requirements

At minimum, CSV files must include:

| column | type | example |
|---|---|---|
| `date` | Date (`YYYY-MM-DD`) | `1991-04-15` |
| `tmin` | numeric °C | `4.2` |
| `tmax` | numeric °C | `13.7` |

Optional grouping columns in climate files: `scenario`, `model`, `period`, `station`.

## Development quick start

```r
install.packages(c("devtools", "testthat", "pkgdown"))
devtools::load_all()
devtools::test()
```

## Notes

- The scientific model logic is thermal-time based and intentionally simple.
- Validation checks are included to catch malformed weather input before simulation.
- Example datasets are in `inst/extdata`.

## License

MIT (see `LICENSE`).
