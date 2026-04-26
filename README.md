# ThermoPheno <img src="man/figures/logo.png" align="right" height="110" alt="ThermoPheno logo" />

ThermoPheno is an R package and Shiny application for simulating crop phenology using thermal-time (GDD) rules under historical and climate-scenario temperature series.

Website: <https://mr-eini.github.io/ThermoPheno-dev/>

## What ThermoPheno does

- Simulates **planting, maturity, and harvest timing** from daily `tmin` / `tmax`.
- Supports **summer and winter crop workflows** (including optional vernalization logic).
- Estimates **required thermal time** from a user baseline period.
- Compares historical and projected climate runs in one interface.

## Package structure

```text
ThermoPheno-dev/
|-- R/                        # Core package functions
|   |-- ThermoPheno_functions.R
|   |-- dwd_validation.R      # Input validation module
|   |-- app_launcher.R
|   `-- zzz.R
|-- inst/
|   |-- app/                  # Shiny application
|   |   `-- app.R
|   `-- extdata/              # Example data
|-- tests/testthat/           # Unit tests
|-- man/                      # Rd docs
|-- .github/workflows/        # CI and docs pipelines
`-- _pkgdown.yml              # pkgdown site config
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
| `tmin` | numeric C | `4.2` |
| `tmax` | numeric C | `13.7` |

Optional grouping columns in climate files: `scenario`, `model`, `period`, `station`.

## Development quick start

```r
install.packages(c("devtools", "testthat", "pkgdown"))
devtools::load_all()
devtools::test()
```

## Website deployment

The pkgdown website is configured in `_pkgdown.yml` and is published to:

<https://mr-eini.github.io/ThermoPheno-dev/>

Deployment runs through `.github/workflows/pkgdown.yaml` on pushes to `main` or `master`, and can also be started manually with `workflow_dispatch`. The workflow builds the site with `pkgdown::build_site_github_pages()` and deploys the generated `docs/` directory to the `gh-pages` branch.

To preview the site locally:

```r
pkgdown::build_site()
```

## Notes

- The scientific model logic is thermal-time based and intentionally simple.
- Validation checks are included to catch malformed weather input before simulation.
- Example datasets are in `inst/extdata`.

## License

MIT (see `LICENSE`).

## DWD Open Data integration (rdwd)

ThermoPheno includes helper functions for pulling DWD data via `rdwd` with a **user-managed cache directory**.

```r
# install.packages("rdwd")
cache_dir <- "~/thermopheno_dwd_cache"   # outside repository

# Daily station temperature (DWD climate daily 'kl')
temp <- ThermoPheno:::get_dwd_daily_temperature(
  station_id = "00386",
  start_year = 2018,
  end_year = 2020,
  cache_dir = cache_dir,
  period = "historical"
)

# Crop phenology observations (DWD phenology open data)
pheno <- ThermoPheno:::get_dwd_crop_phenology(
  crop_pattern = "hafer|oat",
  start_year = 2018,
  end_year = 2020,
  cache_dir = cache_dir,
  reporter_type = "annual_reporters",
  period = "historical"
)
```

### Validation output table

Use `build_dwd_validation_table()` after joining observed and simulated records by keys such as year/station/phase. The output contains:

- `observed_date`
- `simulated_date`
- `error_days` (= simulated - observed)
- summary metrics: `MAE_days`, `RMSE_days`, `bias_days`, and `R2` (when enough variation exists)

### Important

- Keep DWD cache outside this repository.
- Never commit downloaded DWD data to GitHub.
