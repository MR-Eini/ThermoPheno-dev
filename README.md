# ThermoPheno

ThermoPheno is an R package and Shiny application for thermal-time-based crop phenology simulation. It estimates planting, maturity, and harvest timing from daily minimum and maximum temperature data and can be used for historical analysis, scenario comparison, and validation against observed crop calendars.

## Installation from this ZIP

Unzip the package, then install the source folder:

```r
install.packages(c("remotes", "testthat"))
remotes::install_local("ThermoPheno", dependencies = TRUE)
```

Alternatively, if you use the included source archive:

```r
install.packages("ThermoPheno_0.1.0.tar.gz", repos = NULL, type = "source")
```

## Launch the Shiny app

```r
library(ThermoPheno)
run_thermopheno_app()
```

The shorter alias also works:

```r
ThermoPheno()
```

## Example data

Bundled example files are available in `inst/extdata`:

```r
system.file("extdata", "Germany_historical_1981_2010_dummy_data.csv", package = "ThermoPheno")
system.file("extdata", "Germany_10_scenarios_2071_2100_dummy_data.csv", package = "ThermoPheno")
```

## Minimal model example

```r
library(ThermoPheno)

weather_file <- system.file(
  "extdata", "Germany_historical_1981_2010_dummy_data.csv",
  package = "ThermoPheno"
)
weather <- prepare_weather(read.csv(weather_file))

req <- estimate_required_tt(
  weather = weather,
  baseline_years = 1981:2010,
  planting_mmdd = "04-15",
  days_to_maturity = 140,
  t_base = 8,
  t_opt = 25,
  t_max_cut = 35,
  tt_mode = "triangular",
  crop_type = "summer"
)

sim <- run_simulation(
  weather = weather,
  crop_name = "Maize",
  required_tt = req$required_tt,
  earliest_planting_mmdd = "03-15",
  latest_planting_mmdd = "05-31",
  latest_harvest_mmdd = "10-01",
  t_base = 8,
  t_opt = 25,
  t_max_cut = 35,
  tt_mode = "triangular",
  crop_type = "summer",
  min_mean_temp_plant = 8
)

head(sim)
```

## Local checks

```r
devtools::load_all()
devtools::document()
devtools::test()
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))
```

## Main assumptions

- Crop development is driven by air temperature only.
- Daily mean temperature is calculated as `(tmin + tmax) / 2`.
- The current winter-crop logic includes simple vernalization and dormancy rules.
- Photoperiod, radiation, water stress, cultivar differences, and adaptive farmer behaviour are not explicitly modelled.

## License

MIT.
