# DWD validation workflow

This project supports pulling DWD Open Data via `rdwd` and validating ThermoPheno simulations against DWD phenology observations.

## 1) Choose a local cache directory

Use a path **outside** this repository, e.g. `~/thermopheno_dwd_cache`.

## 2) Download DWD daily temperature

```r
temp <- ThermoPheno:::get_dwd_daily_temperature(
  station_id = "00386",
  start_year = 2018,
  end_year = 2020,
  cache_dir = "~/thermopheno_dwd_cache",
  period = "historical"
)
```

## 3) Download DWD crop phenology observations

```r
pheno <- ThermoPheno:::get_dwd_crop_phenology(
  crop_pattern = "hafer|oat",
  start_year = 2018,
  end_year = 2020,
  cache_dir = "~/thermopheno_dwd_cache",
  reporter_type = "annual_reporters",
  period = "historical"
)
```

## 4) Build validation table and metrics

```r
val <- ThermoPheno:::build_dwd_validation_table(
  observed = pheno,
  simulated = sim,
  by = "year",
  observed_date_col = "observed_date",
  simulated_date_col = "maturity_date"
)

val$table    # observed_date, simulated_date, error_days
val$metrics  # MAE_days, RMSE_days, bias_days, R2
```

## Notes

- Do not commit cached DWD files.
- Cache directory is user-controlled and reproducible across runs.
