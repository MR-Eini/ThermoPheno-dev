# DWD validation workflow for ThermoPheno

This workflow validates ThermoPheno against real German DWD observations.

## Inputs downloaded automatically

- DWD annual crop phenology observations from the CDC phenology archive.
- DWD daily climate observations from the CDC daily KL archive.
- Phenology and climate station metadata when available.

## Main script

```r
source("validation/dwd_validate_thermopheno.R")
```

## Outputs

The script writes outputs to:

```text
validation/results/
```

Expected files include:

```text
validation_pairs.csv
validation_metrics.csv
validation_metrics_by_station.csv
validation_scatter_planting.png
validation_scatter_harvest.png
VALIDATION_SUMMARY.txt
```

## Notes

The DWD crop archive uses German file and phase names. The script includes automatic file discovery and configurable phase-name matching through `validation/crop_config.csv`. If no matching phase name is found for a crop, inspect `validation/results/phase_candidates.csv` and adjust the regular expressions in the configuration file.

The first validation design uses 1991–2010 for calibration and 2011–2024 for validation.
