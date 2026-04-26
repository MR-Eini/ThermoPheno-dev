# ThermoPheno development news

## ThermoPheno 0.1.0.9000

- Added DWD Open Data access helpers using `rdwd` for daily temperature and crop phenology retrieval with user-defined local cache directories.
- Added validation table helper with observed/simulated dates, error in days, MAE, RMSE, bias, and R².
- Added CI-safe tests (including mocked DWD access tests) and DWD validation workflow documentation.
- Improved README, pkgdown metadata, and workflow support for DWD validation usage.
