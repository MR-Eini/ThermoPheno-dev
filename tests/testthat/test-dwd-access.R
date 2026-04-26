test_that("build_dwd_validation_table computes expected metrics", {
  observed <- data.frame(year = 2020:2022, obs_date = as.Date(c("2020-06-01", "2021-06-01", "2022-06-01")))
  simulated <- data.frame(year = 2020:2022, sim_date = as.Date(c("2020-06-03", "2021-05-30", "2022-06-06")))

  val <- ThermoPheno:::build_dwd_validation_table(
    observed = observed,
    simulated = simulated,
    by = "year",
    observed_date_col = "obs_date",
    simulated_date_col = "sim_date"
  )

  expect_true(all(c("observed_date", "simulated_date", "error_days") %in% names(val$table)))
  expect_equal(val$metrics$n, 3)
  expect_true(is.finite(val$metrics$MAE_days))
  expect_true(is.finite(val$metrics$RMSE_days))
})

test_that("DWD daily temperature columns are standardized", {
  raw <- data.frame(
    STATIONS_ID = c(386, 386),
    MESS_DATUM = c(20200101, 20200102),
    TNK = c("-1.5", "-999"),
    TXK = c("4.5", "6.0"),
    TMK = c("1.5", "3.0")
  )

  out <- ThermoPheno:::standardize_dwd_daily_temperature(raw)

  expect_equal(out$station_id, c("00386", "00386"))
  expect_equal(out$date, as.Date(c("2020-01-01", "2020-01-02")))
  expect_equal(out$year, c(2020L, 2020L))
  expect_equal(out$tmin[1], -1.5)
  expect_true(is.na(out$tmin[2]))
  expect_equal(out$tmax, c(4.5, 6.0))
  expect_equal(out$tmean, c(1.5, 3.0))
})

test_that("DWD crop phenology columns are standardized and filterable", {
  raw <- data.frame(
    Stations_id = c(386, 390),
    Referenzjahr = c(2020, 2021),
    Objekt_id = c(208, 208),
    Phase_id = c(10, 20),
    Eintrittsdatum = c(20200501, 20210601),
    Jultag = c(122, 152)
  )

  out <- ThermoPheno:::standardize_dwd_crop_phenology(raw, source_file = "PH_Jahresmelder_Hafer.zip")
  out <- ThermoPheno:::.filter_optional(out, "station_id", "00386")
  out <- ThermoPheno:::.filter_optional(out, "phenophase_id", 10L)

  expect_equal(nrow(out), 1)
  expect_equal(out$station_id, "00386")
  expect_equal(out$year, 2020L)
  expect_equal(out$crop_id, 208L)
  expect_equal(out$phenophase_id, 10L)
  expect_equal(out$observed_date, as.Date("2020-05-01"))
})

test_that("DWD download helpers can be mocked and skipped on CI/CRAN", {
  skip_on_cran()
  skip_on_ci()

  fake_select <- function(...) "fake_link.zip"
  fake_data <- function(...) tempfile(fileext = ".zip")
  fake_read <- function(...) data.frame(
    STATIONS_ID = c(386, 386),
    MESS_DATUM = c(20200101, 20200102),
    TNK = c(1, 2),
    TXK = c(9, 10),
    TMK = c(5, 6)
  )

  tmp_cache <- tempfile("dwdcache_")

  out <- testthat::with_mocked_bindings(
    ThermoPheno:::get_dwd_daily_temperature(
      station_id = "00386",
      start_year = 2020,
      end_year = 2020,
      cache_dir = tmp_cache,
      period = "historical"
    ),
    .tp_rdwd_select = fake_select,
    .tp_rdwd_data = fake_data,
    .tp_rdwd_read = fake_read,
    assert_rdwd_available = function() TRUE
  )

  expect_true(nrow(out) == 2)
  expect_true(all(out$year == 2020))
})
