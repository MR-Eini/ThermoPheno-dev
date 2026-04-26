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

test_that("DWD download helpers can be mocked and skipped on CI/CRAN", {
  skip_on_cran()
  skip_on_ci()

  fake_select <- function(...) "fake_link.zip"
  fake_data <- function(...) tempfile(fileext = ".zip")
  fake_read <- function(...) data.frame(MESS_DATUM = c(20200101, 20200102), TMK = c(5, 6))

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
