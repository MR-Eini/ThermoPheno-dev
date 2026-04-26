test_that("calc_daily_tt simple and capped modes behave as expected", {
  expect_equal(ThermoPheno:::calc_daily_tt(5, 15, t_base = 8, mode = "simple"), 2)
  expect_equal(ThermoPheno:::calc_daily_tt(20, 30, t_base = 8, t_opt = 20, mode = "capped"), 12)
})

test_that("calc_daily_tt triangular returns zero outside valid thermal band", {
  expect_equal(ThermoPheno:::calc_daily_tt(-5, 1, t_base = 0, t_opt = 20, t_max_cut = 35, mode = "triangular"), 0)
  expect_equal(ThermoPheno:::calc_daily_tt(40, 44, t_base = 0, t_opt = 20, t_max_cut = 35, mode = "triangular"), 0)
})


test_that("prepare_weather creates required derived columns", {
  df <- data.frame(
    date = c("2020-01-01", "2020-01-02"),
    tmin = c(0, 2),
    tmax = c(10, 12)
  )

  out <- ThermoPheno:::prepare_weather(df)
  expect_true(all(c("year", "doy", "tmean") %in% names(out)))
  expect_equal(out$tmean, c(5, 7))
})
