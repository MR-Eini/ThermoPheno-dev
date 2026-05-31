test_that("observed-calendar calibration estimates thermal time", {
  dates <- seq(as.Date("2000-01-01"), as.Date("2001-12-31"), by = "day")
  weather <- prepare_weather(data.frame(date = dates, tmin = 10, tmax = 20))
  obs <- data.frame(
    crop_year = c(2000, 2001),
    observed_planting_date = as.Date(c("2000-04-01", "2001-04-01")),
    observed_harvest_date = as.Date(c("2000-04-10", "2001-04-10"))
  )
  tt <- estimate_required_tt_from_observed(weather, obs, calibration_years = 2000:2001, t_base = 10)
  expect_equal(tt$required_tt, 50)
  expect_equal(tt$n_valid_years, 2)
})
