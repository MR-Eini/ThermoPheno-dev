test_that("summer crop simulation reaches maturity under simple constant weather", {
  dates <- seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day")
  weather <- prepare_weather(data.frame(date = dates, tmin = 10, tmax = 20))
  sim <- simulate_one_year(
    weather = weather,
    sim_year = 2001,
    required_tt = 100,
    earliest_planting_mmdd = "03-01",
    latest_planting_mmdd = "03-31",
    latest_harvest_mmdd = "10-01",
    t_base = 10,
    tt_mode = "simple",
    crop_type = "summer",
    min_mean_temp_plant = 0,
    crop_name = "Test crop"
  )
  expect_equal(sim$status, "mature")
  expect_equal(as.character(sim$planting_date), "2001-03-01")
  expect_equal(as.character(sim$harvest_date), "2001-03-20")
})

test_that("run_simulation returns one row per year for summer crops", {
  dates <- seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day")
  weather <- prepare_weather(data.frame(date = dates, tmin = 10, tmax = 20))
  sim <- run_simulation(
    weather = weather,
    crop_name = "Test crop",
    required_tt = 100,
    earliest_planting_mmdd = "03-01",
    latest_planting_mmdd = "03-31",
    latest_harvest_mmdd = "10-01",
    t_base = 10,
    tt_mode = "simple",
    crop_type = "summer"
  )
  expect_equal(nrow(sim), 3)
})
