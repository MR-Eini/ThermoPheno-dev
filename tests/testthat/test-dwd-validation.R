test_that("validate_weather_dwd catches missing columns", {
  bad <- data.frame(date = "2020-01-01", tmin = 1)
  res <- ThermoPheno:::validate_weather_dwd(bad)
  expect_false(res$ok)
  expect_true(any(grepl("Missing required columns", res$errors)))
})

test_that("validate_weather_dwd flags tmin > tmax", {
  bad <- data.frame(date = "2020-01-01", tmin = 11, tmax = 9)
  res <- ThermoPheno:::validate_weather_dwd(bad)
  expect_false(res$ok)
})

test_that("validate_weather_dwd accepts valid minimal weather table", {
  good <- data.frame(
    date = c("2020-01-01", "2020-01-02"),
    tmin = c(-1, 0),
    tmax = c(4, 5)
  )
  res <- ThermoPheno:::validate_weather_dwd(good)
  expect_true(res$ok)
  expect_length(res$errors, 0)
})
