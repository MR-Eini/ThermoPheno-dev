test_that("prepare_weather adds required derived columns", {
  x <- data.frame(date = c("2000-01-01", "2000-01-02"), tmin = c(1, 2), tmax = c(5, 6))
  y <- prepare_weather(x)
  expect_true(all(c("year", "doy", "tmean") %in% names(y)))
  expect_equal(y$tmean, c(3, 4))
})

test_that("prepare_weather rejects missing columns", {
  expect_error(prepare_weather(data.frame(date = "2000-01-01", tmin = 1)))
})
