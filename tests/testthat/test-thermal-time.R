test_that("simple thermal time is correct", {
  expect_equal(calc_daily_tt(tmin = 5, tmax = 15, t_base = 5, mode = "simple"), 5)
  expect_equal(calc_daily_tt(tmin = 0, tmax = 4, t_base = 5, mode = "simple"), 0)
})

test_that("capped thermal time is correct", {
  expect_equal(calc_daily_tt(tmin = 20, tmax = 40, t_base = 5, t_opt = 25, mode = "capped"), 20)
})

test_that("triangular thermal time is correct", {
  expect_equal(calc_daily_tt(tmin = 20, tmax = 40, t_base = 5, t_opt = 25, t_max_cut = 35, mode = "triangular"), 10)
  expect_error(calc_daily_tt(tmin = 20, tmax = 40, t_base = 5, mode = "triangular"))
})
