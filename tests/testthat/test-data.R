library(thongke)
library(testthat)

test_that("Test các data_* function", {
  data_1 <- data_simulate_discrete(n = 100, mean = 5, sd = 2, min = 2, max = 8)
  print(mean(data_1))
  print(var(data_1))
  data_2 <- data_simulate_continuous(n = 100, mean = 5, sd = 2, min = 2, max = 8, size = 1)
  print(mean(data_2))
  print(var(data_2))
  data_3 <- data_simulate_regression(n = 10, min_x = 15, max_x = 30, b0 = 5, b1 = 10,
                           sd_eps = 3, round_digits = 2)
  x <- data_3$x
  y <- data_3$y
  print(mean(x))
  print(var(x))
  print(mean(y))
  print(var(y))

  data_simulate_test_goodness_of_fit(c(100, 100, 100, 100, 100, 100),
                                     max_diff = 30, min_diff = -30, step = 50)

  data_simulate_test_k_prop(c(160, 320, 240, 160), c(40, 80, 60, 40),
                            max_diff = 10, min_diff = -10, step = 50)

  data_simulate_test_independent(matrix(c(100, 200, 300, 400, 500, 600, 700, 800, 900), nrow=3, ncol=3, byrow = TRUE),
                                 max_diff = 15, min_diff = -15, step = 200)

  data_simulate_test_goodness_of_fit_2(c(100, 100, 100, 100, 100, 100),)

  data_simulate_test_k_prop_2(c(160, 320, 240, 160), c(40, 80, 60, 40))

  data_simulate_test_independent_2(matrix(c(100, 200, 300, 400, 500, 600, 700, 800, 900), nrow=3, ncol=3, byrow = TRUE))

  expect_equal(1, 1)

})