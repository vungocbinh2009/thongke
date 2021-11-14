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
  expect_equal(1, 1)

})