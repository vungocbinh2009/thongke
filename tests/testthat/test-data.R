library(thongke)
library(testthat)

test_that("Test các data_* function", {
  data1 <- data_simulate_discrete(n = 100, mean = 5, sd = 2, min = 2, max = 8, simplify = TRUE)
  data2 <- data_simulate_continuous(n = 100, mean = 5, sd = 2, min = 2, max = 8, size = 1, simplify = TRUE)
  data3 <- data_simulate_regression(n = 10, min_x = 15, max_x = 30, b0 = 5, b1 = 10, sd_eps = 3, round_digits = 2, simplify = TRUE)
  print(mean(data1))
  print(mean(data2))
  print(mean(data3$x))
  print(mean(data3$y))
  data_simulate_test_goodness_of_fit(
    expected = c(100, 100, 100, 100, 100, 100),
    simplify = TRUE
  )

  data_simulate_test_k_prop(
    expected_m_i = c(160, 320, 240, 160),
    expected_l_i = c(40, 80, 60, 40),
    simplify = TRUE
  )

  data_simulate_test_independent(
    expected_matrix = matrix(
      c(100, 200, 300, 400, 500, 600, 700, 800, 900), nrow=3, ncol=3, byrow = TRUE
    ),
    simplify = TRUE
  )

  expect_equal(1, 1)
})
