library(thongke)
library(testthat)

test_that("Test các check_* function", {
  expect_equal(check_estimate_prop(100, 0.6), TRUE)
  expect_equal(check_test_prop(100, 0.6), TRUE)
  expect_equal(check_test_2_prop(100, 200, 0.2, 0.3), TRUE)
  expect_equal(check_test_k_prop(m_i = c(100, 100, 100), n_i = c(300, 300, 300)), TRUE)
  expect_equal(check_test_independent(matrix = matrix(data = c(328, 122, 77, 33),
                                   ncol = 2, nrow = 2)), TRUE)

  expect_equal(check_estimate_prop(100, 0.07), FALSE)
  expect_equal(check_test_prop(100, 0.03), FALSE)
  expect_equal(check_test_2_prop(100, 200, 0.01, 0.02), FALSE)
  expect_equal(check_test_k_prop(m_i = c(3, 3, 3), n_i = c(300, 300, 300)), FALSE)
  expect_equal(check_test_independent(matrix = matrix(data = c(4, 4, 4, 4),
                                   ncol = 2, nrow = 2)), FALSE)
})
