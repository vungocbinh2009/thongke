#' Hàm kiểm tra điều kiện để áp dụng công thức tính ước lượng khoảng cho tỷ lệ
check_estimate_prop <- function (n, f) {
  return(n*f > 10 && n*(1-f) > 10)
}

#' Hàm kiểm tra điều kiện để áp dụng bài toán kiểm định giả thiết cho tỷ lệ
check_test_prop <- function (n, p0) {
  return(n*p0 >= 5 && n*(1-p0) >= 5)
}

#' Hàm kiểm tra điều kiện để áp dụng bài toán so sánh 2 tỷ lệ
check_test_2_prop <- function(n1, n2, f1, f2) {
  f <- (f1*n1 + f2*n2) / (n1+n2)
  n <- n1 + n2
  return(n*f >= 10 && n*(1-f) >= 10)
}

#' Hàm kiểm tra điều kiện để áp dụng bài toán so sánh nhiều tỷ lệ
check_test_k_prop <- function(m_i, n_i) {
  sum_n_i <- sum(n_i)
  sum_m_i <- sum(m_i)
  sum_l_i <- sum_n_i - sum_m_i
  prop_1 <- (n_i * sum_m_i) / sum_n_i
  prop_2 <- (n_i * sum_l_i) / sum_n_i
  return(length(prop_1[prop_1 < 5]) + length(prop_2[prop_2 < 5]) == 0)
}

#' Hàm kiểm tra điều kiện để áp dụng bài toán kiểm định tính độc lập
check_test_independent <- function(matrix) {
  row_sums <- rowSums(matrix)
  col_sums <- colSums(matrix)
  n <- sum(row_sums)
  f <- NULL
  for(i in seq_along(row_sums)) {
    for(j in seq_along(col_sums)) {
      f <- c(f, row_sums[i] * col_sums[j])
    }
  }
  f <- f / n
  return(length(f[f < 5]) == 0)
}
