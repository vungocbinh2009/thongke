#' File này gồm các hàm kiểm tra xem dữ liệu đầu vào có đủ điều kiện để tính toán hay không ?

check_estimate_prop <- function (n, f, alpha) {
  return(n*f > 10 && n*(1-f) > 10)
}

check_test_prop <- function (n, p0) {
  return(n*p0 >= 5 && n*(1-p0) >= 5)
}

check_test_2_prop <- function(n1, n2, f1, f2, alpha, mode="neq") {
  f <- (f1*n1 + f2*n2) / (n1+n2)
  return(n*f >= 10 && n*(1-f) >= 10)
}

check_test_n_prop <- function(m_i, n_i, alpha) {
  sum_n_i <- sum(n_i)
  sum_m_i <- sum(m_i)
  sum_l_i <- sum_n_i - sum_m_i
  prop_1 <- (n_i * sum_m_i) / sum_n_i
  prop_2 <- (n_i * sum_l_i) / sum_n_i
  return(length(prop_1[prop_1 < 5]) + length(prop_2[prop_2 < 5]) == 0)
}

check_test_independent <- function(matrix, alpha) {
  row_sums <- rowSums(matrix)
  col_sums <- colSums(matrix)
  n <- sum(row_sums)
  f <- NULL
  for(i in seq_along(row_sums)) {
    for(j in seq_along(col_sums)) {
      c(f, row_sums[i] * col_sums[j])
    }
  }
  f <- f / n
  return(length(f[f < 5]) == 0)
}