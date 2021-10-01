printf <- function (...) {
  print(sprintf(...))
}

#' @export
estimate_mean_1 <- function(mean, sigma, alpha, n) {
  u_alpha <- qnorm(1-alpha/2)
  eps <- u_alpha * sigma / sqrt(n)
  top <- mean + eps
  bottom <- mean - eps
  printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
}

#' @export
estimate_mean_2 <- function(mean, s, alpha, degree, n) {
  t_alpha <- qt(1 - alpha/2,df = degree)
  eps <- t_alpha * s / sqrt(n)
  top <- mean + eps
  bottom <- mean - eps
  printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
}

#' @export
estimate_var <- function(s, n, alpha, degree) {
  chi_sq_1 <- qchisq(alpha / 2, df=degree)
  chi_sq_2 <- qchisq(1 - alpha / 2, df=degree)
  bottom <- (n-1) * s * s / chi_sq_2
  top <- (n-1) * s * s / chi_sq_1
  printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
}

#' @export
estimate_prop <- function(f, alpha, n) {
  u_alpha <- qnorm(1-alpha/2)
  eps <- u_alpha * sqrt(f * (1-f) / n)
  bottom <- f - eps
  top <- f + eps
  printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
}

#' @export
sample_size_mean <- function(sigma, u_beta, eps) {
  value <- (sigma*u_beta / eps) * (sigma*u_beta / eps)
  printf("Kích thước mẫu tối thiểu: %.4f", value)
}

#' @export
sample_size_prop_1 <- function(f, u_beta, eps) {
  value <- u_beta*u_beta * f*(1 - f) / (eps*eps)
  printf("Kích thước mẫu tối thiểu: %.4f", value)
}

#' @export
sample_size_prop_2 <- function(u_beta, eps) {
  value <- u_beta*u_beta / (4*eps*eps)
  printf("Kích thước mẫu tối thiểu: %.4f", value)
}

#' @export
test_mean_1 <- function(mean, mean_0, sigma, alpha, n, mode="neq") {
  test <- (mean - mean_0) * sqrt(n) / sigma
  printf("Kết quả test thống kê: %.4f", test)
  c <- switch(
    mode,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  printf("Kết quả của c: %.4f", c)
  if(abs(test) > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' @export
test_mean_2 <- function(mean, mean_0, s, alpha, degree, n, mode="neq") {
  test <- (mean - mean_0) * sqrt(n) / s
  printf("Kết quả test thống kê: %.4f", test)
  c <- switch(
    mode,
    "neq" = qt(1 - alpha / 2, df=degree),
    "less" = qt(1 - alpha, df=degree),
    "greater" = qt(1 - alpha, df=degree)
  )
  printf("Kết quả của c: %.4f", c)
  if(abs(test) > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' @export
test_prop <- function(f, p_0, n, alpha, mode="neq") {
  test <- (f - p_0) * sqrt(n) / sqrt(p_0 * (1-p_0))
  printf("Kết quả test thống kê: %.4f", test)
  c <- switch(
    mode,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  printf("Kết quả của c: %.4f", c)
  if(abs(test) > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' @export
test_chi_squared <- function(actual, expected, alpha) {
  test <- sum((actual - expected)*(actual - expected) / expected)
  c <- qchisq(1 - alpha, df=length(actual)-1)
  printf("Kết quả test thống kê: %.4f", test)
  printf("Kết quả của c: %.4f", c)
  if(test > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' @export
test_2_mean_1 <- function(mean1, mean2, sigma1, sigma2, n1, n2, alpha, mode="neq") {
  test <- (mean1 - mean2) / sqrt(sigma1*sigma1/n1 + sigma2*sigma2/n2)
  printf("Kết quả test thống kê: %.4f", test)
  c <- switch(
    mode,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  printf("Kết quả của c: %.4f", c)
  if(abs(test) > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' @export
test_2_mean_2 <- function(mean1, mean2, s1, s2, n1, n2, alpha, mode="neq") {
  s <- ((n1-1)*s1*s1 + (n2-1)*s2*s2) / (n1+n2-2)
  test <- (mean1 - mean2) / (s * sqrt(1/n1 + 1/n2))
  printf("Kết quả test thống kê: %.4f", test)
  c <- switch(
    mode,
    "neq" = qt(1 - alpha / 2, df=n1+n2-2),
    "less" = qt(1 - alpha, df=n1+n2-2),
    "greater" = qt(1 - alpha, df=n1+n2-2)
  )
  printf("Kết quả của c: %.4f", c)
  if(abs(test) > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' @export
test_2_prop <- function(f1, f2, n1, n2, alpha, mode="neq") {
  f <- (f1*n1 + f2*n2) / (n1+n2)
  test <- (f1 - f2) / sqrt(f*(1-f)*(1/n1 + 1/n2))
  printf("Kết quả test thống kê: %.4f", test)
  c <- switch(
    mode,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  printf("Kết quả của c: %.4f", c)
  if(abs(test) > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' @export
test_n_prop <- function(m_i, n_i, alpha) {
  sum_n_i <- sum(n_i)
  sum_m_i <- sum(m_i)
  sum_l_i <- sum_n_i - sum_m_i
  test <- (sum_n_i * sum_n_i) / (sum_m_i * sum_l_i) * sum(m_i * m_i / n_i) - sum_n_i * sum_m_i / sum_l_i
  c <- qchisq(1 - alpha, df=length(m_i) - 1)
  
  printf("Kết quả test thống kê: %.4f", test)
  printf("Kết quả của c: %.4f", c)
  if(test > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' @export
test_independent <- function(matrix, alpha) {
  row_sums <- rowSums(matrix)
  col_sums <- colSums(matrix)
  n <- sum(row_sums)
  test <- 0
  for(i in seq_along(row_sums)) {
    for(j in seq_along(col_sums)) {
      test <- test + ((matrix[i,j] * matrix[i,j]) / (row_sums[i] * col_sums[j]))
    }
  }
  test <- n * (test - 1)
  degree <- (length(row_sums)-1) * (length(col_sums)-1)
  c <- qchisq(1 - alpha, df=degree)
  printf("Kết quả test thống kê: %.4f", test)
  printf("Kết quả của c: %.4f", c)
  if(test > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' @export
correlation <- function(x, y) {
  printf("Hệ số tương quan: %.4f", cor(x, y, method = "pearson"))
}

#' @export
linear_regression <- function(x, y) {
  df <- data.frame(X = x, Y = y)
  print(lm(lm(Y ~ X, data = df)))

}

