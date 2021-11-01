#' File này bao gồm các hàm dùng để tính toán kết quả các bài toán mà người dùng hay gặp phải
printf <- function (...) {
  print(sprintf(...))
}

#' Hàm này dùng để tìm giá trị alpha từ giá trị z_alpha cho trước (phân bố chuẩn)
#' Hàm này có thể dùng để truyền trực tiếp giá trị z_alpha vào các hàm, từ đó thu được kết quả sát với
#' kết quả thu được khi tính toán bằng tay hơn.
#' @export
get_alpha <- function(z_alpha, two_side = TRUE) {
  if(two_side) {
    return((1 - pnorm(z_alpha)) * 2)
  } else {
    return((1 - pnorm(z_alpha)))
  }
}

#' Hàm này dùng để tính ước lượng khoảng cho giá trị trung bình, dùng phân bố chuẩn tắc
#' @export
estimate_mean_norm <- function(n, mean, sigma, alpha) {
  z_alpha <- qnorm(1-alpha/2)
  eps <- z_alpha * sigma / sqrt(n)
  top <- mean + eps
  bottom <- mean - eps
  print("Bài toán: Ước lượng khoảng cho trung bình (phân bố chuẩn)")
  printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
}

#' Hàm này dùng để tính ước lượng khoảng cho giá trị trung bình, dùng phân bố Student
#' @export
estimate_mean_t <- function(n, mean, s, alpha) {
  t_alpha <- qt(1 - alpha/2,df = n - 1)
  eps <- t_alpha * s / sqrt(n)
  top <- mean + eps
  bottom <- mean - eps
  print("Bài toán: Ước lượng khoảng cho trung bình (phân bố Student)")
  printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
}

#' Hàm này ước lượng khoảng cho phương sai.
#' @export
estimate_var <- function(n, s, alpha) {
  chi_sq_1 <- qchisq(alpha / 2, df=n-1)
  chi_sq_2 <- qchisq(1 - alpha / 2, df=n-1)
  bottom <- (n-1) * s * s / chi_sq_2
  top <- (n-1) * s * s / chi_sq_1
  print("Bài toán: Ước lượng khoảng cho phương sai")
  printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
}

#' Hàm này ước lượng khoảng cho tỷ lệ
#' @export
estimate_prop <- function(n, f, alpha) {
  z_alpha <- qnorm(1-alpha/2)
  eps <- z_alpha * sqrt(f * (1-f) / n)
  bottom <- f - eps
  top <- f + eps
  print("Bài toán: Ước lượng khoảng cho tỷ lệ")
  printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
}

#' Hàm này xác định kích thước mẫu cho trường hợp ước lượng giá trị trung bình.
#' @export
sample_size_mean <- function(sigma, eps, alpha) {
  z_alpha <- qnorm(1-alpha/2)
  value <- (sigma*z_alpha / eps) * (sigma*z_alpha / eps)
  print("Bài toán: Xác định kích thước mẫu (ước lượng trung bình)")
  printf("Kích thước mẫu tối thiểu: %.4f", value)
}

#' Hàm này xác đỉnh kích thước mẫu cho trường hợp ước lượng tỷ lệ (công thức 1)
#' @export
sample_size_prop_1 <- function(f, eps, alpha) {
  z_alpha <- qnorm(1-alpha/2)
  value <- z_alpha*z_alpha * f*(1 - f) / (eps*eps)
  print("Bài toán: Xác định kích thước mẫu (ước lượng tỷ lệ, đã biết f)")
  printf("Kích thước mẫu tối thiểu: %.4f", value)
}

#' Hàm này xác định kích thước mẫu cho trường hợp ước lượng tỷ lệ (công thức 2)
#' @export
sample_size_prop_2 <- function(eps, alpha) {
  z_alpha <- qnorm(1-alpha/2)
  value <- z_alpha*z_alpha / (4*eps*eps)
  print("Bài toán: Xác định kích thước mẫu (ước lượng tỷ lệ, chưa biết f)")
  printf("Kích thước mẫu tối thiểu: %.4f", value)
}

#' Hàm này kiểm định giả thiết về gái trị trung bình, dùng phân bố chuẩn
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' @export
test_mean_norm <- function(n, mean, mean_0, sigma, alpha, mode="neq") {
  test <- (mean - mean_0) * sqrt(n) / sigma
  print("Bài toán: Kiểm định giả thiết cho giá trị trung bình (phân bố chuẩn)")
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

#' Hàm này kiểm định giả thiết về gái trị trung bình, dùng phân bố student
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' @export
test_mean_t <- function(n, mean, mean_0, s, alpha, mode="neq") {
  test <- (mean - mean_0) * sqrt(n) / s
  print("Bài toán: Kiểm định giả thiết cho giá trị trung bình (phân bố Student)")
  printf("Kết quả test thống kê: %.4f", test)
  c <- switch(
    mode,
    "neq" = qt(1 - alpha / 2, df=n-1),
    "less" = qt(1 - alpha, df=n-1),
    "greater" = qt(1 - alpha, df=n-1)
  )
  printf("Kết quả của c: %.4f", c)
  if(abs(test) > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' Hàm này kiểm định giả thiết về xác suất
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' @export
test_prop <- function(n, f, p_0, alpha, mode="neq") {
  test <- (f - p_0) * sqrt(n) / sqrt(p_0 * (1-p_0))
  print("Bài toán: Kiểm định giả thiết cho tỷ lệ")
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

#' Hàm này thực hiện kiểm định khi bình phương
#' actual, expect là các vector thể hiện tần số quan sát và tần số lý thuyết
#' @export
test_chi_squared <- function(actual, expected, alpha) {
  test <- sum((actual - expected)*(actual - expected) / expected)
  c <- qchisq(1 - alpha, df=length(actual)-1)
  print("Bài toán: Kiểm định khi bình phương (kiểm định cho k tỷ lệ)")
  printf("Kết quả test thống kê: %.4f", test)
  printf("Kết quả của c: %.4f", c)
  if(test > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' Hàm này so sánh 2 giá trị trung bình (dùng phân bố chuẩn)
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' @export
test_2_mean_norm <- function(n1, n2, mean1, mean2, sigma1, sigma2, alpha, mode="neq") {
  test <- (mean1 - mean2) / sqrt(sigma1*sigma1/n1 + sigma2*sigma2/n2)
  print("Bài toán: So sánh 2 giá trị trung bình (phân bố chuẩn)")
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

#' Hàm này so sánh 2 giá trị trung bình (dùng phân bố Student)
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' @export
test_2_mean_t <- function(n1, n2, mean1, mean2, s1, s2, alpha, mode="neq") {
  s <- ((n1-1)*s1*s1 + (n2-1)*s2*s2) / (n1+n2-2)
  test <- (mean1 - mean2) / (sqrt(s) * sqrt(1/n1 + 1/n2))
  print("Bài toán: So sánh 2 giá trị trung bình (phân bố Student)")
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

#' Hàm này thực hiện so sánh 2 tỷ lệ
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' @export
test_2_prop <- function(n1, n2, f1, f2, alpha, mode="neq") {
  f <- (f1*n1 + f2*n2) / (n1+n2)
  test <- (f1 - f2) / sqrt(f*(1-f)*(1/n1 + 1/n2))
  print("Bài toán: So sánh 2 tỷ lệ")
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

#' Hàm này thưc hiện so sánh n tỷ lệ.
#' m_i, n_i là các vector thể hiện số quan sát có đặc tính A
#' nào đó và tổng số quan sát trong các tập tổng thể.
#' @export
test_n_prop <- function(m_i, n_i, alpha) {
  sum_n_i <- sum(n_i)
  sum_m_i <- sum(m_i)
  sum_l_i <- sum_n_i - sum_m_i
  test <- (sum_n_i * sum_n_i) / (sum_m_i * sum_l_i) * sum(m_i * m_i / n_i) - sum_n_i * sum_m_i / sum_l_i
  c <- qchisq(1 - alpha, df=length(m_i) - 1)
  print("Bài toán: So sánh n tỷ lệ")
  printf("Kết quả test thống kê: %.4f", test)
  printf("Kết quả của c: %.4f", c)
  if(test > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' Hàm này thực hiện kiểm định tính độc lập của 2 dấu hiệu A và B
#' matrix là một ma trận với các phần tử n_ij nằm trong bảng liên hợp các dấu hiệu
#' (Contingency Table)
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
  print("Bài toán: Kiểm định tính độc lập")
  printf("Kết quả test thống kê: %.4f", test)
  printf("Kết quả của c: %.4f", c)
  if(test > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}

#' Hàm này tính hệ số tương quan.
#' @export
correlation <- function(x, y) {
  print("Bài toán: Tính hệ số tương quan")
  printf("Hệ số tương quan: %.4f", cor(x, y, method = "pearson"))
}

#' Hàm này thực hiện bài toán hồi quy tuyến tính đơn
#' @export
linear_regression <- function(x, y) {
  df <- data.frame(X = x, Y = y)
  result <- lm(Y ~ X, data = df)
  print("Bài toán: Bài toán hồi quy tuyến tính đơn")
  printf("Hệ số tự do: %.4f", result$coefficients[1])
  printf("Hệ số ứng với x: %.4f", result$coefficients[2])
}

#' Hàm này thực hiện việc dự đoán giá trị trong bài toán hồi quy tuyến tính đơn.
#' @export
linear_regression_predict <- function(x, y, value) {
  df <- data.frame(X = x, Y = y)
  result <- lm(Y ~ X, data = df)
  printf("Bài toán: Dự báo giá trị, dựa vào hồi quy tuyến tính đơn")
  printf("Giá trị của Y là: %.4f", result$coefficients[1] + value * result$coefficients[2])
}

