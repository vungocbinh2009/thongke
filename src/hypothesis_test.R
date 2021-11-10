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
  if(!check_test_prop(n, p_0)) {
    print("Không đủ điều kiện áp dụng test thống kê")
    return()
  }

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
  if(!check_test_2_prop(n1, n2, f1, f2)) {
    print("Không đủ điều kiện áp dụng test thống kê")
    return()
  }

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
  if(!check_test_n_prop(m_i, n_i)) {
    print("Không đủ điều kiện áp dụng test thống kê")
    return()
  }

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
  if(!check_test_independent(matrix)) {
    print("Không đủ điều kiện áp dụng test thống kê")
    return()
  }

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

