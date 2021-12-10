#' Hàm này kiểm định giả thiết về gái trị trung bình, dùng phân bố chuẩn
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
test_mean_norm <- function(n, mean, mean_0, sigma, alpha, mode="neq", silent = FALSE) {
  test <- (mean - mean_0) * sqrt(n) / sigma
  c <- switch(
    mode,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  if(!silent) {
    print("Bài toán: Kiểm định giả thiết cho giá trị trung bình (phân bố chuẩn)")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    case_1 <- mode == "less" && mean > mean_0
    case_2 <- mode == "greater" && mean < mean_0
    if(case_1 || case_2) {
      print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
    } else {
      get_test_result(test, c)
    }
  }
  return(list(test = test, c = c))
}

#' Hàm này kiểm định giả thiết về gái trị trung bình, dùng phân bố student
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
test_mean_t <- function(n, mean, mean_0, s, alpha, mode="neq", silent = FALSE) {
  test <- (mean - mean_0) * sqrt(n) / s
  c <- switch(
    mode,
    "neq" = qt(1 - alpha / 2, df=n-1),
    "less" = qt(1 - alpha, df=n-1),
    "greater" = qt(1 - alpha, df=n-1)
  )
  if(!silent) {
    print("Bài toán: Kiểm định giả thiết cho giá trị trung bình (phân bố Student)")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    case_1 <- mode == "less" && mean > mean_0
    case_2 <- mode == "greater" && mean < mean_0
    if(case_1 || case_2) {
      print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
    } else {
      get_test_result(test, c)
    }
  }
  return(list(test = test, c = c))
}

#' Hàm này kiểm định giả thiết về xác suất
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
test_prop <- function(n, f, p_0, alpha, mode="neq", silent = FALSE) {
  if(!check_test_prop(n, p_0)) {
    if(!silent) {
      print("Không đủ điều kiện áp dụng test thống kê")
    }
    return()
  }
  test <- (f - p_0) * sqrt(n) / sqrt(p_0 * (1-p_0))
  c <- switch(
    mode,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  if(!silent) {
    print("Bài toán: Kiểm định giả thiết cho tỷ lệ")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    case_1 <- mode == "less" && f > p_0
    case_2 <- mode == "greater" && f < p_0
    if(case_1 || case_2) {
      print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
    } else {
      get_test_result(test, c)
    }
  }
  return(list(test = test, c = c))
}

#' Hàm này đã Deprecated, dùng hàm test_goodness_of_fit để thay thế.
#' Hàm này thực hiện kiểm định khi bình phương
#' actual, expect là các vector thể hiện tần số quan sát và tần số lý thuyết
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
test_chi_squared <- function(actual, expected, alpha, silent = FALSE) {
  .Deprecated("test_goodness_of_fit")
  test <- sum((actual - expected)*(actual - expected) / expected)
  c <- qchisq(1 - alpha, df=length(actual)-1)
  if(!silent) {
    print("Bài toán: Kiểm định khi bình phương (kiểm định cho k tỷ lệ)")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    get_test_result(test, c)
  }
  return(list(test = test, c = c))
}

#' Hàm này thực hiện kiểm định khi bình phương
#' actual, expect là các vector thể hiện tần số quan sát và tần số lý thuyết
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
test_goodness_of_fit <- function (actual, expected, alpha, silent = FALSE) {
  test <- sum((actual - expected)*(actual - expected) / expected)
  c <- qchisq(1 - alpha, df=length(actual)-1)
  if(!silent) {
    print("Bài toán: Kiểm định khi bình phương (kiểm định cho k tỷ lệ)")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    get_test_result(test, c)
  }
  return(list(test = test, c = c))
}

#' Hàm này so sánh 2 giá trị trung bình (dùng phân bố chuẩn)
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
test_2_mean_norm <- function(n1, n2, mean1, mean2, sigma1, sigma2, alpha, mode="neq", silent = FALSE) {
  test <- (mean1 - mean2) / sqrt(sigma1*sigma1/n1 + sigma2*sigma2/n2)
  c <- switch(
    mode,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  if(!silent) {
    print("Bài toán: So sánh 2 giá trị trung bình (phân bố chuẩn)")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    case_1 <- mode == "less" && mean1 > mean2
    case_2 <- mode == "greater" && mean1 < mean2
    if(case_1 || case_2) {
      print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
    } else {
      get_test_result(test, c)
    }
  }

  return(list(test = test, c = c))
}

#' Hàm này so sánh 2 giá trị trung bình (dùng phân bố Student)
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test), giá trị c và phương sai chung s
#' @export
test_2_mean_t <- function(n1, n2, mean1, mean2, s1, s2, alpha, mode="neq", silent = FALSE) {
  s <- ((n1-1)*s1*s1 + (n2-1)*s2*s2) / (n1+n2-2)
  test <- (mean1 - mean2) / (sqrt(s) * sqrt(1/n1 + 1/n2))
  c <- switch(
    mode,
    "neq" = qt(1 - alpha / 2, df=n1+n2-2),
    "less" = qt(1 - alpha, df=n1+n2-2),
    "greater" = qt(1 - alpha, df=n1+n2-2)
  )
  if(!silent) {
    print("Bài toán: So sánh 2 giá trị trung bình (phân bố Student)")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    case_1 <- mode == "less" && mean1 > mean2
    case_2 <- mode == "greater" && mean1 < mean2
    if(case_1 || case_2) {
      print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
    } else {
      get_test_result(test, c)
    }
  }
  return(list(test = test, c = c, s = s))
}

#' Hàm này thực hiện so sánh 2 tỷ lệ
#' Tham số mode là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test) và giá trị c và tỷ lệ chung f
#' @export
test_2_prop <- function(n1, n2, f1, f2, alpha, mode="neq", silent = FALSE) {
  if(!check_test_2_prop(n1, n2, f1, f2)) {
    if(!silent) {
      print("Không đủ điều kiện áp dụng test thống kê")
    }
    return()
  }
  f <- (f1*n1 + f2*n2) / (n1+n2)
  test <- (f1 - f2) / sqrt(f*(1-f)*(1/n1 + 1/n2))
  c <- switch(
    mode,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  if(!silent) {
    print("Bài toán: So sánh 2 tỷ lệ")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    case_1 <- mode == "less" && f1 > f2
    case_2 <- mode == "greater" && f1 < f2
    if(case_1 || case_2) {
      print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
    } else {
      get_test_result(test, c)
    }
  }
  return(list(test = test, c = c, f = f))
}

#' Hàm này đã deprecated, thay bằng hàm test_k_prop.
#' Hàm này thưc hiện so sánh n tỷ lệ.
#' m_i, n_i là các vector thể hiện số quan sát có đặc tính A
#' nào đó và tổng số quan sát trong các tập tổng thể.
#' Hàm trả về kết quả của test thống kê (test), giá trị c và các giá trị m (sum_m_i),
#' l (sum_l_i), N (sum_n_i)
#' @export
test_n_prop <- function(m_i, n_i, alpha, silent = FALSE) {
  .Deprecated("test_k_prop")
  if(!check_test_k_prop(m_i, n_i)) {
    if(!silent) {
      print("Không đủ điều kiện áp dụng test thống kê")
    }
    return()
  }
  sum_n_i <- sum(n_i)
  sum_m_i <- sum(m_i)
  sum_l_i <- sum_n_i - sum_m_i
  test <- (sum_n_i * sum_n_i) / (sum_m_i * sum_l_i) * sum(m_i * m_i / n_i) - sum_n_i * sum_m_i / sum_l_i
  c <- qchisq(1 - alpha, df=length(m_i) - 1)
  if(!silent) {
    print("Bài toán: So sánh n tỷ lệ")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    get_test_result(test, c)
  }
  return(list(test = test, c = c, sum_n_i = sum_n_i,
              sum_m_i = sum_m_i, sum_l_i = sum_l_i))
}

#' Hàm này thưc hiện so sánh n tỷ lệ.
#' m_i, n_i là các vector thể hiện số quan sát có đặc tính A
#' nào đó và tổng số quan sát trong các tập tổng thể.
#' Hàm trả về kết quả của test thống kê (test), giá trị c và các giá trị m (sum_m_i),
#' l (sum_l_i), N (sum_n_i)
#' @export
test_k_prop <- function (m_i, n_i, alpha, silent = FALSE) {
  if(!check_test_k_prop(m_i, n_i)) {
    if(!silent) {
      print("Không đủ điều kiện áp dụng test thống kê")
    }
    return()
  }
  sum_n_i <- sum(n_i)
  sum_m_i <- sum(m_i)
  sum_l_i <- sum_n_i - sum_m_i
  test <- (sum_n_i * sum_n_i) / (sum_m_i * sum_l_i) * sum(m_i * m_i / n_i) - sum_n_i * sum_m_i / sum_l_i
  c <- qchisq(1 - alpha, df=length(m_i) - 1)
  if(!silent) {
    print("Bài toán: So sánh n tỷ lệ")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    get_test_result(test, c)
  }
  return(list(test = test, c = c, sum_n_i = sum_n_i,
              sum_m_i = sum_m_i, sum_l_i = sum_l_i))
}

#' Hàm này thực hiện kiểm định tính độc lập của 2 dấu hiệu A và B
#' matrix là một ma trận với các phần tử n_ij nằm trong bảng liên hợp các dấu hiệu
#' (Contingency Table)
#' Hàm trả về kết quả của test thống kê (test), giá trị c
#' cùng các kết quả row_sums, col_sums và n (kích thước mẫu)
#' @export
test_independent <- function(matrix, alpha, silent = FALSE) {
  if(!check_test_independent(matrix)) {
    if(!silent) {
      print("Không đủ điều kiện áp dụng test thống kê")
    }
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
  if(!silent) {
    print("Bài toán: Kiểm định tính độc lập")
    printf("Kết quả test thống kê: %.4f", test)
    printf("Kết quả của c: %.4f", c)
    get_test_result(test, c)
  }
  return(list(test = test, c = c,
         row_sums = row_sums, col_sums = col_sums, n = n))
}

#' Hàm này in ra kết quả của bài toán kiểm định giả thiết
get_test_result <- function(test, c) {
  if(abs(test) > c) {
    print("Kết luận: Bác bỏ H0")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}
