#' Kiểm định giả thiết về giá trị trung bình (phân bố chuẩn)
#'
#' Hàm này kiểm định giả thiết về gái trị trung bình, dùng phân bố chuẩn
#' Tham số alternative là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
#' @importFrom checkmate assert_choice
test_mean_norm <- function(n, mean, mean_0, sigma, alpha, alternative="neq", silent = FALSE) {
  assert_choice(alternative, c("neq", "less", "greater"))

  test <- (mean - mean_0) * sqrt(n) / sigma
  c <- switch(
    alternative,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  case_1 <- alternative == "less" && mean > mean_0
  case_2 <- alternative == "greater" && mean < mean_0
  if (case_1 || case_2) {
    rejected <- FALSE
  } else {
    rejected <- abs(test) > c
  }
  if(!silent) {
    print("Bài toán: Kiểm định giả thiết cho giá trị trung bình (phân bố chuẩn)")
    print("Input")
    print_huxtable(
      data.frame(n = n, mean = mean, mean_0 = mean_0,
                 sigma = sigma, alpha = alpha, alternative = alternative
      )
    )
    print("Output")
    print_huxtable(data.frame(T = test, c = c))
    print_test_result(rejected)
  }
  invisible(list(
    input_data = list(
      n = n, mean = mean, mean_0 = mean_0,
      sigma = sigma, alpha = alpha, alternative = alternative
    ),
    output_data = list(
      test = test, c = c, rejected = rejected
    )
  ))
}

#' Kiểm định giả thiết về giá trị trung bình (phân bố Student)
#'
#' Hàm này kiểm định giả thiết về gái trị trung bình, dùng phân bố student
#' Tham số alternative là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
#' @importFrom checkmate assert_choice
test_mean_t <- function(n, mean, mean_0, s, alpha, alternative="neq", silent = FALSE) {
  assert_choice(alternative, c("neq", "less", "greater"))

  test <- (mean - mean_0) * sqrt(n) / s
  c <- switch(
    alternative,
    "neq" = qt(1 - alpha / 2, df=n-1),
    "less" = qt(1 - alpha, df=n-1),
    "greater" = qt(1 - alpha, df=n-1)
  )
  case_1 <- alternative == "less" && mean > mean_0
  case_2 <- alternative == "greater" && mean < mean_0
  if (case_1 || case_2) {
    rejected <- FALSE
  } else {
    rejected <- abs(test) > c
  }
  if(!silent) {
    print("Bài toán: Kiểm định giả thiết cho giá trị trung bình (phân bố Student)")
    print("Input")
    print_huxtable(data.frame(n = n, mean = mean, mean_0 = mean_0, s = s, alpha = alpha, alternative = alternative))
    print("Output")
    print_huxtable(data.frame(T = test, c = c))
    print_test_result(rejected)
  }
  invisible(list(
    input_data = list(
      n = n, mean = mean, mean_0 = mean_0, s = s, alpha = alpha, alternative = alternative
    ),
    output_data = list(
      test = test, c = c, rejected = rejected
    )
  ))
}

#' Kiểm định giả thiết về xác suất.
#'
#' Hàm này kiểm định giả thiết về xác suất
#' Tham số alternative là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
#' @importFrom checkmate assert_choice
test_prop <- function(n, f, p_0, alpha, alternative="neq", silent = FALSE) {
  assert_choice(alternative, c("neq", "less", "greater"))

  if(!check_test_prop(n, p_0)) {
    if(!silent) {
      print("Không đủ điều kiện áp dụng test thống kê")
    }
    return()
  }
  test <- (f - p_0) * sqrt(n) / sqrt(p_0 * (1-p_0))
  c <- switch(
    alternative,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  case_1 <- alternative == "less" && f > p_0
  case_2 <- alternative == "greater" && f < p_0
  if (case_1 || case_2) {
    rejected <- FALSE
  } else {
    rejected <- abs(test) > c
  }
  if(!silent) {
    print("Bài toán: Kiểm định giả thiết cho tỷ lệ")
    print("Input")
    print_huxtable(data.frame(n = n, f = f, p_0 = p_0, alpha = alpha, alternative = alternative))
    print("Output")
    print_huxtable(data.frame(T = test, c = c))
    print_test_result(rejected)
  }
  invisible(list(
    input_data = list(
      n = n, f = f, p_0 = p_0, alpha = alpha, alternative = alternative
    ),
    output_data = list(
      test = test, c = c, rejected = rejected
    )
  ))
}

#' Kiểm định sự phù hợp của k tỷ lệ.
#'
#' Hàm này thực hiện kiểm định khi bình phương
#' actual, expect là các vector thể hiện tần số quan sát và tần số lý thuyết
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
test_goodness_of_fit <- function (actual, expected, alpha, silent = FALSE) {
  test <- sum((actual - expected)^2 / expected)
  c <- qchisq(1 - alpha, df=length(actual)-1)
  if(!silent) {
    print("Bài toán: Kiểm định khi bình phương (kiểm định cho k tỷ lệ)")
    print("Input")
    print_huxtable(t(data.frame(actual = actual, expected = expected)))
    printf("alpha = %.2f", alpha)
    print("Output")
    print_huxtable(data.frame(T = test, c = c))
    print_test_result(abs(test) > c)
  }
  invisible(list(
    input_data = list(
      actual = actual, expected = expected, alpha = alpha
    ),
    output_data = list(
      test = test, c = c, rejected = abs(test) > c
    )
  ))
}

#' Kiểm định giả thiết: So sánh 2 giá trị trung bình (phân bố chuẩn)
#'
#' Hàm này so sánh 2 giá trị trung bình (dùng phân bố chuẩn)
#' Tham số alternative là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
#' @importFrom checkmate assert_choice
test_2_mean_norm <- function(n1, n2, mean1, mean2, sigma1, sigma2, alpha, alternative="neq", silent = FALSE) {
  assert_choice(alternative, c("neq", "less", "greater"))

  test <- (mean1 - mean2) / sqrt(sigma1*sigma1/n1 + sigma2*sigma2/n2)
  c <- switch(
    alternative,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  case_1 <- alternative == "less" && mean1 > mean2
  case_2 <- alternative == "greater" && mean1 < mean2
  if (case_1 || case_2) {
    rejected <- FALSE
  } else {
    rejected <- abs(test) > c
  }
  if(!silent) {
    print("Bài toán: So sánh 2 giá trị trung bình (phân bố chuẩn)")
    print("Input")
    print_huxtable(data.frame(n = c(n1, n2), mean = c(mean1, mean2), sigma = c(sigma1, sigma2)))
    printf("alternative = %s, alpha = %.2f", alternative, alpha)
    print("Output")
    print_huxtable(data.frame(T = test, c = c))
    print_test_result(rejected)
  }
  invisible(list(
    input_data = list(
      n = c(n1, n2), mean = c(mean1, mean2), sigma = c(sigma1, sigma2), alternative = alternative, alpha = alpha
    ),
    output_data = list(
      test = test, c = c, rejected = rejected
    )
  ))
}

#' Kiểm định giả thiết: So sánh 2 giá trị trung bình (phân bố Student)
#'
#' Hàm này so sánh 2 giá trị trung bình (dùng phân bố Student)
#' Tham số alternative là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test), giá trị c và phương sai chung s
#' @export
#' @importFrom checkmate assert_choice
test_2_mean_t <- function(n1, n2, mean1, mean2, s1, s2, alpha, alternative="neq", silent = FALSE) {
  assert_choice(alternative, c("neq", "less", "greater"))

  s_squared <- ((n1-1)*s1*s1 + (n2-1)*s2*s2) / (n1+n2-2)
  test <- (mean1 - mean2) / (sqrt(s_squared) * sqrt(1/n1 + 1/n2))
  c <- switch(
    alternative,
    "neq" = qt(1 - alpha / 2, df=n1+n2-2),
    "less" = qt(1 - alpha, df=n1+n2-2),
    "greater" = qt(1 - alpha, df=n1+n2-2)
  )
  case_1 <- alternative == "less" && mean1 > mean2
  case_2 <- alternative == "greater" && mean1 < mean2
  if (case_1 || case_2) {
    rejected <- FALSE
  } else {
    rejected <- abs(test) > c
  }
  if(!silent) {
    print("Bài toán: So sánh 2 giá trị trung bình (phân bố Student)")
    print("Input")
    print_huxtable(data.frame(n = c(n1, n2), mean = c(mean1, mean2), s = c(s1, s2)))
    printf("alternative = %s, alpha = %.2f", alternative, alpha)
    print("Output")
    print_huxtable(data.frame(T = test, c = c))
    print_test_result(rejected)
  }
  invisible(list(
    input_data = list(
      n = c(n1, n2), mean = c(mean1, mean2), s = c(s1, s2), alternative = alternative, alpha = alpha
    ),
    output_data = list(
      test = test, c = c, s_squared = s_squared, rejected = rejected
    )
  ))
}

#' Kiểm định giả thiết: So sánh 2 tỷ lệ.
#'
#' Hàm này thực hiện so sánh 2 tỷ lệ
#' Tham số alternative là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
#' Hàm trả về kết quả của test thống kê (test) và giá trị c và tỷ lệ chung f
#' @export
#' @importFrom checkmate assert_choice
test_2_prop <- function(n1, n2, f1, f2, alpha, alternative="neq", silent = FALSE) {
  assert_choice(alternative, c("neq", "less", "greater"))

  if(!check_test_2_prop(n1, n2, f1, f2)) {
    if(!silent) {
      print("Không đủ điều kiện áp dụng test thống kê")
    }
    return()
  }
  f <- (f1*n1 + f2*n2) / (n1+n2)
  test <- (f1 - f2) / sqrt(f*(1-f)*(1/n1 + 1/n2))
  c <- switch(
    alternative,
    "neq" = qnorm(1 - alpha / 2),
    "less" = qnorm(1 - alpha),
    "greater" = qnorm(1 - alpha)
  )
  case_1 <- alternative == "less" && f1 > f2
  case_2 <- alternative == "greater" && f1 < f2
  if (case_1 || case_2) {
    rejected <- FALSE
  } else {
    rejected <- abs(test) > c
  }
  if(!silent) {
    print("Bài toán: So sánh 2 tỷ lệ")
    print("Input")
    print_huxtable(data.frame(n = c(n1, n2), f = c(f1, f2)))
    printf("alternative = %s, alpha = %.2f", alternative, alpha)
    print("Output")
    print_huxtable(data.frame(T = test, c = c))
    print_test_result(rejected)
  }
  invisible(list(
    input_data = list(
      n = c(n1, n2), f = c(f1, f2), alternative = alternative, alpha = alpha
    ),
    output_data = list(
      test = test, c = c, f = f, rejected = rejected
    )
  ))
}

#' Kiểm định giả thiết: So sánh k tỷ lệ.
#'
#' Hàm này thưc hiện so sánh k tỷ lệ.
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
    print("Bài toán: So sánh k tỷ lệ")
    print("Input")
    print_huxtable(t(data.frame(m_i = m_i, n_i = n_i)))
    printf("alpha = %.2f", alpha)
    print("Output")
    print_huxtable(data.frame(T = test, c = c))
    print_test_result(abs(test) > c)
  }
  invisible(list(
    input_data = list(
      m_i = m_i, n_i = n_i, alpha = alpha
    ),
    output_data = list(
      test = test, c = c, rejected = abs(test) > c, sum_n_i = sum_n_i,
      sum_m_i = sum_m_i, sum_l_i = sum_l_i
    )
  ))
}

#' KIểm định tính độc lập.
#'
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
    print("Input")
    print_huxtable(matrix)
    printf("alpha = %.2f", alpha)
    print("Output")
    print_huxtable(data.frame(T = test, c = c))
    print_test_result(abs(test) > c)
  }
  invisible(list(
    input_data = list(
      matrix = matrix, alpha = alpha
    ),
    output_data = list(
      test = test, c = c, rejected = abs(test) > c,
      row_sums = row_sums, col_sums = col_sums, n = n
    )
  ))
}

#' Hàm này in kết luận của bài toán kiểm định giả thiết.
print_test_result <- function (rejected) {
  if(rejected) {
    print("Kết luận: Bác bỏ H0, chấp nhận H1")
  } else {
    print("Kết luận: Chưa đủ cơ sở để bác bỏ H0")
  }
}
