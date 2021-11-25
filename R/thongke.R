#' Hàm này tạo dữ liệu giả theo phân bố chuẩn từ min đến max
#' Hàm sẽ in ra bảng phân bố tần số và trả về dữ liệu được hàm tạo ra.
#' Lưu ý: dữ liệu chỉ cho giá trị mean và sd gần đúng và không bằng giá trị mean và sd truyền vào hàm
#' @export
#' @importFrom truncnorm rtruncnorm
data_simulate_discrete <- function(n, mean, sd, min, max, silent=FALSE) {
  data <- rtruncnorm(n, a=min, b=max, mean, sd)
  data <- round(data)
  df <- data.frame(data1 = data)
  if(!silent) {
    print(table(df$data1))
  }
  return(data)
}

#' Hàm này tạo dữ liệu giả theo phân bố chuẩn từ min đến max
#' Hàm sẽ in ra bảng phân bố tần số ghép lớp, theo các giá trị cut cho trước
#' Hàm cũng trả về dữ liệu dưới dạng ghép lớp (điểm giữa của các khoảng)
#' Lưu ý: dữ liệu chỉ cho giá trị mean và sd gần đúng và không bằng giá trị mean và sd truyền vào hàm
#' @export
#' @importFrom truncnorm rtruncnorm
data_simulate_continuous <- function(n, mean, sd, min, max, size, silent=FALSE) {
  data <- rtruncnorm(n, a=min, b=max, mean, sd)
  df <- data.frame(data = data)
  cut_vector <- get_cut_vector(min, max, size)
  df$data.cut <- cut(df$data, breaks=cut_vector)
  df$cal_data <- as.numeric(df$data.cut) * size + (min - size / 2)
  if(!silent) {
    print(with(df, table(data.cut, useNA='ifany')))
  }
  return(df$cal_data)
}

#' Hàm này tạo dữ liệu giả để xây dựng bài toán hồi quy tuyến tính đơn
#' Lưu ý: Các hệ số hồi quy tuyến tính truyền vào không phải các hệ số hồi quy tuyến tính cuối cùng
#' Hàm này trả về các giá trị x, y, có thể dùng dưới dạng data$x, data$y
#' @export
data_simulate_regression <- function(n, min_x, max_x, b0, b1, sd_eps, round_digits, silent=FALSE) {
  # Tạo ngẫu nhiên dữ liệu cho các biến x_1, x_2
  x <- runif(n, min_x, max_x)

  # Tạo các tham số b0, b1, b2 cũng như sai số eps (theo phân bố chuẩn với sigma cố định)
  b0 <- b0
  b1 <- b1

  eps <- rnorm(n, mean = 0, sd = sd_eps)
  y <- b0 + b1 * x + eps

  x <- round(x, digits = round_digits)
  y <- round(y, digits = round_digits)

  df <- data.frame(x = x, y = y)
  df <- df[order(x),]
  if(!silent) {
    print(df)
  }
  return(list(x = x, y = y))
}

#' Hàm này xây dựng cut_vector cho hàm data_simulate_regression
get_cut_vector <- function(min, max, size) {
  groups <- (max - min) / size
  cut_vector <- min
  for(i in 1:groups) {
    cut_vector <- c(cut_vector, min + i*size)
  }
  return(cut_vector)
}


#' Hàm này dùng để tính ước lượng khoảng cho giá trị trung bình, dùng phân bố chuẩn tắc
#' Hàm trả về các giá trị z_alpha, bottom, top
#' @export
estimate_mean_norm <- function(n, mean, sigma, alpha, silent = FALSE) {
  z_alpha <- qnorm(1-alpha/2)
  eps <- z_alpha * sigma / sqrt(n)
  top <- mean + eps
  bottom <- mean - eps
  if (!silent) {
    print("Bài toán: Ước lượng khoảng cho trung bình (phân bố chuẩn)")
    printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
  }
  return(list(z_alpha = z_alpha, top = top, bottom = bottom))
}

#' Hàm này dùng để tính ước lượng khoảng cho giá trị trung bình, dùng phân bố Student
#' #' Hàm trả về các giá trị t_alpha, bottom, top
#' @export
estimate_mean_t <- function(n, mean, s, alpha, silent = FALSE) {
  t_alpha <- qt(1 - alpha/2,df = n - 1)
  eps <- t_alpha * s / sqrt(n)
  top <- mean + eps
  bottom <- mean - eps
  if(!silent) {
    print("Bài toán: Ước lượng khoảng cho trung bình (phân bố Student)")
    printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
  }
  return(list(t_alpha = t_alpha, top = top, bottom = bottom))
}

#' Hàm này ước lượng khoảng cho phương sai.
#' Hàm trả về các giá trị chi_sq_1, chi_sq_2,, bottom, top
#' @export
estimate_var <- function(n, s, alpha, silent = FALSE) {
  chi_sq_1 <- qchisq(alpha / 2, df=n-1)
  chi_sq_2 <- qchisq(1 - alpha / 2, df=n-1)
  bottom <- (n-1) * s * s / chi_sq_2
  top <- (n-1) * s * s / chi_sq_1
  if(!silent) {
    print("Bài toán: Ước lượng khoảng cho phương sai")
    printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
  }
  return(list(chi_sq_1 = chi_sq_1, chi_sq_2 = chi_sq_2,
              bottom = bottom, top = top))
}

#' Hàm này ước lượng khoảng cho tỷ lệ
#' Hàm trả về các giá trị z_alpha, bottom, top
#' @export
estimate_prop <- function(n, f, alpha, silent = FALSE) {
  if(!check_estimate_prop(n, f)) {
    if(!silent) {
      print("Không đủ điều kiện áp dụng test thống kê")
    }
    return()
  }

  z_alpha <- qnorm(1-alpha/2)
  eps <- z_alpha * sqrt(f * (1-f) / n)
  bottom <- f - eps
  top <- f + eps
  if(!silent) {
    print("Bài toán: Ước lượng khoảng cho tỷ lệ")
    printf("Khoảng tin cậy cần tìm là: (%.4f; %.4f)", bottom, top)
  }
  return(list(z_alpha = z_alpha, bottom = bottom, top = top))
}

#' Hàm này xác định kích thước mẫu cho trường hợp ước lượng giá trị trung bình.
#' Hàm trả về các giá trị z_alpha, value - kết quả của phép tính
#' @export
sample_size_mean <- function(sigma, eps, alpha, silent = FALSE) {
  z_alpha <- qnorm(1-alpha/2)
  value <- (sigma*z_alpha / eps) * (sigma*z_alpha / eps)
  if(!silent) {
    print("Bài toán: Xác định kích thước mẫu (ước lượng trung bình)")
    printf("Kích thước mẫu tối thiểu: %.4f", value)
  }
  return(list(z_alpha = z_alpha, value = value))
}

#' Hàm này xác đỉnh kích thước mẫu cho trường hợp ước lượng tỷ lệ (công thức 1)
#' Hàm trả về các giá trị z_alpha, value - kết quả của phép tính
#' @export
sample_size_prop_1 <- function(f, eps, alpha, silent = FALSE) {
  z_alpha <- qnorm(1-alpha/2)
  value <- z_alpha*z_alpha * f*(1 - f) / (eps*eps)
  if(!silent) {
    print("Bài toán: Xác định kích thước mẫu (ước lượng tỷ lệ, đã biết f)")
    printf("Kích thước mẫu tối thiểu: %.4f", value)
  }
  return(list(z_alpha = z_alpha, value = value))
}

#' Hàm này xác định kích thước mẫu cho trường hợp ước lượng tỷ lệ (công thức 2)
#' Hàm trả về các giá trị z_alpha, value - kết quả của phép tính
#' @export
sample_size_prop_2 <- function(eps, alpha, silent = FALSE) {
  z_alpha <- qnorm(1-alpha/2)
  value <- z_alpha*z_alpha / (4*eps*eps)
  if(!silent) {
    print("Bài toán: Xác định kích thước mẫu (ước lượng tỷ lệ, chưa biết f)")
    printf("Kích thước mẫu tối thiểu: %.4f", value)
  }
  return(list(z_alpha = z_alpha, value = value))
}


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
check_test_n_prop <- function(m_i, n_i) {
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
    get_test_result(test, c)
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
    get_test_result(test, c)
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
    get_test_result(test, c)
  }
  return(list(test = test, c = c))
}

#' Hàm này thực hiện kiểm định khi bình phương
#' actual, expect là các vector thể hiện tần số quan sát và tần số lý thuyết
#' Hàm trả về kết quả của test thống kê (test) và giá trị c
#' @export
test_chi_squared <- function(actual, expected, alpha, silent = FALSE) {
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
    get_test_result(test, c)
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
    get_test_result(test, c)
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
    get_test_result(test, c)
  }
  return(list(test = test, c = c, f = f))
}

#' Hàm này thưc hiện so sánh n tỷ lệ.
#' m_i, n_i là các vector thể hiện số quan sát có đặc tính A
#' nào đó và tổng số quan sát trong các tập tổng thể.
#' Hàm trả về kết quả của test thống kê (test), giá trị c và các giá trị m (sum_m_i),
#' l (sum_l_i), N (sum_n_i)
#' @export
test_n_prop <- function(m_i, n_i, alpha, silent = FALSE) {
  if(!check_test_n_prop(m_i, n_i)) {
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


#' Hàm này tính và trả về giá trị hệ số tương quan.
#' @export
correlation <- function(x, y, silent = FALSE) {
  cor <- cor(x, y, method = "pearson")
  if(!silent) {
    print("Bài toán: Tính hệ số tương quan")
    printf("Hệ số tương quan: %.4f", cor)
  }
  return(cor)
}

#' Hàm này thực hiện bài toán hồi quy tuyến tính đơn và trả về các giá trị a, b (y = ax+b)
#' @export
linear_regression <- function(x, y, silent = FALSE) {
  df <- data.frame(X = x, Y = y)
  result <- lm(Y ~ X, data = df)
  if(!silent) {
    print("Bài toán: Bài toán hồi quy tuyến tính đơn")
    printf("Hệ số tự do: %.4f", result$coefficients[1])
    printf("Hệ số ứng với x: %.4f", result$coefficients[2])
  }
  return(list(a = result$coefficients[2], b = result$coefficients[1]))
}

#' Hàm này thực hiện tính và trả về kết quả dự đoán giá trị
#' trong bài toán hồi quy tuyến tính đơn.
#' @export
linear_regression_predict <- function(x, y, value, silent = FALSE) {
  df <- data.frame(X = x, Y = y)
  result <- lm(Y ~ X, data = df)
  predict_value <- result$coefficients[1] + value * result$coefficients[2]
  if(!silent) {
    printf("Bài toán: Dự báo giá trị, dựa vào hồi quy tuyến tính đơn")
    printf("Giá trị của Y là: %.4f", value)
  }
  return(predict_value)
}


#' Hàm này có chức năng tương tự như hàm printf trong C
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


