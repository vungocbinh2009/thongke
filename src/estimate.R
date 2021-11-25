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
