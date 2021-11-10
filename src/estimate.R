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
  if(!check_estimate_prop(n, f)) {
    print("Không đủ điều kiện áp dụng test thống kê")
    return()
  }

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

