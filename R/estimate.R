#' Ước lượng cho giá trị trung bình (phân bố chuẩn)
#'
#' Hàm này dùng để tính ước lượng khoảng cho giá trị trung bình, dùng phân bố chuẩn tắc
#' Hàm trả về các giá trị z_alpha, z_alpha_div_2 (z_alpha/2), bottom, top (khoảng tin cậy 2 phía), min (khoảng tin cậy nhỏ nhất), max (khoảng tin cậy lớn nhất)
#' alternative có 3 chế độ: two_sided, min và max tương ứng với khoảng tin cậy hai phía, lớn nhất và nhả nhất
#' @export
#' @importFrom checkmate assert_choice
estimate_mean_norm <- function(n, mean, sigma, alpha, alternative="two_sided", silent = FALSE) {
  assert_choice(alternative, c("two_sided", "min", "max"))

  z_alpha_div_2 <- qnorm(1-alpha/2)
  z_alpha <- qnorm(1-alpha)
  eps_2 <- z_alpha_div_2 * sigma / sqrt(n)
  eps <- z_alpha * sigma / sqrt(n)
  top <- mean + eps_2
  bottom <- mean - eps_2
  min <- mean - eps
  max <- mean + eps
  if (!silent) {
    print("Bài toán: Ước lượng khoảng cho trung bình (phân bố chuẩn)")
    print("Input")
    print_huxtable(data.frame(n = n, mean = mean, sigma = sigma, alpha = alpha, alternative = alternative))
    print("Output")
    if (alternative == "two_sided") {
      printf("Khoảng tin cậy hai phía là: (%.4f; %.4f)", bottom, top)
    } else if (alternative == "min") {
      printf("Khoảng tin cậy nhỏ nhất là: (%.4f; +inf)", min)
    } else if (alternative == "max") {
      printf("Khoảng tin cậy lớn nhất là: (-inf; %.4f)", max)
    }
  }

  invisible(list(
    input_data = list(
      n = n, mean = mean, sigma = sigma,
      alpha = alpha, alternative = alternative
    ),
    output_data = list(
      z_alpha = z_alpha, z_alpha_div_2 = z_alpha_div_2,
      top = top, bottom = bottom,
      min = min, max = max
    )
  ))
}

#' Ước lượng cho giá trị trung bình (phân bố Student)
#'
#' Hàm này dùng để tính ước lượng khoảng cho giá trị trung bình, dùng phân bố Student
#' Hàm trả về các giá trị z_alpha, z_alpha_div_2 (z_alpha/2), bottom, top (khoảng tin cậy 2 phía), min (khoảng tin cậy nhỏ nhất), max (khoảng tin cậy lớn nhất)
#' alternative có 3 chế độ: two_sided, min và max tương ứng với khoảng tin cậy hai phía, lớn nhất và nhả nhất
#' @export
#' @importFrom checkmate assert_choice
estimate_mean_t <- function(n, mean, s, alpha, alternative = "two_sided", silent = FALSE) {
  assert_choice(alternative, c("two_sided", "min", "max"))

  t_alpha_div_2 <- qt(1 - alpha/2,df = n - 1)
  t_alpha <- qt(1-alpha, df = n - 1)
  eps_2 <- t_alpha_div_2 * s / sqrt(n)
  eps <- t_alpha * s / sqrt(n)
  top <- mean + eps_2
  bottom <- mean - eps_2
  min <- mean - eps
  max <- mean + eps
  if(!silent) {
    print("Bài toán: Ước lượng khoảng cho trung bình (phân bố Student)")
    print("Input")
    print_huxtable(data.frame(n = n, mean = mean, s = s, alpha = alpha, alternative = alternative))
    print("Output")
    if (alternative == "two_sided") {
      printf("Khoảng tin cậy hai phía là: (%.4f; %.4f)", bottom, top)
    } else if (alternative == "max") {
      printf("Khoảng tin cậy lớn nhất là: (-inf; %.4f)", max)
    } else if (alternative == "min") {
      printf("Khoảng tin cậy nhỏ nhất là: (%.4f; +inf)", min)
    }
  }
  invisible(list(
    input_data = list(
      n = n, mean = mean, s = s,
      alpha = alpha, alternative = alternative
    ),
    output_data = list(
      t_alpha = t_alpha, t_alpha_div_2 = t_alpha_div_2,
      top = top, bottom = bottom,
      min = min, max = max
    )
  ))
}

#' Ước lượng khoảng cho phương sai
#'
#' Hàm này ước lượng khoảng cho phương sai.
#' Hàm trả về các giá trị chi_sq_1, chi_sq_2, chi_sq_1_2 (chisq(1-a/2)), chi_sq_2_2 (chisq(a/2)), bottom, top, min, max.
#' alternative có 3 chế độ: two_sided, min và max tương ứng với khoảng tin cậy hai phía, lớn nhất và nhả nhất
#' @export
#' @importFrom checkmate assert_choice
estimate_var <- function(n, s, alpha, alternative = "two_sided", silent = FALSE) {
  assert_choice(alternative, c("two_sided", "min", "max"))

  chi_sq_2_div_2 <- qchisq(alpha / 2, df=n-1)
  chi_sq_1_div_2 <- qchisq(1 - alpha / 2, df=n-1)
  chi_sq_2 <- qchisq(alpha, df=n-1)
  chi_sq_1 <- qchisq(1 - alpha, df=n-1)
  bottom <- (n-1) * s * s / chi_sq_1_div_2
  top <- (n-1) * s * s / chi_sq_2_div_2
  min <- (n-1) * s * s / chi_sq_1
  max <- (n-1) * s * s / chi_sq_2
  if(!silent) {
    print("Bài toán: Ước lượng khoảng cho phương sai")
    print("Input")
    print_huxtable(data.frame(n = n, s = s, alpha = alpha, alternative = alternative))
    print("Output")
    if (alternative == "two_sided") {
      printf("Khoảng tin cậy hai phía là: (%.4f; %.4f)", bottom, top)
    } else if (alternative == "max") {
      printf("Khoảng tin cậy lớn nhất là: (0; %.4f)", max)
    } else if (alternative == "min") {
      printf("Khoảng tin cậy nhỏ nhất là: (%.4f; +inf)", min)
    }
  }
  invisible(list(
    input_data = list(
      n = n, s = s, alpha = alpha, alternative = alternative
    ),
    output_data = list(
      hi_sq_2 = chi_sq_2, chi_sq_1 = chi_sq_1,
      chi_sq_2_div_2 = chi_sq_2_div_2, chi_sq_1_div_2 = chi_sq_1_div_2,
      bottom = bottom, top = top,
      min = min, max = max
    )
  ))
}

#' Ước lượng khoảng cho tỷ lệ.
#'
#' Hàm này ước lượng khoảng cho tỷ lệ
#' Hàm trả về các giá trị z_alpha, z_alpha_div_2 (z_alpha/2), bottom, top (khoảng tin cậy 2 phía), min (khoảng tin cậy nhỏ nhất), max (khoảng tin cậy lớn nhất)
#' alternative có 3 chế độ: two_sided, min và max tương ứng với khoảng tin cậy hai phía, lớn nhất và nhả nhất
#' @export
#' @importFrom checkmate assert_choice
estimate_prop <- function(n, f, alpha, alternative = "two_sided", silent = FALSE) {
  if(!check_estimate_prop(n, f)) {
    if(!silent) {
      print("Không đủ điều kiện áp dụng test thống kê")
    }
    return()
  }
  assert_choice(alternative, c("two_sided", "min", "max"))

  z_alpha_div_2 <- qnorm(1-alpha/2)
  z_alpha <- qnorm(1-alpha)
  eps_2 <- z_alpha_div_2 * sqrt(f * (1-f) / n)
  eps <- z_alpha * sqrt(f * (1-f) / n)
  bottom <- f - eps_2
  top <- f + eps_2
  min <- f - eps
  max <- f + eps
  if(!silent) {
    print("Bài toán: Ước lượng khoảng cho tỷ lệ")
    print("Input")
    print_huxtable(data.frame(n = n, f = f, alpha = alpha, alternative = alternative))
    print("Output")
    if(alternative == "two_sided") {
      printf("Khoảng tin cậy hai phía là: (%.4f; %.4f)", bottom, top)
    } else if(alternative == "max") {
      printf("Khoảng tin cậy lớn nhất là: (0; %.4f)", max)
    } else if(alternative == "min") {
      printf("Khoảng tin cậy nhỏ nhất là: (%.4f; 1)", min)
    }
  }
  invisible(list(
    input_data = list(
      n = n, f = f, alpha = alpha, alternative = alternative
    ),
    output_data = list(
      z_alpha = z_alpha, z_alpha_div_2 = z_alpha_div_2,
      bottom = bottom, top = top,
      min = min, max = max
    )
  ))
}

#' Xác định kích thước mấu (TH ước lượng cho trung bình)
#'
#' Hàm này xác định kích thước mẫu cho trường hợp ước lượng giá trị trung bình.
#' Hàm trả về các giá trị z_alpha, value - kết quả của phép tính
#' @export
sample_size_mean <- function(sigma, eps, alpha, silent = FALSE) {
  z_alpha <- qnorm(1-alpha/2)
  value <- (sigma*z_alpha / eps) * (sigma*z_alpha / eps)
  if(!silent) {
    print("Bài toán: Xác định kích thước mẫu (ước lượng trung bình)")
    print("Input")
    print_huxtable(data.frame(sigma = sigma, eps = eps, alpha = alpha))
    print("Output")
    printf("Kích thước mẫu tối thiểu: %.4f", value)
  }
  invisible(list(
    input_data = list(
      sigma = sigma, eps = eps, alpha = alpha
    ),
    output_data = list(
      z_alpha = z_alpha, value = value
    )
  ))
}

#' xác đỉnh kích thước mẫu (TH ước lượng tỷ lệ, công thức 1)
#'
#' Hàm này xác đỉnh kích thước mẫu cho trường hợp ước lượng tỷ lệ (công thức 1)
#' Hàm trả về các giá trị z_alpha, value - kết quả của phép tính
#' @export
sample_size_prop_1 <- function(f, eps, alpha, silent = FALSE) {
  z_alpha <- qnorm(1-alpha/2)
  value <- z_alpha*z_alpha * f*(1 - f) / (eps*eps)
  if(!silent) {
    print("Bài toán: Xác định kích thước mẫu (ước lượng tỷ lệ, đã biết f)")
    print("Input")
    print_huxtable(data.frame(f = f, eps = eps, alpha = alpha))
    print("Output")
    printf("Kích thước mẫu tối thiểu: %.4f", value)
  }
  invisible(list(
    input_data = list(
      f = f, eps = eps, alpha = alpha
    ),
    output_data = list(
      z_alpha = z_alpha, value = value
    )
  ))
}

#' xác đỉnh kích thước mẫu (TH ước lượng tỷ lệ, công thức 2)
#'
#' Hàm này xác định kích thước mẫu cho trường hợp ước lượng tỷ lệ (công thức 2)
#' Hàm trả về các giá trị z_alpha, value - kết quả của phép tính
#' @export
sample_size_prop_2 <- function(eps, alpha, silent = FALSE) {
  z_alpha <- qnorm(1-alpha/2)
  value <- z_alpha*z_alpha / (4*eps*eps)
  if(!silent) {
    print("Bài toán: Xác định kích thước mẫu (ước lượng tỷ lệ, chưa biết f)")
    print("Input")
    print_huxtable(data.frame(eps = eps, alpha = alpha))
    print("Output")
    printf("Kích thước mẫu tối thiểu: %.4f", value)
  }
  invisible(list(
    input_data = list(
      eps = eps, alpha = alpha
    ),
    output_data = list(
      z_alpha = z_alpha, value = value
    )
  ))
}
