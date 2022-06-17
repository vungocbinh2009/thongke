#' Tính hệ số tương quan.
#'
#' Hàm này tính và trả về giá trị hệ số tương quan.
#' @export
correlation <- function(x, y, silent = FALSE) {
  cor <- cor(x, y, method = "pearson")
  if(!silent) {
    print("Bài toán: Tính hệ số tương quan")
    print("Input")
    str(data.frame(x, y))
    printf("Hệ số tương quan: %.4f", cor)
  }
  invisible(list(
    input_data = list(
      x = x, y = y
    ),
    output_data = list(
      cor = cor
    )
  ))
}

#' Bài toán hồi quy tuyến tính đơn.
#'
#' Hàm này thực hiện bài toán hồi quy tuyến tính đơn và
#' trả về các giá trị a, b (y = ax+b)
#' @export
linear_regression <- function(x, y, silent = FALSE) {
  df <- data.frame(X = x, Y = y)
  result <- lm(Y ~ X, data = df)
  if(!silent) {
    print("Bài toán: Bài toán hồi quy tuyến tính đơn")
    print("Input")
    str(data.frame(x, y))
    printf("Hệ số tự do: %.4f", result$coefficients[1])
    printf("Hệ số ứng với x: %.4f", result$coefficients[2])
  }
  invisible(list(
    input_data = list(
      x = x, y = y
    ),
    output_data = list(
      a = result$coefficients[2], b = result$coefficients[1]
    )
  ))
}

#' Dự đoán giá trị cho bài toán hồi quy tuyến tính đơn.
#'
#' Hàm này thực hiện tính và trả về kết quả dự đoán giá trị
#' trong bài toán hồi quy tuyến tính đơn.
#' @export
linear_regression_predict <- function(x, y, value, silent = FALSE) {
  df <- data.frame(X = x, Y = y)
  result <- lm(Y ~ X, data = df)
  predict_value <- result$coefficients[1] + value * result$coefficients[2]
  if(!silent) {
    printf("Bài toán: Dự báo giá trị, dựa vào hồi quy tuyến tính đơn")
    print("Input")
    str(data.frame(x, y))
    printf("Value = %.2f", value)
    printf("Giá trị của Y là: %.4f", predict_value)
  }
  invisible(list(
    input_data = list(
      x = x, y = y, value = value
    ),
    output_data = list(
      predict_value = predict_value
    )
  ))
}

#' Tính các tổng hay dùng giữa x và y.
#'
#' Hàm này tính tất cả các giá trị tổng giữa x và y
#' @export
calculate_sum <- function (x, y, silent=FALSE) {
  sum_x <- sum(x)
  sum_y <- sum(y)
  sum_x2 <- sum(x*x)
  sum_y2 <- sum(y*y)
  sum_xy <- sum(x*y)
  if(!silent) {
    print("Input")
    print_huxtable(t(data.frame(x = x, y = y)))
    print("Output")
    print_huxtable(data.frame(sum_x = sum_x, sum_y = sum_y, sum_xy = sum_xy, sum_x2 = sum_x2, sum_y2 = sum_y2))
  }
  invisible(list(
    input_data = list(
      x = x, y = y
    ),
    output_data = list(
      sum_x = sum_x, sum_y = sum_y, sum_xy = sum_xy,
      sum_x2 = sum_x2, sum_y2 = sum_y2
    )
  ))
  invisible(list(
    sum_x = sum_x,
    sum_y = sum_y,
    sum_xy = sum_xy,
    sum_x2 = sum_x2,
    sum_y2 = sum_y2
  ))
}
