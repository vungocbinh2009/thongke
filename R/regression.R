#' Tính hệ số tương quan.
#'
#' Hàm này tính và trả về giá trị hệ số tương quan.
#' @export
correlation <- function(x, y, silent = FALSE) {
  cor <- cor(x, y, method = "pearson")
  if(!silent) {
    print("Bài toán: Tính hệ số tương quan")
    printf("Hệ số tương quan: %.4f", cor)
  }
  invisible(cor)
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
    printf("Hệ số tự do: %.4f", result$coefficients[1])
    printf("Hệ số ứng với x: %.4f", result$coefficients[2])
  }
  invisible(list(a = result$coefficients[2], b = result$coefficients[1]))
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
    printf("Giá trị của Y là: %.4f", predict_value)
  }
  invisible(predict_value)
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
    printf("Tổng x: %.4f", sum_x)
    printf("Tổng y: %.4f", sum_y)
    printf("Tổng xy: %.4f", sum_xy)
    printf("Tổng x2: %.4f", sum_x2)
    printf("Tổng y2: %.4f", sum_y2)
  }
  invisible(list(
    sum_x = sum_x,
    sum_y = sum_y,
    sum_xy = sum_xy,
    sum_x2 = sum_x2,
    sum_y2 = sum_y2
  ))
}
