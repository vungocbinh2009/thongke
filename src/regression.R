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

