#' Hàm này tính và trả về giá trị hệ số tương quan.
#' @export
correlation <- function(x, y) {
  print("Bài toán: Tính hệ số tương quan")
  cor <- cor(x, y, method = "pearson")
  printf("Hệ số tương quan: %.4f", cor)
  return(cor)
}

#' Hàm này thực hiện bài toán hồi quy tuyến tính đơn và trả về các giá trị a, b (y = ax+b)
#' @export
linear_regression <- function(x, y) {
  df <- data.frame(X = x, Y = y)
  result <- lm(Y ~ X, data = df)
  print("Bài toán: Bài toán hồi quy tuyến tính đơn")
  printf("Hệ số tự do: %.4f", result$coefficients[1])
  printf("Hệ số ứng với x: %.4f", result$coefficients[2])
  return(list(a = result$coefficients[2], b = result$coefficients[1]))
}

#' Hàm này thực hiện tính và trả về kết quả dự đoán giá trị
#' trong bài toán hồi quy tuyến tính đơn.
#' @export
linear_regression_predict <- function(x, y, value) {
  df <- data.frame(X = x, Y = y)
  result <- lm(Y ~ X, data = df)
  predict_value <- result$coefficients[1] + value * result$coefficients[2]
  printf("Bài toán: Dự báo giá trị, dựa vào hồi quy tuyến tính đơn")
  printf("Giá trị của Y là: %.4f", value)
  return(predict_value)
}

