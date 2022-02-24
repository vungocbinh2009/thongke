#' Tạo dữ liệu rời rạc.
#'
#' Hàm này tạo dữ liệu giả theo phân bố chuẩn từ min đến max
#' Hàm sẽ in ra bảng phân bố tần số và trả về dữ liệu được hàm tạo ra.
#' Lưu ý: dữ liệu chỉ cho giá trị mean và sd gần đúng và không bằng giá trị mean và sd truyền vào hàm
#' @export
#' @importFrom truncnorm rtruncnorm
data_simulate_discrete <- function(n, mean, sd, min, max, round_digits = 0, silent=FALSE) {
  data <- rtruncnorm(n, a=min, b=max, mean, sd)
  data <- round(data, digits = round_digits)
  df <- data.frame(data1 = data)
  if(!silent) {
    print(table(df$data1))
  }
  return(data)
}

#' Tạo dữ liệu liên tục.
#'
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

#' Tạo dữ liệu cho bài toán hồi quy tuyến tính
#'
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

#' Dữ liệu cho bài toán kiểm định khi bình phương.
#'
#' Hàm này dùng để tạo dữ liệu cho bài toán kiểm định sự phù hợp của k tỷ lệ
#' @export
data_simulate_test_goodness_of_fit <- function (expected, silent=FALSE) {
  size <- sum(expected)
  data <- sample(seq_along(expected), size, prob = expected, replace = TRUE)
  freq <- as.integer(table(data))
  if(!silent) {
    print(freq)
  }
  return(freq)
}

#' Dữ liệu cho bài toán kiểm định k tỷ lệ.
#'
#' Hàm này dùng để tạo dữ liệu cho bài toán so sánh k tỷ lệ
#' @export
data_simulate_test_k_prop <- function (expected_m_i, expected_l_i, silent=FALSE) {
  expected <- c(expected_m_i, expected_l_i)
  size <- sum(expected)
  data <- sample(seq_along(expected), size, prob = expected, replace = TRUE)
  freq <- as.integer(table(data))
  matrix <- matrix(freq, nrow = 2, ncol = length(expected_m_i), byrow = TRUE)
  if(!silent) {
    print(matrix)
  }
  return(matrix)
}

#' Dữ liệu cho bài toán kiểm định tính độc lập.
#'
#' Hàm này dùng để tạo dữ liệu cho bài toán kiểm định tính độc lập
#' @export
data_simulate_test_independent <- function (expected_matrix, silent=FALSE) {
  vector <- as.vector(expected_matrix)
  size <- sum(vector)
  data <- sample(seq_along(vector), size, prob = vector, replace = TRUE)
  freq <- as.integer(table(data))
  matrix <- matrix(freq, nrow=dim(expected_matrix)[1],
                   ncol = dim(expected_matrix)[2], byrow = FALSE)
  if(!silent) {
    print(matrix)
  }
  return(matrix)
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
