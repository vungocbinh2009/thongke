#' Sinh dữ liệu rời rạc.
#'
#' Hàm này sinh dữ liệu giả theo phân bố chuẩn từ min đến max
#' Hàm sẽ in ra bảng phân bố tần số và trả về dữ liệu được hàm tạo ra.
#' Tham số simplify=TRUE cho phép đơn giản hóa kết quả hàm trả về, chỉ trả về vector dữ liệu.
#' Lưu ý: dữ liệu chỉ cho giá trị mean và sd gần đúng và không bằng
#' giá trị mean và sd truyền vào hàm
#'
#' @return Có 2 trường hợp:
#' * Nếu simplify=FALSE, hàm trả về 1 list gồm input_data, chứa các tham số đã truyền vào,
#' và output_data, có vector data là dữ liệu được sinh ra.
#' * Nếu simplify=TRUE, hàm chỉ trả về vector dữ liệu được sinh ra.
#' @export
#' @importFrom truncnorm rtruncnorm
data_simulate_discrete <- function(n, mean, sd, min, max, round_digits = 0, silent=FALSE, simplify = FALSE) {
  data <- rtruncnorm(n, a=min, b=max, mean, sd)
  data <- round(data, digits = round_digits)
  if(!silent) {
    print("Bài toán: Sinh dữ liệu theo phân bố chuẩn")
    print("Input")
    print_huxtable(data.frame(n = n, mean = mean, sd = sd, min = min, max = max, round_digits = round_digits))
    print("Output")
    print_huxtable(as.data.frame(table(data)))
  }
  if(simplify) {
    invisible(data)
  } else {
    invisible(list(
      input_data = list(
        n = n, mean = mean, sd = sd,
        min = min, max = max, round_digits = round_digits
      ),
      output_data = list(
        data = data
      )
    ))
  }

}

#' Sinh dữ liệu liên tục.
#'
#' Hàm này sinh dữ liệu giả theo phân bố chuẩn từ min đến max
#' Hàm sẽ in ra bảng phân bố tần số ghép lớp, theo các giá trị cut cho trước
#' Hàm cũng trả về dữ liệu dưới dạng ghép lớp (điểm giữa của các khoảng)
#' Tham số simplify=TRUE cho phép hàm trả về dữ liệu đơn giản hơn, nếu muốn.
#' Lưu ý: dữ liệu chỉ cho giá trị mean và sd gần đúng và không bằng giá trị mean và sd
#' truyền vào hàm
#'
#' @return Có 2 trường hợp.
#' * Nếu simplify=FALSE, hàm trả về 1 list gồm input_data, chứa các tham số đã truyền vào,
#' và output_data, có vector data là dữ liệu được sinh ra.
#' * Nếu simplify=TRUE, hàm chỉ trả về vector dữ liệu được sinh ra.
#' @export
#' @importFrom truncnorm rtruncnorm
data_simulate_continuous <- function(n, mean, sd, min, max, size, silent=FALSE, simplify = FALSE) {
  data <- rtruncnorm(n, a=min, b=max, mean, sd)
  df <- data.frame(data = data)
  cut_vector <- get_cut_vector(min, max, size)
  df$data.cut <- cut(df$data, breaks=cut_vector)
  df$cal_data <- as.numeric(df$data.cut) * size + (min - size / 2)
  if(!silent) {
    print("Bài toán: Sinh dữ liệu ngẫu nhiên")
    print("Input")
    print_huxtable(data.frame(n = n, mean = mean, sd = sd, min = min, max = max, size = size))
    print("Output")
    print_huxtable(with(df, table(data.cut, useNA='ifany')))
  }
  if(simplify) {
    invisible(df$cal_data)
  } else {
    invisible(list(
      input_data = list(
        n = n, mean = mean, sd = sd, min = min, max = max, size = size
      ),
      output_data = list(
        data = df$cal_data
      )
    ))
  }
}

#' Tạo dữ liệu cho bài toán hồi quy tuyến tính
#'
#' Hàm này tạo dữ liệu giả để xây dựng bài toán hồi quy tuyến tính đơn.
#' Lưu ý: Các hệ số hồi quy tuyến tính truyền vào không phải các hệ số
#' hồi quy tuyến tính cuối cùng.
#' Hàm này trả về các vector x, y, có thể dùng dưới dạng data$x, data$y nếu sử dụng tham số
#' simplify=TRUE
#'
#' @return Có 2 trường hợp:
#' * Nếu simplify=FALSE, hàm trả về 1 list gồm input_data, chứa các tham số đã truyền vào,
#' và output_data, gồm 2 vector là x và y.
#' * Nếu simplify=TRUE, hàm chỉ trả về 1 list gồm 2 vector dữ liệu là x và y để sử dụng.
#' @export
data_simulate_regression <- function(n, min_x, max_x, b0, b1, sd_eps, round_digits, silent=FALSE, simplify = FALSE) {
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
    print("Bài toán: Sinh dữ liệu cho bài toán hồi quy tuyến tính đơn")
    print("Input")
    print_huxtable(
      data.frame(
        n = n, min_x = min_x, max_x = max_x,
        b0 = b0, b1 = b1, sd_eps = sd_eps, round_digits = round_digits
      )
    )
    print("Output")
    print_huxtable(df)
  }
  if(simplify) {
    invisible(list(
      x = x, y = y
    ))
  } else {
    invisible(list(
      input_data = list(
        n = n, min_x = min_x, max_x = max_x,
        b0 = b0, b1 = b1, sd_eps = sd_eps, round_digits = round_digits
      ),
      output_data = list(
        x = x, y = y
      )
    ))
  }
}

#' Dữ liệu cho bài toán kiểm định khi bình phương.
#'
#' Hàm này dùng để tạo dữ liệu cho bài toán kiểm định sự phù hợp của k tỷ lệ
#'
#' @return
#' * Nếu simplify=FALSE, hàm trả về 1 list gồm input_data, chứa các tham số đã truyền vào,
#' và output_data, có vector freq là dữ liệu được sinh ra.
#' * Nếu simplify=TRUE, hàm chỉ trả về vector dữ liệu được sinh ra.
#' @export
data_simulate_test_goodness_of_fit <- function (expected, silent=FALSE, simplify = FALSE) {
  size <- sum(expected)
  data <- sample(seq_along(expected), size, prob = expected, replace = TRUE)
  freq <- table(data)
  if(!silent) {
    print("Bài toán: Sinh dữ liệu cho bài toán kiểm định khi bình phương")
    print("Input")
    print_huxtable(t(expected))
    print("Output")
    print_huxtable(freq)
  }
  if(simplify) {
    invisible(freq)
  } else {
    invisible(list(
      input_data = list(
        expected = expected
      ),
      output_data = list(
        freq = freq
      )
    ))
  }
}

#' Dữ liệu cho bài toán kiểm định k tỷ lệ.
#'
#' Hàm này dùng để tạo dữ liệu cho bài toán so sánh k tỷ lệ
#'
#' @return Có 2 trường hợp:
#' * Nếu simplify=FALSE, hàm trả về input_data gồm các tham số đã nhập vào và output_data
#' bao gồm 1 ma trận matrix là kết quả của hàm.
#' * Nếu simplify=TRUE, hàm chỉ trả về ma trận dữ liệu.
#'
#' @export
data_simulate_test_k_prop <- function (expected_m_i, expected_l_i, silent=FALSE, simplify = FALSE) {
  expected <- c(expected_m_i, expected_l_i)
  size <- sum(expected)
  data <- sample(seq_along(expected), size, prob = expected, replace = TRUE)
  freq <- table(data)
  matrix <- matrix(freq, nrow = 2, ncol = length(expected_m_i), byrow = TRUE)
  if(!silent) {
    print("Bài toán: Sinh dữ liệu cho bài toán kiểm định k tỷ lệ")
    print("Input")
    print_huxtable(t(data.frame(expected_m_i = expected_m_i, expected_l_i = expected_l_i)))
    print("Output")
    print_huxtable(matrix)
  }
  if(simplify) {
    invisible(matrix)
  } else {
    invisible(list(
      input_data = list(
        expected_m_i = expected_m_i, expected_l_i = expected_l_i
      ),
      output_data = list(
        matrix = matrix
      )
    ))
  }
}

#' Dữ liệu cho bài toán kiểm định tính độc lập.
#'
#' Hàm này dùng để tạo dữ liệu cho bài toán kiểm định tính độc lập
#'
#' @return Có 2 trường hợp:
#' * Nếu simplify=FALSE, hàm trả về input_data gồm các tham số đã nhập vào và output_data
#' bao gồm 1 ma trận matrix là kết quả của hàm.
#' * Nếu simplify=TRUE, hàm chỉ trả về ma trận dữ liệu.
#' @export
data_simulate_test_independent <- function (expected_matrix, silent=FALSE, simplify = FALSE) {
  vector <- as.vector(expected_matrix)
  size <- sum(vector)
  data <- sample(seq_along(vector), size, prob = vector, replace = TRUE)
  freq <- table(data)
  matrix <- matrix(freq, nrow=dim(expected_matrix)[1],
                   ncol = dim(expected_matrix)[2], byrow = FALSE)
  if(!silent) {
    print("Bài toán: Sinh dữ liệu cho bài toán kiểm định tính độc lập")
    print("Input")
    print_huxtable(expected_matrix)
    print("Output")
    print_huxtable(matrix)
  }
  if(simplify) {
    invisible(matrix)
  } else {
    invisible(list(
      input_data = list(
        expected_matrix = expected_matrix
      ),
      output_data = list(
        matrix = matrix
      )
    ))
  }
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
