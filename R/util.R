#' Hàm này có chức năng tương tự như hàm printf trong C
printf <- function (...) {
  print(sprintf(...))
}

#' Hàm này dùng để tìm giá trị alpha từ giá trị z_alpha cho trước (phân bố chuẩn)
#'
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

#' Hàm này dùng để tạo và in bảng dataframe, sử dụng huxtable.
#'@import huxtable
print_huxtable <- function(df) {
  hux_output <- as_hux(df)
  hux_output <- set_all_borders(hux_output)
  print_screen(hux_output, colnames = FALSE)
}

