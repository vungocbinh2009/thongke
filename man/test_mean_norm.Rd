% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hypothesis_test.R
\name{test_mean_norm}
\alias{test_mean_norm}
\title{Kiểm định giả thiết về giá trị trung bình (phân bố chuẩn)}
\usage{
test_mean_norm(
  n,
  mean,
  mean_0,
  sigma,
  alpha,
  alternative = "neq",
  silent = FALSE
)
}
\description{
Hàm này kiểm định giả thiết về gái trị trung bình, dùng phân bố chuẩn
Tham số alternative là 1 trong 3 giá trị: neq, less, greater tương ứng với 3 đối thiết.
Hàm trả về kết quả của test thống kê (test) và giá trị c
}
