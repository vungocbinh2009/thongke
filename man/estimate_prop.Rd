% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{estimate_prop}
\alias{estimate_prop}
\title{Ước lượng khoảng cho tỷ lệ.}
\usage{
estimate_prop(n, f, alpha, alternative = "two_sided", silent = FALSE)
}
\description{
Hàm này ước lượng khoảng cho tỷ lệ
Hàm trả về các giá trị z_alpha, z_alpha_div_2 (z_alpha/2), bottom, top (khoảng tin cậy 2 phía), min (khoảng tin cậy nhỏ nhất), max (khoảng tin cậy lớn nhất)
alternative có 3 chế độ: two_sided, min và max tương ứng với khoảng tin cậy hai phía, lớn nhất và nhả nhất
}
