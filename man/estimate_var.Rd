% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{estimate_var}
\alias{estimate_var}
\title{Ước lượng khoảng cho phương sai}
\usage{
estimate_var(n, s, alpha, alternative = "two_sided", silent = FALSE)
}
\description{
Hàm này ước lượng khoảng cho phương sai.
Hàm trả về các giá trị chi_sq_1, chi_sq_2, chi_sq_1_2 (chisq(1-a/2)), chi_sq_2_2 (chisq(a/2)), bottom, top, min, max.
alternative có 3 chế độ: two_sided, min và max tương ứng với khoảng tin cậy hai phía, lớn nhất và nhả nhất
}
