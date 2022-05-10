-   [Introduction](#introduction)
-   [Why ?](#why)
-   [Install](#install)
-   [How to use](#how-to-use)
-   [License](#license)

Introduction
============

A simple statistics package for students

Why ?
=====

-   Some exercises in statistics do not have functions in R and need
    3rd-party packages.
-   Calculate hypothesis testing exercises use traditional method.
-   Just for fun :grinning:

Install
=======

``` r
# Install devtools
install.packages("devtools")
# Install thongke
devtools::install_github("vungocbinh2009/thongke")
```

How to use
==========

### Generate data

``` r
library(thongke)
library(testthat)

test_that("Test các data_* function", {
  data_1 <- data_simulate_discrete(n = 100, mean = 5, sd = 2, min = 2, max = 8)
  print(mean(data_1))
  print(var(data_1))
  data_2 <- data_simulate_continuous(n = 100, mean = 5, sd = 2, min = 2, max = 8, size = 1)
  print(mean(data_2))
  print(var(data_2))
  data_3 <- data_simulate_regression(n = 10, min_x = 15, max_x = 30, b0 = 5, b1 = 10, sd_eps = 3, round_digits = 2)
  x <- data_3$x
  y <- data_3$y
  print(mean(x))
  print(var(x))
  print(mean(y))
  print(var(y))

  data_simulate_test_goodness_of_fit(
    c(100, 100, 100, 100, 100, 100),
  )

  data_simulate_test_k_prop(
    c(160, 320, 240, 160), c(40, 80, 60, 40),
  )

  data_simulate_test_independent(matrix(
    c(100, 200, 300, 400, 500, 600, 700, 800, 900), nrow=3, ncol=3, byrow = TRUE)
  )

  expect_equal(1, 1)
})
```

    ## 
    ##  2  3  4  5  6  7  8 
    ##  2 13 15 19 28 18  5 
    ## [1] 5.32
    ## [1] 2.219798
    ## data.cut
    ## (2,3] (3,4] (4,5] (5,6] (6,7] (7,8] 
    ##    12    17    20    24    18     9 
    ## [1] 4.96
    ## [1] 2.230707
    ##        x      y
    ## 3  16.27 167.54
    ## 1  17.25 174.90
    ## 2  17.59 180.95
    ## 6  18.50 188.89
    ## 8  20.89 218.35
    ## 5  23.67 230.88
    ## 10 24.55 250.47
    ## 7  27.55 282.14
    ## 4  29.01 296.66
    ## 9  29.35 296.70
    ## [1] 22.463
    ## [1] 25.49316
    ## [1] 228.748
    ## [1] 2576.007
    ## [1] 103 110 104  95  84 104
    ##      [,1] [,2] [,3] [,4]
    ## [1,]  158  314  258  148
    ## [2,]   35   75   68   44
    ##      [,1] [,2] [,3]
    ## [1,]   92  197  309
    ## [2,]  417  524  632
    ## [3,]  709  745  875
    ## Test passed 😸

### Calculate

``` r
library(thongke)
library(testthat)

test_that("Test các trường hợp cụ thể", {
  # In ra để không mất dòng dưới
  estimate_mean_t(
    mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144)
  )
  estimate_mean_norm(
    sigma = 3, n = 36, alpha = 0.05, mean = 66
  )
  print("===================================================")
  estimate_mean_norm(
    sigma = 3, n = 36, alpha = 0.05, mean = 66, mode = "min"
  )
  print("===================================================")
  estimate_mean_norm(
    sigma = 3, n = 36, alpha = 0.05, mean = 66, mode = "max"
  )
  # Đáp số: 65,02 - 66,98
  print("===================================================")
  estimate_mean_t(
    mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144)
  )
  print("===================================================")
  estimate_mean_t(
    mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144), mode = "min"
  )
  print("===================================================")
  estimate_mean_t(
    mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144), mode = "max"
  )
  # Đáp số: 39,5023 - 40,0977
  print("===================================================")
  estimate_var(n = 30, s = 0.032, alpha = 0.025)
  print("===================================================")
  estimate_var(n = 30, s = 0.032, alpha = 0.025, mode = "min")
  print("===================================================")
  estimate_var(n = 30, s = 0.032, alpha = 0.025, mode = "max")
  # Đáp số: 0.000649 - 0.001851
  print("===================================================")
  estimate_prop(n = 100, f = 0.6, alpha = 0.1)
  print("===================================================")
  estimate_prop(n = 100, f = 0.6, alpha = 0.1, mode = "min")
  print("===================================================")
  estimate_prop(n = 100, f = 0.6, alpha = 0.1, mode = "max")
  # Đáp số: 0.52 - 0.68
  print("===================================================")
  sample_size_mean(
    sigma = 3, alpha = get_alpha(1.64), eps = 0.5
  )
  # Đáp số: 96.826
  print("===================================================")
  sample_size_prop_1(
    f = 0.64, alpha = get_alpha(1.64), eps = 0.02
  )
  # Đáp số: 1549.2
  print("===================================================")
  sample_size_prop_2(eps = 0.02, alpha = get_alpha(1.64))
  # Đáp số: 1681
  print("===================================================")
  test_mean_norm(
    sigma = 5.2, alpha = 0.05, n = 100,
    mean = 27.56, mean_0 = 26, mode = "neq"
  )
  # Đáp số: T=3 - c=1.96 - Bác bỏ
  print("===================================================")
  data <- c(19, 18, 22, 20, 16, 25)
  test_mean_t(
    mean = mean(data), mean_0 = 21.5, mode = "neq",
    n = 6, alpha = 0.05, s = sqrt(var(data))
  )
  # Đáp số: T=-1.16 - c=2.571 - Chấp nhận
  print("===================================================")
  test_prop(f = 0.4, p_0 = 0.45, alpha = 0.05, n = 200, mode = "neq")
  # Đáp số: T=-1.43, c=1.96, Chấp nhận
  print("===================================================")
  test_goodness_of_fit(
    expected = c(100, 100, 100, 100, 100, 100),
    actual = c(106, 92, 97, 105, 88, 112),
    alpha = 0.05
  )
  # Đáp số: T=4.22, c=11.070, Chấp nhận
  print("===================================================")
  test_2_mean_norm(
    n1 = 40, n2 = 50, mean1 = 130, mean2 = 140,
    sigma1 = sqrt(80), sigma2 = sqrt(100),
    alpha = 0.01, mode = "neq"
  )
  # Đáp số: T=5, c=2.58, Bác bỏ
  print("===================================================")
  test_2_mean_t(
    mode = "neq", alpha = 0.01, mean1 = 4.8, n1 = 10,
    s1 = 1.1, mean2 = 4.3, n2 = 12, s2 = 0.9
  )
  # Đáp số: T=1.174, c=2.845, Chấp nhận
  print("===================================================")
  test_2_prop(
    f1 = 0.42, f2 = 0.46, n1 = 100, n2 = 200,
    alpha = 0.05, mode = "neq"
  )
  # Đáp số: T=-0.66, c=1.96, Chấp nhận
  print("===================================================")
  test_k_prop(
    m_i = c(79, 82, 77, 83, 76, 81),
    n_i = c(100, 100, 100, 100, 100, 100),
    alpha = 0.05
  )
  # Đáp số: T=2.42. c=11.07. Chấp nhận
  print("===================================================")
  test_independent(
    matrix = matrix(data = c(328, 122, 77, 33), ncol = 2, nrow = 2),
    alpha = 0.05
  )
  # Đáp số: T=0.368, c=3.841, Chấp nhận
  print("===================================================")
  correlation(
    x = c(80, 85, 88, 90, 95, 92, 82, 75, 78, 85),
    y = c(2.4, 2.8, 3.3, 3.1, 3.7, 3, 2.5, 2.3, 2.8, 3.1)
  )
  # Đáp số: 0.858
  print("===================================================")
  linear_regression(
    x = c(400, 600, 500, 600, 400, 500),
    y = c(44, 47, 48, 48, 43, 46)
  )
  # Đáp số: y=0.02x + 36
  calculate_sum(
    x = c(400, 600, 500, 600, 400, 500),
    y = c(44, 47, 48, 48, 43, 46)
  )
  expect_equal(1, 1)
})
```

    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố Student)"
    ## [1] "Khoảng tin cậy hai phía là: (39.5083; 40.0917)"
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố chuẩn)"
    ## [1] "Khoảng tin cậy hai phía là: (65.0200; 66.9800)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố chuẩn)"
    ## [1] "Khoảng tin cậy nhỏ nhất là: (65.1776; +inf)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố chuẩn)"
    ## [1] "Khoảng tin cậy lớn nhất là: (-inf; 66.8224)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố Student)"
    ## [1] "Khoảng tin cậy hai phía là: (39.5083; 40.0917)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố Student)"
    ## [1] "Khoảng tin cậy nhỏ nhất là: (39.5429; +inf)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố Student)"
    ## [1] "Khoảng tin cậy lớn nhất là: (-inf; 40.0571)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho phương sai"
    ## [1] "Khoảng tin cậy hai phía là: (0.0006; 0.0020)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho phương sai"
    ## [1] "Khoảng tin cậy nhỏ nhất là: (0.0006; +inf)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho phương sai"
    ## [1] "Khoảng tin cậy lớn nhất là: (0; 0.0019)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho tỷ lệ"
    ## [1] "Khoảng tin cậy hai phía là: (0.5194; 0.6806)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho tỷ lệ"
    ## [1] "Khoảng tin cậy nhỏ nhất là: (0.5372; 1)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho tỷ lệ"
    ## [1] "Khoảng tin cậy lớn nhất là: (0; 0.6628)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Xác định kích thước mẫu (ước lượng trung bình)"
    ## [1] "Kích thước mẫu tối thiểu: 96.8256"
    ## [1] "==================================================="
    ## [1] "Bài toán: Xác định kích thước mẫu (ước lượng tỷ lệ, đã biết f)"
    ## [1] "Kích thước mẫu tối thiểu: 1549.2096"
    ## [1] "==================================================="
    ## [1] "Bài toán: Xác định kích thước mẫu (ước lượng tỷ lệ, chưa biết f)"
    ## [1] "Kích thước mẫu tối thiểu: 1681.0000"
    ## [1] "==================================================="
    ## [1] "Bài toán: Kiểm định giả thiết cho giá trị trung bình (phân bố chuẩn)"
    ## [1] "Kết quả test thống kê: 3.0000"
    ## [1] "Kết quả của c: 1.9600"
    ## [1] "Kết luận: Bác bỏ H0, chấp nhận H1"
    ## [1] "==================================================="
    ## [1] "Bài toán: Kiểm định giả thiết cho giá trị trung bình (phân bố Student)"
    ## [1] "Kết quả test thống kê: -1.1619"
    ## [1] "Kết quả của c: 2.5706"
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: Kiểm định giả thiết cho tỷ lệ"
    ## [1] "Kết quả test thống kê: -1.4213"
    ## [1] "Kết quả của c: 1.9600"
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: Kiểm định khi bình phương (kiểm định cho k tỷ lệ)"
    ## [1] "Kết quả test thống kê: 4.2200"
    ## [1] "Kết quả của c: 11.0705"
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: So sánh 2 giá trị trung bình (phân bố chuẩn)"
    ## [1] "Kết quả test thống kê: -5.0000"
    ## [1] "Kết quả của c: 2.5758"
    ## [1] "Kết luận: Bác bỏ H0, chấp nhận H1"
    ## [1] "==================================================="
    ## [1] "Bài toán: So sánh 2 giá trị trung bình (phân bố Student)"
    ## [1] "Kết quả test thống kê: 1.1736"
    ## [1] "Kết quả của c: 2.8453"
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: So sánh 2 tỷ lệ"
    ## [1] "Kết quả test thống kê: -0.6569"
    ## [1] "Kết quả của c: 1.9600"
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: So sánh n tỷ lệ"
    ## [1] "Kết quả test thống kê: 2.4282"
    ## [1] "Kết quả của c: 11.0705"
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: Kiểm định tính độc lập"
    ## [1] "Kết quả test thống kê: 0.3685"
    ## [1] "Kết quả của c: 3.8415"
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: Tính hệ số tương quan"
    ## [1] "Hệ số tương quan: 0.8590"
    ## [1] "==================================================="
    ## [1] "Bài toán: Bài toán hồi quy tuyến tính đơn"
    ## [1] "Hệ số tự do: 36.0000"
    ## [1] "Hệ số ứng với x: 0.0200"
    ## [1] "Tổng x: 3000.0000"
    ## [1] "Tổng y: 276.0000"
    ## [1] "Tổng xy: 138800.0000"
    ## [1] "Tổng x2: 1540000.0000"
    ## [1] "Tổng y2: 12718.0000"
    ## Test passed 🌈

License
=======

MIT License

Copyright (c) 2021 vungocbinh2009

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
