Table of Contents
=================

-   [Introduction](#introduction)
-   [Why ?](#why-?)
-   [Install](#install)
-   [How to use](#how-to-use)
    -   [Generate data](#generate-data)
    -   [Calculate](#calculate)
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

Generate data
-------------

``` r
library(thongke)
library(testthat)

test_that("Test các data_* function", {
  data1 <- data_simulate_discrete(n = 100, mean = 5, sd = 2, min = 2, max = 8, simplify = TRUE)
  data2 <- data_simulate_continuous(n = 100, mean = 5, sd = 2, min = 2, max = 8, size = 1, simplify = TRUE)
  data3 <- data_simulate_regression(n = 10, min_x = 15, max_x = 30, b0 = 5, b1 = 10, sd_eps = 3, round_digits = 2, simplify = TRUE)
  print(mean(data1))
  print(mean(data2))
  print(mean(data3$x))
  print(mean(data3$y))
  data_simulate_test_goodness_of_fit(
    expected = c(100, 100, 100, 100, 100, 100),
    simplify = TRUE
  )

  data_simulate_test_k_prop(
    expected_m_i = c(160, 320, 240, 160),
    expected_l_i = c(40, 80, 60, 40),
    simplify = TRUE
  )

  data_simulate_test_independent(
    expected_matrix = matrix(
      c(100, 200, 300, 400, 500, 600, 700, 800, 900), nrow=3, ncol=3, byrow = TRUE
    ),
    simplify = TRUE
  )

  expect_equal(1, 1)
})
```

    ## [1] "Bài toán: Sinh dữ liệu theo phân bố chuẩn"
    ## [1] "Input"
    ##                      n   mean    sd   min   max   round_digi  
    ##                                                           ts  
    ##                  ─────────────────────────────────────────────
    ##                    100      5     2     2     8            0  
    ## [1] "Output"
    ##                                data         Freq  
    ##                              ─────────────────────
    ##                                2               2  
    ##                                3              14  
    ##                                4              19  
    ##                                5              20  
    ##                                6              26  
    ##                                7              13  
    ##                                8               6  
    ## [1] "Bài toán: Sinh dữ liệu ngẫu nhiên"
    ## [1] "Input"
    ##                         n   mean    sd   min   max   size  
    ##                     ───────────────────────────────────────
    ##                       100      5     2     2     8      1  
    ## [1] "Output"
    ##                                          V1       
    ##                              ─────────────────────
    ##                                  (2,3]   10       
    ##                                  (3,4]   16       
    ##                                  (4,5]   22       
    ##                                  (5,6]   21       
    ##                                  (6,7]   22       
    ##                                  (7,8]   9        
    ## [1] "Bài toán: Sinh dữ liệu cho bài toán hồi quy tuyến tính đơn"
    ## [1] "Input"
    ##                  n   min_x   max_x   b0   b1   sd_eps   round_di  
    ##                                                             gits  
    ##               ────────────────────────────────────────────────────
    ##                 10      15      30    5   10        3          2  
    ## [1] "Output"
    ##                                      x         y  
    ##                              ─────────────────────
    ##                                  16.89    170.94  
    ##                                  18.35    187.39  
    ##                                  21.29    218.68  
    ##                                  22.93    229.6   
    ##                                  23.14    236.38  
    ##                                  23.22    237.08  
    ##                                  23.24    234.44  
    ##                                  25.12    261.09  
    ##                                  26.88    275.01  
    ##                                  27.17    273.76  
    ## [1] 5.17
    ## [1] 5.06
    ## [1] 22.823
    ## [1] 232.437
    ## [1] "Bài toán: Sinh dữ liệu cho bài toán kiểm định khi bình phương"
    ## [1] "Input"
    ##                        100   100   100   100   100   100  
    ## [1] "Output"
    ##                                          V1       
    ##                              ─────────────────────
    ##                                      1   81       
    ##                                      2   110      
    ##                                      3   93       
    ##                                      4   104      
    ##                                      5   106      
    ##                                      6   106      
    ## [1] "Bài toán: Sinh dữ liệu cho bài toán kiểm định k tỷ lệ"
    ## [1] "Input"
    ##                             160    320    240    160  
    ##                              40     80     60     40  
    ## [1] "Output"
    ##                             182    291    251    156  
    ##                              34     83     66     37  
    ## [1] "Bài toán: Sinh dữ liệu cho bài toán kiểm định tính độc lập"
    ## [1] "Input"
    ##                                100     200     300  
    ##                                400     500     600  
    ##                                700     800     900  
    ## [1] "Output"
    ##                                104     204     290  
    ##                                391     483     603  
    ##                                696     797     932  
    ## Test passed 😀

Calculate
---------

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
    sigma = 3, n = 36, alpha = 0.05, mean = 66, alternative = "min"
  )
  print("===================================================")
  estimate_mean_norm(
    sigma = 3, n = 36, alpha = 0.05, mean = 66, alternative = "max"
  )
  # Đáp số: 65,02 - 66,98
  print("===================================================")
  estimate_mean_t(
    mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144)
  )
  print("===================================================")
  estimate_mean_t(
    mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144), alternative = "min"
  )
  print("===================================================")
  estimate_mean_t(
    mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144), alternative = "max"
  )
  # Đáp số: 39,5023 - 40,0977
  print("===================================================")
  estimate_var(n = 30, s = 0.032, alpha = 0.025)
  print("===================================================")
  estimate_var(n = 30, s = 0.032, alpha = 0.025, alternative = "min")
  print("===================================================")
  estimate_var(n = 30, s = 0.032, alpha = 0.025, alternative = "max")
  # Đáp số: 0.000649 - 0.001851
  print("===================================================")
  estimate_prop(n = 100, f = 0.6, alpha = 0.1)
  print("===================================================")
  estimate_prop(n = 100, f = 0.6, alpha = 0.1, alternative = "min")
  print("===================================================")
  estimate_prop(n = 100, f = 0.6, alpha = 0.1, alternative = "max")
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
    mean = 27.56, mean_0 = 26, alternative = "neq"
  )
  # Đáp số: T=3 - c=1.96 - Bác bỏ
  print("===================================================")
  data <- c(19, 18, 22, 20, 16, 25)
  test_mean_t(
    mean = mean(data), mean_0 = 21.5, alternative = "neq",
    n = 6, alpha = 0.05, s = sqrt(var(data))
  )
  # Đáp số: T=-1.16 - c=2.571 - Chấp nhận
  print("===================================================")
  test_prop(f = 0.4, p_0 = 0.45, alpha = 0.05, n = 200, alternative = "neq")
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
    alpha = 0.01, alternative = "neq"
  )
  # Đáp số: T=5, c=2.58, Bác bỏ
  print("===================================================")
  test_2_mean_t(
    alternative = "neq", alpha = 0.01, mean1 = 4.8, n1 = 10,
    s1 = 1.1, mean2 = 4.3, n2 = 12, s2 = 0.9
  )
  # Đáp số: T=1.174, c=2.845, Chấp nhận
  print("===================================================")
  test_2_prop(
    f1 = 0.42, f2 = 0.46, n1 = 100, n2 = 200,
    alpha = 0.05, alternative = "neq"
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
    ## [1] "Input"
    ##                    n   mean             s   alpha   alternative  
    ##                ──────────────────────────────────────────────────
    ##                   15   39.8   0.379473319    0.01   two_sided    
    ## [1] "Output"
    ## [1] "Khoảng tin cậy hai phía là: (39.5083; 40.0917)"
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố chuẩn)"
    ## [1] "Input"
    ##                       n   mean   sigma   alpha   alternative  
    ##                   ────────────────────────────────────────────
    ##                      36     66       3    0.05   two_sided    
    ## [1] "Output"
    ## [1] "Khoảng tin cậy hai phía là: (65.0200; 66.9800)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố chuẩn)"
    ## [1] "Input"
    ##                       n   mean   sigma   alpha   alternative  
    ##                   ────────────────────────────────────────────
    ##                      36     66       3    0.05   min          
    ## [1] "Output"
    ## [1] "Khoảng tin cậy nhỏ nhất là: (65.1776; +inf)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố chuẩn)"
    ## [1] "Input"
    ##                       n   mean   sigma   alpha   alternative  
    ##                   ────────────────────────────────────────────
    ##                      36     66       3    0.05   max          
    ## [1] "Output"
    ## [1] "Khoảng tin cậy lớn nhất là: (-inf; 66.8224)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố Student)"
    ## [1] "Input"
    ##                    n   mean             s   alpha   alternative  
    ##                ──────────────────────────────────────────────────
    ##                   15   39.8   0.379473319    0.01   two_sided    
    ## [1] "Output"
    ## [1] "Khoảng tin cậy hai phía là: (39.5083; 40.0917)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố Student)"
    ## [1] "Input"
    ##                    n   mean             s   alpha   alternative  
    ##                ──────────────────────────────────────────────────
    ##                   15   39.8   0.379473319    0.01   min          
    ## [1] "Output"
    ## [1] "Khoảng tin cậy nhỏ nhất là: (39.5429; +inf)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho trung bình (phân bố Student)"
    ## [1] "Input"
    ##                    n   mean             s   alpha   alternative  
    ##                ──────────────────────────────────────────────────
    ##                   15   39.8   0.379473319    0.01   max          
    ## [1] "Output"
    ## [1] "Khoảng tin cậy lớn nhất là: (-inf; 40.0571)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho phương sai"
    ## [1] "Input"
    ##                           n       s   alpha   alternative  
    ##                      ──────────────────────────────────────
    ##                          30   0.032   0.025   two_sided    
    ## [1] "Output"
    ## [1] "Khoảng tin cậy hai phía là: (0.0006; 0.0020)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho phương sai"
    ## [1] "Input"
    ##                           n       s   alpha   alternative  
    ##                      ──────────────────────────────────────
    ##                          30   0.032   0.025   min          
    ## [1] "Output"
    ## [1] "Khoảng tin cậy nhỏ nhất là: (0.0006; +inf)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho phương sai"
    ## [1] "Input"
    ##                           n       s   alpha   alternative  
    ##                      ──────────────────────────────────────
    ##                          30   0.032   0.025   max          
    ## [1] "Output"
    ## [1] "Khoảng tin cậy lớn nhất là: (0; 0.0019)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho tỷ lệ"
    ## [1] "Input"
    ##                           n      f   alpha   alternative  
    ##                      ─────────────────────────────────────
    ##                         100    0.6     0.1   two_sided    
    ## [1] "Output"
    ## [1] "Khoảng tin cậy hai phía là: (0.5194; 0.6806)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho tỷ lệ"
    ## [1] "Input"
    ##                           n      f   alpha   alternative  
    ##                      ─────────────────────────────────────
    ##                         100    0.6     0.1   min          
    ## [1] "Output"
    ## [1] "Khoảng tin cậy nhỏ nhất là: (0.5372; 1)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Ước lượng khoảng cho tỷ lệ"
    ## [1] "Input"
    ##                           n      f   alpha   alternative  
    ##                      ─────────────────────────────────────
    ##                         100    0.6     0.1   max          
    ## [1] "Output"
    ## [1] "Khoảng tin cậy lớn nhất là: (0; 0.6628)"
    ## [1] "==================================================="
    ## [1] "Bài toán: Xác định kích thước mẫu (ước lượng trung bình)"
    ## [1] "Input"
    ##                           sigma     eps         alpha  
    ##                         ───────────────────────────────
    ##                               3     0.5   0.101005167  
    ## [1] "Output"
    ## [1] "Kích thước mẫu tối thiểu: 96.8256"
    ## [1] "==================================================="
    ## [1] "Bài toán: Xác định kích thước mẫu (ước lượng tỷ lệ, đã biết f)"
    ## [1] "Input"
    ##                               f     eps         alpha  
    ##                         ───────────────────────────────
    ##                            0.64    0.02   0.101005167  
    ## [1] "Output"
    ## [1] "Kích thước mẫu tối thiểu: 1549.2096"
    ## [1] "==================================================="
    ## [1] "Bài toán: Xác định kích thước mẫu (ước lượng tỷ lệ, chưa biết f)"
    ## [1] "Input"
    ##                                  eps         alpha  
    ##                            ─────────────────────────
    ##                                 0.02   0.101005167  
    ## [1] "Output"
    ## [1] "Kích thước mẫu tối thiểu: 1681.0000"
    ## [1] "==================================================="
    ## [1] "Bài toán: Kiểm định giả thiết cho giá trị trung bình (phân bố chuẩn)"
    ## [1] "Input"
    ##                  n    mean   mean_0   sigma   alpha   alternativ  
    ##                                                       e           
    ##              ─────────────────────────────────────────────────────
    ##                100   27.56       26     5.2    0.05   neq         
    ## [1] "Output"
    ##                                     T            c  
    ##                             ────────────────────────
    ##                                     3   1.95996398  
    ## [1] "Kết luận: Bác bỏ H0, chấp nhận H1"
    ## [1] "==================================================="
    ## [1] "Bài toán: Kiểm định giả thiết cho giá trị trung bình (phân bố Student)"
    ## [1] "Input"
    ##                n   mean   mean_0            s   alpha   alternativ  
    ##                                                         e           
    ##            ─────────────────────────────────────────────────────────
    ##                6     20     21.5   3.16227766    0.05   neq         
    ## [1] "Output"
    ##                                      T            c  
    ##                            ──────────────────────────
    ##                              -1.161895   2.57058184  
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: Kiểm định giả thiết cho tỷ lệ"
    ## [1] "Input"
    ##                        n     f    p_0   alpha   alternative  
    ##                    ──────────────────────────────────────────
    ##                      200   0.4   0.45    0.05   neq          
    ## [1] "Output"
    ##                                       T            c  
    ##                           ────────────────────────────
    ##                             -1.42133811   1.95996398  
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: Kiểm định khi bình phương (kiểm định cho k tỷ lệ)"
    ## [1] "Input"
    ##                        106    92    97   105    88   112  
    ##                        100   100   100   100   100   100  
    ## [1] "alpha = 0.05"
    ## [1] "Output"
    ##                                     T            c  
    ##                             ────────────────────────
    ##                                  4.22   11.0704977  
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: So sánh 2 giá trị trung bình (phân bố chuẩn)"
    ## [1] "Input"
    ##                               n    mean         sigma  
    ##                         ───────────────────────────────
    ##                              40     130    8.94427191  
    ##                              50     140   10           
    ## [1] "alternative = neq, alpha = 0.01"
    ## [1] "Output"
    ##                                     T           c  
    ##                             ───────────────────────
    ##                                    -5   2.5758293  
    ## [1] "Kết luận: Bác bỏ H0, chấp nhận H1"
    ## [1] "==================================================="
    ## [1] "Bài toán: So sánh 2 giá trị trung bình (phân bố Student)"
    ## [1] "Input"
    ##                                  n    mean       s  
    ##                            ─────────────────────────
    ##                                 10     4.8     1.1  
    ##                                 12     4.3     0.9  
    ## [1] "alternative = neq, alpha = 0.01"
    ## [1] "Output"
    ##                                      T            c  
    ##                           ───────────────────────────
    ##                             1.17363132   2.84533971  
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: So sánh 2 tỷ lệ"
    ## [1] "Input"
    ##                                      n         f  
    ##                              ─────────────────────
    ##                                    100      0.42  
    ##                                    200      0.46  
    ## [1] "alternative = neq, alpha = 0.05"
    ## [1] "Output"
    ##                                       T            c  
    ##                          ─────────────────────────────
    ##                            -0.656945245   1.95996398  
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: So sánh k tỷ lệ"
    ## [1] "Input"
    ##                         79    82    77    83    76    81  
    ##                        100   100   100   100   100   100  
    ## [1] "alpha = 0.05"
    ## [1] "Output"
    ##                                      T            c  
    ##                           ───────────────────────────
    ##                             2.42815008   11.0704977  
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: Kiểm định tính độc lập"
    ## [1] "Input"
    ##                                    328        77  
    ##                                    122        33  
    ## [1] "alpha = 0.05"
    ## [1] "Output"
    ##                                       T            c  
    ##                           ────────────────────────────
    ##                             0.368526041   3.84145882  
    ## [1] "Kết luận: Chưa đủ cơ sở để bác bỏ H0"
    ## [1] "==================================================="
    ## [1] "Bài toán: Tính hệ số tương quan"
    ## [1] "Input"
    ## 'data.frame':    10 obs. of  2 variables:
    ##  $ x: num  80 85 88 90 95 92 82 75 78 85
    ##  $ y: num  2.4 2.8 3.3 3.1 3.7 3 2.5 2.3 2.8 3.1
    ## [1] "Hệ số tương quan: 0.8590"
    ## [1] "==================================================="
    ## [1] "Bài toán: Bài toán hồi quy tuyến tính đơn"
    ## [1] "Input"
    ## 'data.frame':    6 obs. of  2 variables:
    ##  $ x: num  400 600 500 600 400 500
    ##  $ y: num  44 47 48 48 43 46
    ## [1] "Hệ số tự do: 36.0000"
    ## [1] "Hệ số ứng với x: 0.0200"
    ## [1] "Input"
    ##                        400   600   500   600   400   500  
    ##                         44    47    48    48    43    46  
    ## [1] "Output"
    ##                    sum_x   sum_y   sum_xy    sum_x2   sum_y2  
    ##                  ─────────────────────────────────────────────
    ##                     3000     276   138800   1540000    12718  
    ## Test passed 😸

License
=======

MIT License

Copyright (c) 2021-2022 vungocbinh2009

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
