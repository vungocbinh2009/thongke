# thongke
Một thư viện tính toán thống kê đơn giản, dễ sử dụng dành cho người dạy và người học

# Install
```r
# Install devtools
install.packages("devtools")
# Install thongke
devtools::install_github("vungocbinh2009/thongke")
```

# How to use
```r
estimate_mean_norm(sigma = 3, n = 36, alpha = 0.05, mean = 66)
# Đáp số: 65,02 - 66,98

estimate_mean_t(mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144))
# Đáp số: 39,5023 - 40,0977

estimate_var(n = 30, s = 0.032, alpha = 0.025)
# Đáp số: 0.000649 - 0.001851

estimate_prop(n = 100, f = 0.6, alpha = 0.1)
# Đáp số: 0.52 - 0.68

sample_size_mean(sigma = 3, alpha = 0.1, eps = 0.5)
# Đáp số: 96.826

sample_size_prop_1(f = 0.64, alpha = 0.1, eps = 0.02)
# Đáp số: 1549.2

sample_size_prop_2(eps = 0.02, alpha = 0.1)
# Đáp số: 1681

test_mean_norm(sigma = 5.2, alpha = 0.05, n = 100,
               mean = 27.56, mean_0 = 26, mode = "neq")
# Đáp số: T=3 - c=1.96 - Bác bỏ

data <- c(19, 18, 22, 20, 16, 25)
test_mean_t(mean = mean(data), mean_0 = 21.5, mode = "neq",
            n = 6, alpha = 0.05, s = sqrt(var(data)))
# Đáp số: T=-1.16 - c=2.571 - Chấp nhận

test_prop(f = 0.4, p_0 = 0.45, alpha = 0.05, n = 200, mode = "neq")
# Đáp số: T=-1.43, c=1.96, Chấp nhận

test_chi_squared(expected = c(100, 100, 100, 100, 100, 100),
                 actual = c(106, 92, 97, 105, 88, 112), alpha = 0.05)
# Đáp số: T=4.22, c=11.070, Chấp nhận

test_2_mean_norm(n1 = 40, n2 = 50, mean1 = 130, mean2 = 140,
                 sigma1 = sqrt(80), sigma2 = sqrt(100),
                 alpha = 0.01, mode = "neq")
# Đáp số: T=5, c=2.58, Bác bỏ

test_2_mean_t(mode = "neq", alpha = 0.01, mean1 = 4.8, n1 = 10,
              s1 = 1.1, mean2 = 4.3, n2 = 12, s2 = 0.9)
# Đáp số: T=1.174, c=2.845, Chấp nhận

test_2_prop(f1 = 0.42, f2 = 0.46, n1 = 100, n2 = 200,
            alpha = 0.05, mode = "neq")
# Đáp số: T=-0.66, c=1.96, Chấp nhận

test_n_prop(m_i = c(79, 82, 77, 83, 76, 81),
            n_i = c(100, 100, 100, 100, 100, 100), alpha = 0.05)
# Đáp số: T=2.42. c=11.07. Chấp nhận

test_independent(matrix = matrix(data = c(328, 122, 77, 33),
                                 ncol = 2, nrow = 2), alpha = 0.05)
# Đáp số: T=0.368, c=3.841, Chấp nhận

correlation(x = c(80, 85, 88, 90, 95, 92, 82, 75, 78, 85),
            y = c(2.4, 2.8, 3.3, 3.1, 3.7, 3, 2.5, 2.3, 2.8, 3.1))
# Đáp số: 0.858

linear_regression(x = c(400, 600, 500, 600, 400, 500),
                  y = c(44, 47, 48, 48, 43, 46))
# Đáp số: y=0.02x + 36
```

# Giấy phép:
MIT License
