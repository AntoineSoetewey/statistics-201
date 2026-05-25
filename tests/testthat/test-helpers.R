library(testthat)

source(file.path("..", "..", "global.R"))

# --- extract ---

test_that("extract parses comma-separated numbers", {
  expect_equal(extract("1, 2, 3"), c(1, 2, 3))
})

test_that("extract handles semicolons as delimiters", {
  expect_equal(extract("1; 2; 3"), c(1, 2, 3))
})

test_that("extract handles decimal values", {
  expect_equal(extract("0.9, -0.8, 1.3"), c(0.9, -0.8, 1.3))
})

# --- t.test2 (one-sample z-test, known variance) ---

test_that("t.test2 statistic matches manual formula", {
  x <- c(1, 2, 3, 4, 5)
  result <- t.test2(x, V = 1, m0 = 3)
  expect_equal(result$statistic, (mean(x) - 3) / sqrt(1 / length(x)))
})

test_that("t.test2 two-sided p-value is symmetric", {
  x <- c(1, 2, 3, 4, 5)
  res_greater <- t.test2(x, V = 1, m0 = 2, alternative = "greater")
  res_less <- t.test2(x, V = 1, m0 = 2, alternative = "less")
  expect_equal(res_greater$statistic, res_less$statistic)
})

test_that("t.test2 CI contains the true mean under H0", {
  x <- rep(5, 10)
  result <- t.test2(x, V = 1, m0 = 5)
  expect_true(result$LCL <= 5 && 5 <= result$UCL)
})

# --- t.test3 (two-sample z-test, known variances) ---

test_that("t.test3 statistic is zero when means are equal", {
  x <- rep(3, 5)
  y <- rep(3, 5)
  result <- t.test3(x, y, V1 = 1, V2 = 1, m0 = 0)
  expect_equal(result$statistic, 0)
})

test_that("t.test3 statistic matches manual formula", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  V1 <- 1; V2 <- 1
  S <- sqrt(V1 / length(x) + V2 / length(y))
  expected_stat <- (mean(x) - mean(y) - 0) / S
  result <- t.test3(x, y, V1 = V1, V2 = V2, m0 = 0)
  expect_equal(result$statistic, expected_stat)
})

# --- prop.z.test (one-proportion z-test using SE based on phat) ---

test_that("prop.z.test estimate equals x/n", {
  result <- prop.z.test(x = 20, n = 100, p0 = 0.2)
  expect_equal(result$estimate, 0.2)
})

test_that("prop.z.test p-value is 1 when phat equals p0 exactly", {
  result <- prop.z.test(x = 20, n = 100, p0 = 0.2)
  expect_equal(result$statistic, 0)
  expect_equal(result$p.value, 1)
})

test_that("prop.z.test CI is symmetric around phat", {
  result <- prop.z.test(x = 30, n = 100, p0 = 0.2, conf.level = 0.95)
  phat <- 30 / 100
  expect_equal(result$conf.int[1] + result$conf.int[2], 2 * phat)
})

# --- prop.z.test2 (two-proportion z-test) ---

test_that("prop.z.test2 estimates match x/n inputs", {
  result <- prop.z.test2(x1 = 20, x2 = 30, n1 = 100, n2 = 100)
  expect_equal(result$estimate1, 0.2)
  expect_equal(result$estimate2, 0.3)
})

test_that("prop.z.test2 pooled stderr equals unpooled when proportions are equal", {
  res_pooled <- prop.z.test2(x1 = 25, x2 = 25, n1 = 100, n2 = 100, pooled.stderr = TRUE)
  res_unpooled <- prop.z.test2(x1 = 25, x2 = 25, n1 = 100, n2 = 100, pooled.stderr = FALSE)
  expect_equal(res_pooled$stderr, res_unpooled$stderr)
})

test_that("prop.z.test2 statistic is zero when proportions are equal", {
  result <- prop.z.test2(x1 = 25, x2 = 25, n1 = 100, n2 = 100, p0 = 0)
  expect_equal(result$statistic, 0)
})

# --- prop.z.test3 (one-proportion z-test using SE based on p0) ---

test_that("prop.z.test3 SE uses p0 not phat", {
  result <- prop.z.test3(x = 20, n = 100, p0 = 0.3)
  expected_SE <- sqrt(0.3 * 0.7 / 100)
  expect_equal(result$stderr, expected_SE)
})

test_that("prop.z.test3 statistic matches manual computation", {
  result <- prop.z.test3(x = 40, n = 100, p0 = 0.3)
  expected_stat <- (0.4 - 0.3) / sqrt(0.3 * 0.7 / 100)
  expect_equal(result$statistic, expected_stat)
})

# --- t_test1_sum (one-sample t-test from summary statistics) ---

test_that("t_test1_sum matches t.test on raw data", {
  dat <- c(0.9, -0.8, 1.3, -0.3, 1.7)
  raw <- t.test(dat, mu = 0, alternative = "two.sided", conf.level = 0.95)
  summ <- t_test1_sum(n = length(dat), xbar = mean(dat), s2 = var(dat),
                       mu0 = 0, alpha = 0.05, alternative = "two.sided")
  expect_equal(round(unname(summ$statistic), 6), round(unname(raw$statistic), 6))
  expect_equal(round(summ$p.value, 6), round(raw$p.value, 6))
  expect_equal(round(as.vector(summ$conf.int), 4), round(as.vector(raw$conf.int), 4))
})

test_that("t_test1_sum one-sided greater CI has Inf upper bound", {
  summ <- t_test1_sum(n = 10, xbar = 2, s2 = 1, mu0 = 0, alpha = 0.05, alternative = "greater")
  expect_equal(summ$conf.int[2], Inf)
})

test_that("t_test1_sum one-sided less CI has -Inf lower bound", {
  summ <- t_test1_sum(n = 10, xbar = 2, s2 = 1, mu0 = 0, alpha = 0.05, alternative = "less")
  expect_equal(summ$conf.int[1], -Inf)
})

# --- t_test2_sum (two-sample t-test from summary statistics) ---

test_that("t_test2_sum equal-variance matches t.test on raw data", {
  dat1 <- c(0.9, -0.8, 0.1, -0.3, 0.2)
  dat2 <- c(0.8, -0.9, -0.1, 0.4, 0.1)
  raw <- t.test(dat1, dat2, mu = 0, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)
  summ <- t_test2_sum(length(dat1), length(dat2), mean(dat1), mean(dat2),
                       var(dat1), var(dat2), TRUE, 0, 0.05, "two.sided")
  expect_equal(round(unname(summ$statistic), 5), round(unname(raw$statistic), 5))
  expect_equal(round(summ$p.value, 5), round(raw$p.value, 5))
})

test_that("t_test2_sum unequal-variance matches t.test (Welch) on raw data", {
  dat1 <- c(1, 2, 3, 4, 5)
  dat2 <- c(3, 3, 3, 3, 3)
  raw <- t.test(dat1, dat2, var.equal = FALSE, alternative = "two.sided", conf.level = 0.95)
  summ <- t_test2_sum(length(dat1), length(dat2), mean(dat1), mean(dat2),
                       var(dat1), var(dat2), FALSE, 0, 0.05, "two.sided")
  expect_equal(round(unname(summ$statistic), 5), round(unname(raw$statistic), 5))
  expect_equal(round(summ$p.value, 5), round(raw$p.value, 5))
})

# --- vartest_sum (chi-squared variance test from summary statistics) ---

test_that("vartest_sum statistic matches manual formula", {
  n <- 10; s2 <- 2; sigma2_0 <- 1
  result <- vartest_sum(n, s2, sigma2_0, alpha = 0.05, alternative = "two.sided")
  expect_equal(result$statistic, (n - 1) * s2 / sigma2_0)
})

test_that("vartest_sum two-sided p-value is in [0, 1]", {
  result <- vartest_sum(20, 1.5, 1, alpha = 0.05, alternative = "two.sided")
  expect_true(result$p.value >= 0 && result$p.value <= 1)
})

test_that("vartest_sum greater CI has Inf upper bound", {
  result <- vartest_sum(10, 2, 1, alpha = 0.05, alternative = "greater")
  expect_equal(result$conf.int[2], Inf)
})

test_that("vartest_sum less CI has 0 lower bound", {
  result <- vartest_sum(10, 2, 1, alpha = 0.05, alternative = "less")
  expect_equal(result$conf.int[1], 0)
})

# --- ftest_sum (F-test for two variances from summary statistics) ---

test_that("ftest_sum statistic equals s21/s22", {
  result <- ftest_sum(10, 10, s21 = 4, s22 = 2, alpha = 0.05, alternative = "two.sided")
  expect_equal(result$statistic, 4 / 2)
})

test_that("ftest_sum two-sided p-value is in [0, 1]", {
  result <- ftest_sum(10, 10, s21 = 2, s22 = 1, alpha = 0.05, alternative = "two.sided")
  expect_true(result$p.value >= 0 && result$p.value <= 1)
})

test_that("ftest_sum greater CI has Inf upper bound", {
  result <- ftest_sum(10, 10, s21 = 2, s22 = 1, alpha = 0.05, alternative = "greater")
  expect_equal(result$conf.int[2], Inf)
})

test_that("ftest_sum less CI has 0 lower bound", {
  result <- ftest_sum(10, 10, s21 = 2, s22 = 1, alpha = 0.05, alternative = "less")
  expect_equal(result$conf.int[1], 0)
})

test_that("ftest_sum matches var.test on raw data", {
  dat1 <- c(0.9, -0.8, 0.1, -0.3, 0.2, 0.7, -0.8, 0.1, -0.3, 0.2)
  dat2 <- c(0.4, -0.3, -0.1, 0.4, 0.1, 0.2, -0.1, -0.1, 0.4, 0.1)
  raw <- var.test(dat1, dat2, alternative = "two.sided", conf.level = 0.95)
  summ <- ftest_sum(length(dat1), length(dat2), var(dat1), var(dat2),
                    alpha = 0.05, alternative = "two.sided")
  expect_equal(round(unname(summ$statistic), 5), round(unname(raw$statistic), 5))
  expect_equal(round(summ$p.value, 5), round(raw$p.value, 5))
})
