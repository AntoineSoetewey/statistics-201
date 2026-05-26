library(testthat)

source(file.path("..", "..", "global.R"))

# Tests for the app-specific logic in global.R.
# The focus is on non-trivial choices made by the app: the extract() input
# parser, the z-test functions that use a known variance instead of the sample
# variance, the two-proportion SE switch (pooled vs unpooled), the deliberate
# use of p0-based SE for hypothesis testing vs phat-based SE for the CI, the
# Welch-Satterthwaite df formula, and the summary-statistics paths that must
# reproduce the same results as R's built-in functions on raw data.

# --- extract ---

test_that("extract parses comma-separated numbers", {
  expect_equal(extract("1, 2, 3"), c(1, 2, 3))
})

test_that("extract treats semicolons as comma equivalents", {
  # The app accepts both ',' and ';' as separators
  expect_equal(extract("1; 2; 3"), c(1, 2, 3))
})

test_that("extract handles negative values and decimals", {
  expect_equal(extract("0.9, -0.8, 1.3"), c(0.9, -0.8, 1.3))
})

# --- t.test2: one-sample z-test using known population variance ---
# The app uses t.test2() when the user provides sigma^2 (known variance).
# SE = sqrt(V/n), NOT sqrt(s^2/n) — this is the key difference from t.test().

test_that("t.test2 uses the known variance V for SE, not the sample variance", {
  dat <- c(2, 3, 5, 7, 8)    # mean = 5, but shift m0 so stat != 0
  V_known <- 1                # var(dat) = 6.5, so SEs clearly differ
  result <- t.test2(dat, V = V_known, m0 = 3)
  expect_equal(result$statistic, (mean(dat) - 3) / sqrt(V_known / length(dat)))
  # Confirm it does NOT use sample variance
  expect_false(isTRUE(all.equal(
    result$statistic,
    (mean(dat) - 3) / sqrt(var(dat) / length(dat))
  )))
})

test_that("t.test2 two-sided p-value is consistent with the z-distribution", {
  dat <- c(1, 2, 3, 4, 5)
  result <- t.test2(dat, V = 1, m0 = 3, alternative = "two.sided")
  expected_p <- 2 * pnorm(abs(result$statistic), lower.tail = FALSE)
  expect_equal(result$p.value, expected_p)
})

# --- t.test3: two-sample z-test using known population variances ---

test_that("t.test3 uses known variances V1 and V2, not sample variances", {
  x <- c(1, 2, 3); y <- c(4, 5, 6)
  V1 <- 0.5; V2 <- 0.5
  result <- t.test3(x, y, V1 = V1, V2 = V2, m0 = 0)
  S_known <- sqrt(V1 / length(x) + V2 / length(y))
  expect_equal(result$statistic, (mean(x) - mean(y)) / S_known)
  # Confirm it does NOT use sample variances
  S_sample <- sqrt(var(x) / length(x) + var(y) / length(y))
  expect_false(isTRUE(all.equal(result$statistic, (mean(x) - mean(y)) / S_sample)))
})

# --- prop.z.test vs prop.z.test3: two SE formulas for CI vs hypothesis test ---
# The app intentionally uses different SEs for the CI (phat-based) and the
# hypothesis test statistic (p0-based). This is the standard approach:
# CI uses SE = sqrt(phat*(1-phat)/n), hypothesis test uses SE = sqrt(p0*(1-p0)/n).

test_that("prop.z.test (CI) uses phat-based SE, prop.z.test3 (HT) uses p0-based SE", {
  x <- 30; n <- 100; p0 <- 0.5    # phat = 0.3, p0 = 0.5
  ci_result <- prop.z.test(x, n, p0)
  ht_result <- prop.z.test3(x, n, p0)
  # These SEs must differ because phat (0.3) != p0 (0.5)
  expect_false(isTRUE(all.equal(ci_result$stderr, ht_result$stderr)))
  expect_equal(ci_result$stderr, sqrt(0.3 * 0.7 / 100))
  expect_equal(ht_result$stderr, sqrt(0.5 * 0.5 / 100))
})

test_that("prop.z.test and prop.z.test3 agree when phat equals p0", {
  # When phat = p0 the two SE formulas coincide
  x <- 50; n <- 100; p0 <- 0.5    # phat = 0.5 = p0
  ci_result <- prop.z.test(x, n, p0)
  ht_result <- prop.z.test3(x, n, p0)
  expect_equal(ci_result$stderr, ht_result$stderr)
})

# --- prop.z.test2: pooled vs unpooled SE ---
# The app lets users toggle between pooled and unpooled standard error.
# These must produce different statistics when the two proportions differ.

test_that("prop.z.test2 pooled and unpooled SE differ when proportions are unequal", {
  res_pooled   <- prop.z.test2(x1 = 20, x2 = 50, n1 = 100, n2 = 100, pooled.stderr = TRUE)
  res_unpooled <- prop.z.test2(x1 = 20, x2 = 50, n1 = 100, n2 = 100, pooled.stderr = FALSE)
  expect_false(isTRUE(all.equal(res_pooled$stderr, res_unpooled$stderr)))
  expect_false(isTRUE(all.equal(res_pooled$statistic, res_unpooled$statistic)))
})

test_that("prop.z.test2 pooled SE uses the combined proportion phat", {
  x1 <- 20; x2 <- 40; n1 <- 100; n2 <- 100
  result <- prop.z.test2(x1, x2, n1, n2, pooled.stderr = TRUE)
  phat_pooled <- (x1 + x2) / (n1 + n2)
  expected_SE <- sqrt(phat_pooled * (1 - phat_pooled) * (1 / n1 + 1 / n2))
  expect_equal(result$stderr, expected_SE)
})

# --- t_test1_sum: one-sample t-test from summary statistics ---
# The app offers a "summary statistics" input mode; this path must reproduce
# identical results to R's t.test() applied to the corresponding raw data.

test_that("t_test1_sum from summary statistics matches t.test on raw data", {
  dat <- c(0.9, -0.8, 1.3, -0.3, 1.7)
  raw  <- t.test(dat, mu = 0, alternative = "two.sided", conf.level = 0.95)
  summ <- t_test1_sum(n = length(dat), xbar = mean(dat), s2 = var(dat),
                      mu0 = 0, alpha = 0.05, alternative = "two.sided")
  expect_equal(round(unname(summ$statistic), 6), round(unname(raw$statistic), 6))
  expect_equal(round(summ$p.value, 6),           round(raw$p.value, 6))
  expect_equal(round(as.vector(summ$conf.int), 4), round(as.vector(raw$conf.int), 4))
})

test_that("t_test1_sum one-sided CIs use Inf bounds correctly", {
  summ_gr <- t_test1_sum(n = 10, xbar = 2, s2 = 1, mu0 = 0, alpha = 0.05, alternative = "greater")
  summ_le <- t_test1_sum(n = 10, xbar = 2, s2 = 1, mu0 = 0, alpha = 0.05, alternative = "less")
  expect_equal(summ_gr$conf.int[2],  Inf)
  expect_equal(summ_le$conf.int[1], -Inf)
})

# --- t_test2_sum: two-sample t-test from summary statistics ---

test_that("t_test2_sum equal-variance from summary statistics matches t.test on raw data", {
  dat1 <- c(0.9, -0.8, 0.1, -0.3, 0.2)
  dat2 <- c(0.8, -0.9, -0.1, 0.4, 0.1)
  raw  <- t.test(dat1, dat2, mu = 0, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)
  summ <- t_test2_sum(length(dat1), length(dat2), mean(dat1), mean(dat2),
                      var(dat1), var(dat2), TRUE, 0, 0.05, "two.sided")
  expect_equal(round(unname(summ$statistic), 5), round(unname(raw$statistic), 5))
  expect_equal(round(summ$p.value, 5),            round(raw$p.value, 5))
})

test_that("t_test2_sum Welch degrees of freedom differ from pooled df", {
  # The app uses the Welch-Satterthwaite formula, not simply n1+n2-2.
  # The two paths must give different df when variances are very unequal.
  n1 <- 10; n2 <- 15; s21 <- 4; s22 <- 0.1
  pooled <- t_test2_sum(n1, n2, 0, 0, s21, s22, TRUE,  0, 0.05, "two.sided")
  welch  <- t_test2_sum(n1, n2, 0, 0, s21, s22, FALSE, 0, 0.05, "two.sided")
  expect_equal(pooled$parameter, n1 + n2 - 2)
  expect_false(isTRUE(all.equal(welch$parameter, pooled$parameter)))
})

test_that("t_test2_sum Welch (unequal-variance) matches t.test on raw data", {
  dat1 <- c(1, 2, 3, 4, 5)
  dat2 <- c(3, 3, 3, 3, 3)
  raw  <- t.test(dat1, dat2, var.equal = FALSE, alternative = "two.sided", conf.level = 0.95)
  summ <- t_test2_sum(length(dat1), length(dat2), mean(dat1), mean(dat2),
                      var(dat1), var(dat2), FALSE, 0, 0.05, "two.sided")
  expect_equal(round(unname(summ$statistic), 5), round(unname(raw$statistic), 5))
  expect_equal(round(summ$p.value, 5),            round(raw$p.value, 5))
})

# --- vartest_sum: chi-squared variance test from summary statistics ---

test_that("vartest_sum from summary statistics matches varTest on raw data", {
  dat  <- c(0.9, -0.8, 0.1, -0.3, 0.2)
  raw  <- EnvStats::varTest(dat, sigma.squared = 1, alternative = "two.sided")
  summ <- vartest_sum(length(dat), var(dat), 1, 0.05, "two.sided")
  expect_equal(round(summ$statistic, 5), round(unname(raw$statistic), 5))
  expect_equal(round(summ$p.value, 5),   round(raw$p.value, 5))
})

test_that("vartest_sum one-sided CIs use Inf and 0 bounds correctly", {
  res_gr <- vartest_sum(10, 2, 1, alpha = 0.05, alternative = "greater")
  res_le <- vartest_sum(10, 2, 1, alpha = 0.05, alternative = "less")
  expect_equal(res_gr$conf.int[2], Inf)
  expect_equal(res_le$conf.int[1], 0)
})

# --- ftest_sum: F-test for two variances from summary statistics ---

test_that("ftest_sum from summary statistics matches var.test on raw data", {
  dat1 <- c(0.9, -0.8, 0.1, -0.3, 0.2, 0.7, -0.8, 0.1, -0.3, 0.2)
  dat2 <- c(0.4, -0.3, -0.1, 0.4, 0.1, 0.2, -0.1, -0.1, 0.4, 0.1)
  raw  <- var.test(dat1, dat2, alternative = "two.sided", conf.level = 0.95)
  summ <- ftest_sum(length(dat1), length(dat2), var(dat1), var(dat2),
                    alpha = 0.05, alternative = "two.sided")
  expect_equal(round(unname(summ$statistic), 5), round(unname(raw$statistic), 5))
  expect_equal(round(summ$p.value, 5),            round(raw$p.value, 5))
})

test_that("ftest_sum one-sided CIs use Inf and 0 bounds correctly", {
  res_gr <- ftest_sum(10, 10, s21 = 2, s22 = 1, alpha = 0.05, alternative = "greater")
  res_le <- ftest_sum(10, 10, s21 = 2, s22 = 1, alpha = 0.05, alternative = "less")
  expect_equal(res_gr$conf.int[2], Inf)
  expect_equal(res_le$conf.int[1], 0)
})
