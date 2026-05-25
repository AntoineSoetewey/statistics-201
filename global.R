extract <- function(text) {
  text <- gsub(";", ",", text)
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = TRUE)[[1]]
  as.numeric(split)
}

t.test2 <- function(x, V, m0 = 0, alpha = 0.05, alternative = "two.sided") {
  M <- mean(x)
  n <- length(x)
  sigma <- sqrt(V)
  S <- sqrt(V / n)
  statistic <- (M - m0) / S
  p <- if (alternative == "two.sided") {
    2 * pnorm(abs(statistic), lower.tail = FALSE)
  } else if (alternative == "less") {
    pnorm(statistic, lower.tail = TRUE)
  } else {
    pnorm(statistic, lower.tail = FALSE)
  }
  LCL <- (M - S * qnorm(1 - alpha / 2))
  UCL <- (M + S * qnorm(1 - alpha / 2))
  value <- list(mean = M, m0 = m0, sigma = sigma, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
  return(value)
}

t.test3 <- function(x, y, V1, V2, m0 = 0, alpha = 0.05, alternative = "two.sided") {
  M1 <- mean(x)
  M2 <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  sigma1 <- sqrt(V1)
  sigma2 <- sqrt(V2)
  S <- sqrt((V1 / n1) + (V2 / n2))
  statistic <- (M1 - M2 - m0) / S
  p <- if (alternative == "two.sided") {
    2 * pnorm(abs(statistic), lower.tail = FALSE)
  } else if (alternative == "less") {
    pnorm(statistic, lower.tail = TRUE)
  } else {
    pnorm(statistic, lower.tail = FALSE)
  }
  LCL <- (M1 - M2 - S * qnorm(1 - alpha / 2))
  UCL <- (M1 - M2 + S * qnorm(1 - alpha / 2))
  value <- list(mean1 = M1, mean2 = M2, m0 = m0, sigma1 = sigma1, sigma2 = sigma2, S = S, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
  return(value)
}

prop.z.test <- function(x, n, p0 = 0.5, conf.level = 0.95, alternative = "two.sided") {
  ts.z <- NULL
  cint <- NULL
  p.val <- NULL
  phat <- x / n
  qhat <- 1 - phat
  SE.phat <- sqrt((phat * qhat) / n)
  ts.z <- (phat - p0) / SE.phat
  p.val <- if (alternative == "two.sided") {
    2 * pnorm(abs(ts.z), lower.tail = FALSE)
  } else if (alternative == "less") {
    pnorm(ts.z, lower.tail = TRUE)
  } else {
    pnorm(ts.z, lower.tail = FALSE)
  }
  cint <- phat + c(
    -1 * ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat),
    ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat)
  )
  return(list(x = x, n = n, estimate = phat, null.value = p0, stderr = SE.phat, statistic = ts.z, p.value = p.val, conf.int = cint))
}

prop.z.test2 <- function(x1, x2, n1, n2, p0 = 0, pooled.stderr = TRUE, conf.level = 0.95, alternative = "two.sided") {
  ts.z <- NULL
  cint <- NULL
  p.val <- NULL
  phat1 <- x1 / n1
  qhat1 <- 1 - phat1
  phat2 <- x2 / n2
  qhat2 <- 1 - phat2
  pooled.phat <- ((n1 * phat1) + (n2 * phat2)) / (n1 + n2)
  pooled.qhat <- 1 - pooled.phat
  if (pooled.stderr == FALSE) {
    SE.phat <- sqrt((phat1 * qhat1) / n1 + (phat2 * qhat2) / n2)
  } else {
    SE.phat <- sqrt(pooled.phat * pooled.qhat * (1 / n1 + 1 / n2))
  }
  ts.z <- (phat1 - phat2 - p0) / SE.phat
  p.val <- if (alternative == "two.sided") {
    2 * pnorm(abs(ts.z), lower.tail = FALSE)
  } else if (alternative == "less") {
    pnorm(ts.z, lower.tail = TRUE)
  } else {
    pnorm(ts.z, lower.tail = FALSE)
  }
  cint <- (phat1 - phat2) + c(
    -1 * ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat),
    ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat)
  )
  return(list(x1 = x1, x2 = x2, n1 = n1, n2 = n2, estimate1 = phat1, estimate2 = phat2, null.value = p0, stderr = SE.phat, pooled.phat = pooled.phat, statistic = ts.z, p.value = p.val, conf.int = cint))
}

prop.z.test3 <- function(x, n, p0 = 0.5, conf.level = 0.95, alternative = "two.sided") {
  ts.z <- NULL
  cint <- NULL
  p.val <- NULL
  phat <- x / n
  qhat <- 1 - phat
  SE.phat <- sqrt((p0 * (1 - p0)) / n)
  ts.z <- (phat - p0) / SE.phat
  p.val <- if (alternative == "two.sided") {
    2 * pnorm(abs(ts.z), lower.tail = FALSE)
  } else if (alternative == "less") {
    pnorm(ts.z, lower.tail = TRUE)
  } else {
    pnorm(ts.z, lower.tail = FALSE)
  }
  return(list(x = x, n = n, estimate = phat, null.value = p0, stderr = SE.phat, statistic = ts.z, p.value = p.val))
}

t_test1_sum <- function(n, xbar, s2, mu0, alpha, alternative) {
  s <- sqrt(s2)
  df <- n - 1
  SE <- s / sqrt(n)
  t_stat <- (xbar - mu0) / SE
  p_val <- if (alternative == "two.sided") 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
            else if (alternative == "less") pt(t_stat, df = df, lower.tail = TRUE)
            else pt(t_stat, df = df, lower.tail = FALSE)
  t_two <- qt(alpha / 2, df = df, lower.tail = FALSE)
  t_one <- qt(alpha, df = df, lower.tail = FALSE)
  ci <- if (alternative == "two.sided") c(xbar - t_two * SE, xbar + t_two * SE)
        else if (alternative == "greater") c(xbar - t_one * SE, Inf)
        else c(-Inf, xbar + t_one * SE)
  list(estimate = xbar, null.value = mu0, stderr = SE, statistic = t_stat,
       parameter = df, p.value = p_val, conf.int = ci, alternative = alternative)
}

t_test2_sum <- function(n1, n2, xbar1, xbar2, s21, s22, var_equal, mu0, alpha, alternative) {
  if (var_equal) {
    df <- n1 + n2 - 2
    SE <- sqrt(((n1 - 1) * s21 + (n2 - 1) * s22) / df) * sqrt(1 / n1 + 1 / n2)
  } else {
    df <- (s21 / n1 + s22 / n2)^2 / ((s21 / n1)^2 / (n1 - 1) + (s22 / n2)^2 / (n2 - 1))
    SE <- sqrt(s21 / n1 + s22 / n2)
  }
  t_stat <- (xbar1 - xbar2 - mu0) / SE
  p_val <- if (alternative == "two.sided") 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
            else if (alternative == "less") pt(t_stat, df = df, lower.tail = TRUE)
            else pt(t_stat, df = df, lower.tail = FALSE)
  t_two <- qt(alpha / 2, df = df, lower.tail = FALSE)
  t_one <- qt(alpha, df = df, lower.tail = FALSE)
  diff_est <- xbar1 - xbar2
  ci <- if (alternative == "two.sided") c(diff_est - t_two * SE, diff_est + t_two * SE)
        else if (alternative == "greater") c(diff_est - t_one * SE, Inf)
        else c(-Inf, diff_est + t_one * SE)
  list(estimate = c(xbar1, xbar2), null.value = mu0, stderr = SE, statistic = t_stat,
       parameter = df, p.value = p_val, conf.int = ci, alternative = alternative)
}

vartest_sum <- function(n, s2, sigma2_0, alpha, alternative) {
  df <- n - 1
  chi2_stat <- (n - 1) * s2 / sigma2_0
  p_val <- if (alternative == "two.sided") {
    2 * min(pchisq(chi2_stat, df = df, lower.tail = TRUE), pchisq(chi2_stat, df = df, lower.tail = FALSE))
  } else if (alternative == "less") {
    pchisq(chi2_stat, df = df, lower.tail = TRUE)
  } else {
    pchisq(chi2_stat, df = df, lower.tail = FALSE)
  }
  ci <- if (alternative == "two.sided") {
    c((n - 1) * s2 / qchisq(alpha / 2, df = df, lower.tail = FALSE),
      (n - 1) * s2 / qchisq(alpha / 2, df = df, lower.tail = TRUE))
  } else if (alternative == "greater") {
    c((n - 1) * s2 / qchisq(alpha, df = df, lower.tail = FALSE), Inf)
  } else {
    c(0, (n - 1) * s2 / qchisq(alpha, df = df, lower.tail = TRUE))
  }
  list(estimate = s2, null.value = sigma2_0, statistic = chi2_stat,
       parameters = df, p.value = p_val, conf.int = ci, alternative = alternative)
}

ftest_sum <- function(n1, n2, s21, s22, alpha, alternative) {
  df1 <- n1 - 1
  df2 <- n2 - 1
  F_stat <- s21 / s22
  p_val <- if (alternative == "two.sided") {
    2 * min(pf(F_stat, df1 = df1, df2 = df2, lower.tail = TRUE), pf(F_stat, df1 = df1, df2 = df2, lower.tail = FALSE))
  } else if (alternative == "greater") {
    pf(F_stat, df1 = df1, df2 = df2, lower.tail = FALSE)
  } else {
    pf(F_stat, df1 = df1, df2 = df2, lower.tail = TRUE)
  }
  f_upper <- qf(alpha / 2, df1 = df1, df2 = df2, lower.tail = FALSE)
  f_upper_rev <- qf(alpha / 2, df1 = df2, df2 = df1, lower.tail = FALSE)
  f_one <- qf(alpha, df1 = df1, df2 = df2, lower.tail = FALSE)
  f_one_rev <- qf(alpha, df1 = df2, df2 = df1, lower.tail = FALSE)
  ci <- if (alternative == "two.sided") c(F_stat / f_upper, F_stat * f_upper_rev)
        else if (alternative == "greater") c(F_stat / f_one, Inf)
        else c(0, F_stat * f_one_rev)
  list(estimate = F_stat, null.value = 1, statistic = F_stat,
       parameter = c(df1, df2), p.value = p_val, conf.int = ci, alternative = alternative)
}
