library(shiny)
library(ggplot2)
library(EnvStats)

options(shiny.mathjax.config = "config=TeX-AMS-MML_SVG")

# Define UI for statistical inference application
ui <- shiny::tagList(
  withMathJax(), 
  includeCSS(path = "www/css/styles.css"), 
  
  tags$head(
    tags$link(
      rel = "shortcut icon", 
      href = "https://antoinesoetewey.com/favicon.ico"
    )
  ), 
  
  tags$div(
    tags$div(
      class = "app_title", 
      
      titlePanel(
        title = "Statistics 201 - Inference", 
        windowTitle = "Inference"
      ),
      
      tags$h4(
        tags$a(
          href = "https://antoinesoetewey.com/", 
          target = "_blank", 
          "Antoine Soetewey"
        )
      )
    ),
    
    # Sidebar with a slider input for number of bins
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "inference",
            label = "Inference for:",
            choices = c("one mean", "two means (independent samples)", "two means (paired samples)", "one proportion", "two proportions", "one variance", "two variances"),
            multiple = FALSE,
            selected = "one mean"
          ),
          hr(),
          conditionalPanel(
            condition = "input.inference == 'one mean'",
            radioButtons("input_type_onemean", "Input type:",
              choices = c("Raw data" = "raw", "Summary statistics" = "summary"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.input_type_onemean == 'raw'",
              textInput("sample_onemean", "Sample", value = "0.9, -0.8, 1.3, -0.3, 1.7", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc.")
            ),
            conditionalPanel(
              condition = "input.input_type_onemean == 'summary'",
              numericInput("n_onemean_sum", "\\(n = \\)", value = 5, min = 2, step = 1),
              numericInput("xbar_onemean_sum", "\\(\\bar{x} = \\)", value = 0.56, step = 0.01),
              conditionalPanel(
                condition = "input.popsd_onemean == 0",
                numericInput("s2_onemean_sum", "\\(s^2 = \\)", value = 1.063, min = 0, step = 0.01)
              )
            ),
            hr(),
            checkboxInput("popsd_onemean", "Variance of the population is known", FALSE),
            conditionalPanel(
              condition = "input.popsd_onemean == 1",
              numericInput("sigma2_onemean", "\\(\\sigma^2 = \\)",
                           value = 1, min = 0, step = 1
              )
            )
          ),
          conditionalPanel(
            condition = "input.inference == 'two means (independent samples)'",
            radioButtons("input_type_twomeans", "Input type:",
              choices = c("Raw data" = "raw", "Summary statistics" = "summary"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.input_type_twomeans == 'raw'",
              textInput("sample1_twomeans", "Sample 1", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
              textInput("sample2_twomeans", "Sample 2", value = "0.8, -0.9, -0.1, 0.4, 0.1", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc.")
            ),
            conditionalPanel(
              condition = "input.input_type_twomeans == 'summary'",
              numericInput("n1_twomeans_sum", "\\(n_1 = \\)", value = 5, min = 2, step = 1),
              numericInput("n2_twomeans_sum", "\\(n_2 = \\)", value = 5, min = 2, step = 1),
              numericInput("xbar1_twomeans_sum", "\\(\\bar{x}_1 = \\)", value = 0.22, step = 0.01),
              numericInput("xbar2_twomeans_sum", "\\(\\bar{x}_2 = \\)", value = 0.06, step = 0.01),
              conditionalPanel(
                condition = "input.popsd_twomeans == 0",
                numericInput("s21_twomeans_sum", "\\(s^2_1 = \\)", value = 0.277, min = 0, step = 0.01),
                numericInput("s22_twomeans_sum", "\\(s^2_2 = \\)", value = 0.212, min = 0, step = 0.01)
              )
            ),
            hr(),
            conditionalPanel(
              condition = "input.popsd_twomeans == 0",
              radioButtons(
                inputId = "var.equal",
                label = "Assuming",
                choices = c(
                  "\\( \\sigma^2_1 = \\sigma^2_2 \\)" = TRUE,
                  "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = FALSE
                )
              )
            ),
            checkboxInput("popsd_twomeans", "Variances of the populations are known", FALSE),
            conditionalPanel(
              condition = "input.popsd_twomeans == 1",
              numericInput("sigma21_twomeans", "\\(\\sigma^2_1 = \\)",
                           value = 1, min = 0, step = 1
              ),
              numericInput("sigma22_twomeans", "\\(\\sigma^2_2 = \\)",
                           value = 1, min = 0, step = 1
              )
            )
          ),
          conditionalPanel(
            condition = "input.inference == 'two means (paired samples)'",
            radioButtons("input_type_twomeanspaired", "Input type:",
              choices = c("Raw data" = "raw", "Summary statistics" = "summary"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.input_type_twomeanspaired == 'raw'",
              textInput("sample1_twomeanspaired", "Sample 1", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
              textInput("sample2_twomeanspaired", "Sample 2", value = "0.8, -0.9, -0.1, 0.4, 0.1", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc.")
            ),
            conditionalPanel(
              condition = "input.input_type_twomeanspaired == 'summary'",
              numericInput("n_twomeanspaired_sum", "\\(n = \\)", value = 5, min = 2, step = 1),
              numericInput("dbar_twomeanspaired_sum", "\\(\\bar{D} = \\)", value = 0.04, step = 0.01),
              conditionalPanel(
                condition = "input.popsd_twomeanspaired == 0",
                numericInput("s2d_twomeanspaired_sum", "\\(s^2_D = \\)", value = 0.133, min = 0, step = 0.001)
              )
            ),
            hr(),
            checkboxInput("popsd_twomeanspaired", "\\( \\sigma^2_D \\) is known", FALSE),
            conditionalPanel(
              condition = "input.popsd_twomeanspaired == 1",
              numericInput("sigma2_twomeanspaired", "\\(\\sigma^2_D = \\)",
                           value = 1, min = 0, step = 1
              )
            )
          ),
          conditionalPanel(
            condition = "input.inference == 'one proportion'",
            numericInput("n_oneprop", "\\(n = \\)",
                         value = 30, min = 1, step = 1
            ),
            hr(),
            radioButtons(
              inputId = "propx_oneprop",
              label = NULL,
              choices = c(
                "Proportion of success \\(\\widehat{p}\\)" = "prop_true",
                "Number of successes \\(x\\)" = "prop_false"
              )
            ),
            conditionalPanel(
              condition = "input.propx_oneprop == 'prop_true'",
              numericInput("p_oneprop", "\\(\\widehat{p} = \\)",
                           value = 0.2, min = 0, max = 1, step = 0.01
              )
            ),
            conditionalPanel(
              condition = "input.propx_oneprop == 'prop_false'",
              numericInput("x_oneprop", "\\(x = \\)",
                           value = 10, min = 0, step = 1
              )
            )
          ),
          conditionalPanel(
            condition = "input.inference == 'two proportions'",
            numericInput("n1_twoprop", "\\(n_1 = \\)",
                         value = 30, min = 1, step = 1
            ),
            numericInput("n2_twoprop", "\\(n_2 = \\)",
                         value = 30, min = 1, step = 1
            ),
            hr(),
            radioButtons(
              inputId = "propx_twoprop",
              label = NULL,
              choices = c(
                "Proportion of success \\(\\widehat{p}\\)" = "prop_true",
                "Number of successes \\(x\\)" = "prop_false"
              )
            ),
            conditionalPanel(
              condition = "input.propx_twoprop == 'prop_true'",
              numericInput("p1_twoprop", "\\(\\widehat{p}_1 = \\)",
                           value = 0.2, min = 0, max = 1, step = 0.01
              ),
              numericInput("p2_twoprop", "\\(\\widehat{p}_2 = \\)",
                           value = 0.3, min = 0, max = 1, step = 0.01
              )
            ),
            conditionalPanel(
              condition = "input.propx_twoprop == 'prop_false'",
              numericInput("x1_twoprop", "\\(x_1 = \\)",
                           value = 10, min = 0, step = 1
              ),
              numericInput("x2_twoprop", "\\(x_2 = \\)",
                           value = 12, min = 0, step = 1
              )
            )
          ),
          conditionalPanel(
            condition = "input.inference == 'one variance'",
            radioButtons("input_type_onevar", "Input type:",
              choices = c("Raw data" = "raw", "Summary statistics" = "summary"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.input_type_onevar == 'raw'",
              textInput("sample_onevar", "Sample", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 795, 810, 775, 781, 803, 823, 780, etc.")
            ),
            conditionalPanel(
              condition = "input.input_type_onevar == 'summary'",
              numericInput("n_onevar_sum", "\\(n = \\)", value = 5, min = 2, step = 1),
              numericInput("s2_onevar_sum", "\\(s^2 = \\)", value = 0.277, min = 0, step = 0.001)
            )
          ),
          conditionalPanel(
            condition = "input.inference == 'two variances'",
            radioButtons("input_type_twovar", "Input type:",
              choices = c("Raw data" = "raw", "Summary statistics" = "summary"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.input_type_twovar == 'raw'",
              textInput("sample1_twovar", "Sample 1", value = "0.9, -0.8, 0.1, -0.3, 0.2, 0.7, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
              textInput("sample2_twovar", "Sample 2", value = "0.4, -0.3, -0.1, 0.4, 0.1, 0.2, -0.1, -0.1, 0.4, 0.1", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc.")
            ),
            conditionalPanel(
              condition = "input.input_type_twovar == 'summary'",
              numericInput("n1_twovar_sum", "\\(n_1 = \\)", value = 10, min = 2, step = 1),
              numericInput("n2_twovar_sum", "\\(n_2 = \\)", value = 10, min = 2, step = 1),
              numericInput("s21_twovar_sum", "\\(s^2_1 = \\)", value = 0.237, min = 0, step = 0.001),
              numericInput("s22_twovar_sum", "\\(s^2_2 = \\)", value = 0.059, min = 0, step = 0.001)
            )
          ),
          hr(),
          tags$b("Null hypothesis"),
          conditionalPanel(
            condition = "input.inference == 'one mean'",
            sprintf("\\( H_0 : \\mu = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'two means (independent samples)'",
            sprintf("\\( H_0 : \\mu_1 - \\mu_2 = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'two means (paired samples)'",
            sprintf("\\( H_0 : \\mu_D = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'one proportion'",
            sprintf("\\( H_0 : p = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'two proportions'",
            sprintf("\\( H_0 : p_1 - p_2 = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'one variance'",
            sprintf("\\( H_0 : \\sigma^2 = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'two variances'",
            sprintf("\\( H_0 : \\sigma^2_1 = \\sigma^2_2 \\)")
          ),
          conditionalPanel(
            condition = "input.inference != 'two variances'",
            numericInput("h0",
                         label = NULL,
                         value = 0.1, step = 0.1
            )
          ),
          conditionalPanel(
            condition = "input.inference == 'two variances'",
            br()
          ),
          conditionalPanel(
            condition = "input.inference == 'two proportions'",
            checkboxInput("pooledstderr_twoprop", "Use pooled standard error", FALSE)
          ),
          conditionalPanel(
            condition = "input.inference != 'two variances'",
            radioButtons(
              inputId = "alternative",
              label = "Alternative",
              choices = c(
                "\\( \\neq \\)" = "two.sided",
                "\\( > \\)" = "greater",
                "\\( < \\)" = "less"
              )
            )
          ),
          conditionalPanel(
            condition = "input.inference == 'two variances'",
            radioButtons(
              inputId = "alternative_twovar",
              label = "Alternative",
              choices = c(
                "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = "two.sided",
                "\\( \\sigma^2_1 > \\sigma^2_2 \\)" = "greater",
                "\\( \\sigma^2_1 < \\sigma^2_2 \\)" = "less"
              )
            )
          ),
          hr(),
          sliderInput("alpha",
                      "Significance level \\(\\alpha = \\)",
                      min = 0.01,
                      max = 0.20,
                      value = 0.05
          ),
        ),
        
        mainPanel(
          br(),
          conditionalPanel(
            condition = "input.inference == 'one mean'",
            uiOutput("results_onemean")
          ),
          conditionalPanel(
            condition = "input.inference == 'two means (independent samples)'",
            uiOutput("results_twomeans")
          ),
          conditionalPanel(
            condition = "input.inference == 'two means (paired samples)'",
            uiOutput("results_twomeanspaired")
          ),
          conditionalPanel(
            condition = "input.inference == 'one proportion'",
            uiOutput("results_oneprop")
          ),
          conditionalPanel(
            condition = "input.inference == 'two proportions'",
            uiOutput("results_twoprop")
          ),
          conditionalPanel(
            condition = "input.inference == 'one variance'",
            uiOutput("results_onevar")
          ),
          conditionalPanel(
            condition = "input.inference == 'two variances'",
            uiOutput("results_twovar")
          ),
          br(),
          br(),
          plotOutput("plot"),
          br(),
          br()
        )
      ),
    )      
  ), 
  
  tags$footer(
    tags$div(
      class = "footer_container", 
      
      includeHTML(path = "www/html/footer.html")
    )
  )
  
)
  


server <- function(input, output) {
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
    SE.phat <- sqrt((p0 * (1-p0)) / n)
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

  # Helper: one-sample t-test from summary statistics
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

  # Helper: two-sample t-test from summary statistics
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

  # Helper: chi-squared variance test from summary statistics
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

  # Helper: F-test for two variances from summary statistics
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

  output$results_onemean <- renderUI({
    # Extract summary statistics based on input type
    if (input$input_type_onemean == "raw") {
      dat <- extract(input$sample_onemean)
      if (anyNA(dat) | length(dat) < 2) return("Invalid input or not enough observations")
      n_val <- length(dat)
      xbar_val <- mean(dat)
      s_val <- sd(dat)
      s2_val <- var(dat)
    } else {
      n_val <- input$n_onemean_sum
      xbar_val <- input$xbar_onemean_sum
      s2_val <- input$s2_onemean_sum
      s_val <- sqrt(max(s2_val, 0))
      if (is.na(n_val) | n_val < 2) return("Invalid input: n must be \u2265 2")
    }

    if (input$inference == "one mean" & input$popsd_onemean == FALSE) {
      if (input$input_type_onemean == "raw") {
        test_confint <- t.test(x = dat, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha)
        test <- t.test(x = dat, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      } else {
        if (is.na(s2_val) | s2_val < 0) return("Invalid input: s\u00b2 must be \u2265 0")
        test_confint <- t_test1_sum(n_val, xbar_val, s2_val, input$h0, input$alpha, "two.sided")
        test <- t_test1_sum(n_val, xbar_val, s2_val, input$h0, input$alpha, input$alternative)
      }
      withMathJax(
        tags$b("Data"),
        br(),
        if (input$input_type_onemean == "raw") paste(c(paste(dat, collapse = ", ")), collapse = " "),
        if (input$input_type_onemean == "raw") br(),
        paste0("\\(n =\\) ", n_val),
        br(),
        paste0("\\(\\bar{x} =\\) ", round(xbar_val, 3)),
        br(),
        paste0("\\(s =\\) ", round(s_val, 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0(
          (1 - input$alpha) * 100,
          ifelse(input$alternative == "two.sided",
            paste0("% CI for \\(\\mu = \\bar{x} \\pm t_{\\alpha/2, n - 1} \\dfrac{s}{\\sqrt{n}} = \\) ", round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(s_val, 3), " / ", round(sqrt(n_val), 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0("% CI for \\(\\mu = \\bar{x} - t_{\\alpha, n - 1} \\dfrac{s}{\\sqrt{n}} = \\) ", round(test$estimate, 3), "  \\( - \\) ", "\\( ( \\)", round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3), " * ", round(s_val, 3), " / ", round(sqrt(n_val), 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test$conf.int[1], 3), "; \\(+\\infty\\))"),
              paste0("% CI for \\(\\mu = \\bar{x} + t_{\\alpha, n - 1} \\dfrac{s}{\\sqrt{n}} = \\) ", round(test$estimate, 3), "  \\( + \\) ", "\\( ( \\)", round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3), " * ", round(s_val, 3), " / ", round(sqrt(n_val), 3), "\\( ) \\) ", "\\( = \\) ", "(-\\(\\infty\\); ", round(test$conf.int[2], 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu = \\) ", test$null.value, " and \\(H_1 : \\mu \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = \\) ",
          "(", round(test$estimate, 3), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", ifelse(input$alternative == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", "we do not reject the null hypothesis that the true mean is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "one mean" & input$popsd_onemean == TRUE) {
      if (input$input_type_onemean == "raw") {
        test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      } else {
        test <- t.test2(x = rep(xbar_val, n_val), V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      }
      withMathJax(
        tags$b("Data"),
        br(),
        if (input$input_type_onemean == "raw") paste(c(paste(dat, collapse = ", ")), collapse = " "),
        if (input$input_type_onemean == "raw") br(),
        paste0("\\(n =\\) ", n_val),
        br(),
        paste0("\\(\\bar{x} =\\) ", round(xbar_val, 3)),
        br(),
        paste0("\\(\\sigma =\\) ", round(sqrt(input$sigma2_onemean), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        tags$em("(two-sided)"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% Confidence Interval for \\(\\mu = \\bar{x} \\pm z_{\\alpha/2} \\dfrac{\\sigma}{\\sqrt{n}} = \\) ",
          round(test$mean, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$sigma, 3), " / ", round(sqrt(n_val), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu = \\) ", input$h0, " and \\(H_1 : \\mu \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\bar{x} - \\mu_0}{\\sigma / \\sqrt{n}} = \\) ",
          "(", round(test$mean, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$sigma / sqrt(n_val), 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", "we do not reject the null hypothesis that the true mean is "), input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      "loading..."
    }
  })

  output$results_twomeanspaired <- renderUI({
    if (input$input_type_twomeanspaired == "raw") {
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) return("Invalid input or not enough observations")
      if (length(dat1) != length(dat2)) return("Number of observations must be equal in the two samples")
      n_val <- length(dat1)
      dbar_val <- mean(dat2 - dat1)
      s2d_val <- var(dat2 - dat1)
      sd_val <- sd(dat2 - dat1)
    } else {
      n_val <- input$n_twomeanspaired_sum
      dbar_val <- input$dbar_twomeanspaired_sum
      s2d_val <- input$s2d_twomeanspaired_sum
      sd_val <- sqrt(max(s2d_val, 0))
      if (is.na(n_val) | n_val < 2) return("Invalid input: n must be \u2265 2")
    }

    if (input$inference == "two means (paired samples)" & input$popsd_twomeanspaired == FALSE) {
      if (input$input_type_twomeanspaired == "raw") {
        test_confint <- t.test(x = dat2, y = dat1, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha, paired = TRUE)
        test <- t.test(x = dat2, y = dat1, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = TRUE)
      } else {
        if (is.na(s2d_val) | s2d_val < 0) return("Invalid input: s\u00b2_D must be \u2265 0")
        test_confint <- t_test1_sum(n_val, dbar_val, s2d_val, input$h0, input$alpha, "two.sided")
        test <- t_test1_sum(n_val, dbar_val, s2d_val, input$h0, input$alpha, input$alternative)
      }
      withMathJax(
        tags$b("Data"),
        br(),
        if (input$input_type_twomeanspaired == "raw") paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeanspaired == "raw") br(),
        if (input$input_type_twomeanspaired == "raw") paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeanspaired == "raw") br(),
        if (input$input_type_twomeanspaired == "raw") paste(c("Difference \\((D) = Sample_2 - Sample_1=\\)", paste(dat2 - dat1, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeanspaired == "raw") br(),
        paste0("Number of pairs \\(n =\\) ", n_val),
        br(),
        paste0("\\(\\bar{D} =\\) ", round(dbar_val, 3)),
        br(),
        paste0("\\(s^2_D =\\) ", round(s2d_val, 3)),
        br(),
        paste0("\\(s_D =\\) ", round(sd_val, 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0(
          (1 - input$alpha) * 100,
          ifelse(input$alternative == "two.sided",
            paste0("% CI for \\(\\mu_D = \\bar{D} \\pm t_{\\alpha/2, n - 1} \\dfrac{s_D}{\\sqrt{n}} = \\) ", round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(sd_val, 3), " / ", round(sqrt(n_val), 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0("% CI for \\(\\mu_D = \\bar{D} - t_{\\alpha, n - 1} \\dfrac{s_D}{\\sqrt{n}} = \\) ", round(test$estimate, 3), "  \\( - \\) ", "\\( ( \\)", round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3), " * ", round(sd_val, 3), " / ", round(sqrt(n_val), 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test$conf.int[1], 3), "; \\(+\\infty\\))"),
              paste0("% CI for \\(\\mu_D = \\bar{D} + t_{\\alpha, n - 1} \\dfrac{s_D}{\\sqrt{n}} = \\) ", round(test$estimate, 3), "  \\( + \\) ", "\\( ( \\)", round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3), " * ", round(sd_val, 3), " / ", round(sqrt(n_val), 3), "\\( ) \\) ", "\\( = \\) ", "(-\\(\\infty\\); ", round(test$conf.int[2], 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_D = \\) ", test$null.value, " and \\(H_1 : \\mu_D \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{\\bar{D} - \\mu_0}{s_D / \\sqrt{n}} = \\) ",
          "(", round(test$estimate, 3), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", ifelse(input$alternative == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean of the difference is equal to ", "we do not reject the null hypothesis that the true mean of the difference is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two means (paired samples)" & input$popsd_twomeanspaired == TRUE) {
      if (input$input_type_twomeanspaired == "raw") {
        test <- t.test2(x = dat2 - dat1, V = input$sigma2_twomeanspaired, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      } else {
        test <- t.test2(x = rep(dbar_val, n_val), V = input$sigma2_twomeanspaired, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      }
      withMathJax(
        tags$b("Data"),
        br(),
        if (input$input_type_twomeanspaired == "raw") paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeanspaired == "raw") br(),
        if (input$input_type_twomeanspaired == "raw") paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeanspaired == "raw") br(),
        if (input$input_type_twomeanspaired == "raw") paste(c("Difference \\((D) = Sample_2 - Sample_1=\\)", paste(dat2 - dat1, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeanspaired == "raw") br(),
        paste0("Number of pairs \\(n =\\) ", n_val),
        br(),
        paste0("\\(\\bar{D} =\\) ", round(dbar_val, 3)),
        br(),
        paste0("\\(\\sigma^2_D =\\) ", round(input$sigma2_twomeanspaired, 3)),
        br(),
        paste0("\\(\\sigma_D =\\) ", round(sqrt(input$sigma2_twomeanspaired), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        tags$em("(two-sided)"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% Confidence Interval for \\(\\mu_D = \\bar{D} \\pm z_{\\alpha/2} \\dfrac{\\sigma_D}{\\sqrt{n}} = \\) ",
          round(test$mean, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$sigma, 3), " / ", round(sqrt(n_val), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_D = \\) ", input$h0, " and \\(H_1 : \\mu_D \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\bar{D} - \\mu_0}{\\sigma_D / \\sqrt{n}} = \\) ",
          "(", round(test$mean, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$sigma / sqrt(n_val), 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean of the difference is equal to ", "we do not reject the null hypothesis that the true mean of the difference is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      "loading..."
    }
  })

  output$results_twomeans <- renderUI({
    if (input$input_type_twomeans == "raw") {
      dat1 <- extract(input$sample1_twomeans)
      dat2 <- extract(input$sample2_twomeans)
      if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) return("Invalid input or not enough observations")
      n1_val <- length(dat1); n2_val <- length(dat2)
      xbar1_val <- mean(dat1); xbar2_val <- mean(dat2)
      s21_val <- var(dat1); s22_val <- var(dat2)
    } else {
      n1_val <- input$n1_twomeans_sum; n2_val <- input$n2_twomeans_sum
      xbar1_val <- input$xbar1_twomeans_sum; xbar2_val <- input$xbar2_twomeans_sum
      s21_val <- input$s21_twomeans_sum; s22_val <- input$s22_twomeans_sum
      if (is.na(n1_val) | n1_val < 2 | is.na(n2_val) | n2_val < 2) return("Invalid input: n must be \u2265 2")
      if (is.na(s21_val) | s21_val < 0 | is.na(s22_val) | s22_val < 0) return("Invalid input: s\u00b2 must be \u2265 0")
    }

    if (input$inference == "two means (independent samples)" & input$popsd_twomeans == FALSE & input$var.equal == TRUE) {
      if (input$input_type_twomeans == "raw") {
        test_confint <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
        test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
      } else {
        test_confint <- t_test2_sum(n1_val, n2_val, xbar1_val, xbar2_val, s21_val, s22_val, TRUE, input$h0, input$alpha, "two.sided")
        test <- t_test2_sum(n1_val, n2_val, xbar1_val, xbar2_val, s21_val, s22_val, TRUE, input$h0, input$alpha, input$alternative)
      }
      s_p <- sqrt(((n1_val - 1) * s21_val + (n2_val - 1) * s22_val) / (n1_val + n2_val - 2))
      withMathJax(
        tags$b("Data"),
        br(),
        if (input$input_type_twomeans == "raw") paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeans == "raw") br(),
        if (input$input_type_twomeans == "raw") paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeans == "raw") br(),
        paste0("\\(n_1 =\\) ", n1_val),
        br(),
        paste0("\\(n_2 =\\) ", n2_val),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(xbar1_val, 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(xbar2_val, 3)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(s21_val, 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(s22_val, 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0((1 - input$alpha) * 100, ifelse(input$alternative == "two.sided", "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, n_1 + n_2 - 2} (s_p) \\sqrt{\\frac{1}{n_1} + \\frac{1}{n_2}} \\)", ifelse(input$alternative == "greater", "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 - t_{\\alpha, n_1 + n_2 - 2} (s_p) \\sqrt{\\frac{1}{n_1} + \\frac{1}{n_2}} \\)", "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 + t_{\\alpha, n_1 + n_2 - 2} (s_p) \\sqrt{\\frac{1}{n_1} + \\frac{1}{n_2}} \\)"))),
        br(),
        paste0("where ", "\\( s_p = \\sqrt{\\frac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2}{n_1 + n_2 - 2}} = \\) ", round(s_p, 3)),
        br(),
        br(),
        paste0(
          "\\( \\Rightarrow \\)", (1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\) ",
          ifelse(input$alternative == "two.sided",
            paste0(round(test_confint$estimate[1], 3), ifelse(test_confint$estimate[2] >= 0, paste0(" - ", round(test_confint$estimate[2], 3)), paste0(" + ", round(abs(test_confint$estimate[2]), 3))), " \\( \\pm \\) ", "\\( (\\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(s_p, 3), " * ", round(sqrt(1 / n1_val + 1 / n2_val), 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0(round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), " \\( - \\) ", "\\( (\\)", round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3), " * ", round(s_p, 3), " * ", round(sqrt(1 / n1_val + 1 / n2_val), 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test$conf.int[1], 3), "; \\(+\\infty\\))"),
              paste0(round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), " \\( + \\) ", "\\( (\\)", round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3), " * ", round(s_p, 3), " * ", round(sqrt(1 / n1_val + 1 / n2_val), 3), "\\( ) \\) ", "\\( = \\) ", "(-\\(\\infty\\); ", round(test$conf.int[2], 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{s_p \\sqrt{\\frac{1}{n_1} + \\frac{1}{n_2}}} = \\) ",
          "(", round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / (", round(s_p, 3), " * ", round(sqrt(1 / n1_val + 1 / n2_val), 3), ") \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm t_{\\alpha/2, n_1 + n_2 - 2} = \\pm t(\\)", ifelse(input$alternative == "greater", " \\( t_{\\alpha, n_1 + n_2 - 2} = t(\\)", " \\( -t_{\\alpha, n_1 + n_2 - 2} = -t(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in means is ", "we do not reject the null hypothesis that the true difference in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two means (independent samples)" & input$popsd_twomeans == FALSE & input$var.equal == FALSE) {
      if (input$input_type_twomeans == "raw") {
        test_confint <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
        test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      } else {
        test_confint <- t_test2_sum(n1_val, n2_val, xbar1_val, xbar2_val, s21_val, s22_val, FALSE, input$h0, input$alpha, "two.sided")
        test <- t_test2_sum(n1_val, n2_val, xbar1_val, xbar2_val, s21_val, s22_val, FALSE, input$h0, input$alpha, input$alternative)
      }
      withMathJax(
        tags$b("Data"),
        br(),
        if (input$input_type_twomeans == "raw") paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeans == "raw") br(),
        if (input$input_type_twomeans == "raw") paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeans == "raw") br(),
        paste0("\\(n_1 =\\) ", n1_val),
        br(),
        paste0("\\(n_2 =\\) ", n2_val),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(xbar1_val, 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(xbar2_val, 3)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(s21_val, 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(s22_val, 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0((1 - input$alpha) * 100, ifelse(input$alternative == "two.sided", "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, \\nu} \\sqrt{\\frac{s^2_1}{n_1} + \\frac{s^2_2}{n_2}} \\)", ifelse(input$alternative == "greater", "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 - t_{\\alpha, \\nu} \\sqrt{\\frac{s^2_1}{n_1} + \\frac{s^2_2}{n_2}} \\)", "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 + t_{\\alpha, \\nu} \\sqrt{\\frac{s^2_1}{n_1} + \\frac{s^2_2}{n_2}} \\)"))),
        br(),
        paste0("where ", "\\( \\nu = \\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}\\Bigg)^2}{\\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1}\\Bigg)^2}{n_1-1} + \\dfrac{\\Bigg(\\dfrac{s^2_2}{n_2}\\Bigg)^2}{n_2-1}} = \\) ", round(test$parameter, 3)),
        br(),
        br(),
        paste0(
          "\\( \\Rightarrow \\)", (1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\) ",
          ifelse(input$alternative == "two.sided",
            paste0(round(test_confint$estimate[1], 3), ifelse(test_confint$estimate[2] >= 0, paste0(" - ", round(test_confint$estimate[2], 3)), paste0(" + ", round(abs(test_confint$estimate[2]), 3))), " \\( \\pm \\) ", "\\( (\\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0(round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), " \\( - \\) ", "\\( (\\)", round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3), " * ", round(test$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test$conf.int[1], 3), "; \\(+\\infty\\))"),
              paste0(round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), " \\( + \\) ", "\\( (\\)", round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3), " * ", round(test$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "(-\\(\\infty\\); ", round(test$conf.int[2], 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$em(paste0("Note: for simplicity, the number of degrees of freedom is sometimes approximated as df = \\(min(n_1 - 1, n_2 - 1) \\), so in this case df = ", min(n1_val - 1, n2_val - 1), ".")),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\frac{s^2_1}{n_1} + \\frac{s^2_2}{n_2}}} = \\) ",
          "(", round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm t_{\\alpha/2, \\nu} = \\pm t(\\)", ifelse(input$alternative == "greater", " \\( t_{\\alpha, \\nu} = t(\\)", " \\( -t_{\\alpha, \\nu} = -t(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), ", ", round(test$parameter, 3), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in means is ", "we do not reject the null hypothesis that the true difference in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two means (independent samples)" & input$popsd_twomeans == TRUE) {
      if (input$input_type_twomeans == "raw") {
        test <- t.test3(x = dat1, y = dat2, V1 = input$sigma21_twomeans, V2 = input$sigma22_twomeans, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      } else {
        test <- t.test3(x = rep(xbar1_val, n1_val), y = rep(xbar2_val, n2_val), V1 = input$sigma21_twomeans, V2 = input$sigma22_twomeans, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      }
      withMathJax(
        tags$b("Data"),
        br(),
        if (input$input_type_twomeans == "raw") paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeans == "raw") br(),
        if (input$input_type_twomeans == "raw") paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        if (input$input_type_twomeans == "raw") br(),
        paste0("\\(n_1 =\\) ", n1_val),
        br(),
        paste0("\\(n_2 =\\) ", n2_val),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(xbar1_val, 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(xbar2_val, 3)),
        br(),
        paste0("\\(\\sigma^2_1 =\\) ", round(input$sigma21_twomeans, 3)),
        br(),
        paste0("\\(\\sigma^2_2 =\\) ", round(input$sigma22_twomeans, 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        tags$em("(two-sided)"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm z_{\\alpha/2} \\sqrt{\\frac{\\sigma^2_1}{n_1} + \\frac{\\sigma^2_2}{n_2}} = \\) ",
          round(test$mean1, 3), ifelse(test$mean2 >= 0, paste0(" - ", round(test$mean2, 3)), paste0(" + ", round(abs(test$mean2), 3))), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$S, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", input$h0, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\frac{\\sigma^2_1}{n_1} + \\frac{\\sigma^2_2}{n_2}}} = \\) ",
          "(", round(test$mean1, 3), ifelse(test$mean2 >= 0, paste0(" - ", round(test$mean2, 3)), paste0(" + ", round(abs(test$mean2), 3))), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$S, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in means is ", "we do not reject the null hypothesis that the true difference in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      "loading..."
    }
  })

  output$results_oneprop <- renderUI({
    if (input$inference == "one proportion" & input$propx_oneprop == "prop_true") {
      test <- prop.z.test(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test2 <- prop.z.test3(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test_confint <- prop.z.test(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided")
      withMathJax(
        tags$b("Data"),
        br(),
        paste0("\\(n =\\) ", round(test$n, 3)),
        br(),
        paste0("\\(\\widehat{p} =\\) ", round(test$estimate, 3)),
        br(),
        paste0("\\(\\widehat{q} = 1 - \\widehat{p} =\\) ", round(1 - test$estimate, 3)),
        br(),
        helpText(paste0("\\( n\\widehat{p} = \\) ", round(test$n * test$estimate, 3), " and \\( n(1-\\widehat{p}) = \\) ", round(test$n * (1 - test$estimate), 3))),
        helpText(paste0("Assumptions \\( n\\widehat{p} \\geq 5\\) and \\( n(1-\\widehat{p}) \\geq 5\\)", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0(
          (1 - input$alpha) * 100,
          ifelse(input$alternative == "two.sided",
            paste0("% CI for \\(p = \\widehat{p} \\pm z_{\\alpha/2} \\sqrt{\\frac{\\widehat{p}(1-\\widehat{p})}{n}} = \\) ", round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0("% CI for \\(p = \\widehat{p} - z_{\\alpha} \\sqrt{\\frac{\\widehat{p}(1-\\widehat{p})}{n}} = \\) ", round(test_confint$estimate, 3), "  \\( - \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$estimate - qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "; \\(+\\infty\\))"),
              paste0("% CI for \\(p = \\widehat{p} + z_{\\alpha} \\sqrt{\\frac{\\widehat{p}(1-\\widehat{p})}{n}} = \\) ", round(test_confint$estimate, 3), "  \\( + \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "(-\\(\\infty\\); ", round(test_confint$estimate + qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p = \\) ", test$null.value, " and \\(H_1 : p \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\widehat{p} - p_0}{\\sqrt{\\frac{p_0(1-p_0)}{n}}} = \\) ",
          "(", round(test2$estimate, 3), ifelse(test2$null.value >= 0, paste0(" - ", test2$null.value), paste0(" + ", abs(test2$null.value))), ") / ", round(test2$stderr, 3), " \\( = \\) ",
          ifelse(test2$null.value >= 0 & test2$null.value <= 1, round(test2$statistic, 3), "Error: \\( p_0 \\) must be \\( 0 \\leq p_0 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test2$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test2$p.value < input$alpha, "we reject the null hypothesis that the true proportion is ", "we do not reject the null hypothesis that the true proportion is "), test2$null.value, " \\((p\\)-value ", ifelse(test2$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test2$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "one proportion" & input$propx_oneprop == "prop_false") {
      if (input$x_oneprop > input$n_oneprop) return("Invalid input: number of successes cannot exceed sample size")
      test <- prop.z.test(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test2 <- prop.z.test3(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test_confint <- prop.z.test(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided")
      withMathJax(
        tags$b("Data"),
        br(),
        paste0("\\(n =\\) ", round(test$n, 3)),
        br(),
        paste0("\\(\\widehat{p} = \\dfrac{x}{n} = \\) ", test$x, " \\( / \\) ", test$n, " \\( = \\) ", round(test$estimate, 3)),
        br(),
        paste0("\\(\\widehat{q} = 1 - \\widehat{p} = \\) ", round(1 - test$estimate, 3)),
        br(),
        helpText(paste0("\\( n\\widehat{p} = \\) ", round(test$n * test$estimate, 3), " and \\( n(1-\\widehat{p}) = \\) ", round(test$n * (1 - test$estimate), 3))),
        helpText(paste0("Assumptions \\( n\\widehat{p} \\geq 5\\) and \\( n(1-\\widehat{p}) \\geq 5\\)", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0(
          (1 - input$alpha) * 100,
          ifelse(input$alternative == "two.sided",
            paste0("% CI for \\(p = \\widehat{p} \\pm z_{\\alpha/2} \\sqrt{\\frac{\\widehat{p}(1-\\widehat{p})}{n}} = \\) ", round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0("% CI for \\(p = \\widehat{p} - z_{\\alpha} \\sqrt{\\frac{\\widehat{p}(1-\\widehat{p})}{n}} = \\) ", round(test_confint$estimate, 3), "  \\( - \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$estimate - qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "; \\(+\\infty\\))"),
              paste0("% CI for \\(p = \\widehat{p} + z_{\\alpha} \\sqrt{\\frac{\\widehat{p}(1-\\widehat{p})}{n}} = \\) ", round(test_confint$estimate, 3), "  \\( + \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "(-\\(\\infty\\); ", round(test_confint$estimate + qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p = \\) ", test$null.value, " and \\(H_1 : p \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\widehat{p} - p_0}{\\sqrt{\\frac{p_0(1-p_0)}{n}}} = \\) ",
          "(", round(test2$estimate, 3), ifelse(test2$null.value >= 0, paste0(" - ", test2$null.value), paste0(" + ", abs(test2$null.value))), ") / ", round(test2$stderr, 3), " \\( = \\) ",
          ifelse(test2$null.value >= 0 & test2$null.value <= 1, round(test2$statistic, 3), "Error: \\( p_0 \\) must be \\( 0 \\leq p_0 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test2$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test2$p.value < input$alpha, "we reject the null hypothesis that the true proportion is ", "we do not reject the null hypothesis that the true proportion is "), test2$null.value, " \\((p\\)-value ", ifelse(test2$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test2$p.value, 3))), ")", ".")
      )
    } else {
      "loading..."
    }
  })

  output$results_twoprop <- renderUI({
    if (input$inference == "two proportions" & input$propx_twoprop == "prop_true" & input$pooledstderr_twoprop == FALSE) {
      test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      test_confint <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        tags$b("Data"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\widehat{p}_1 =\\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\widehat{p}_2 =\\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\widehat{q}_1 = 1 - \\widehat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\widehat{q}_2 = 1 - \\widehat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\widehat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\widehat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\widehat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\widehat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\widehat{p}_1 \\geq 5\\), \\( n_1(1-\\widehat{p}_1) \\geq 5\\), \\( n_2\\widehat{p}_2 \\geq 5\\) and \\( n_2(1-\\widehat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0(
          (1 - input$alpha) * 100,
          ifelse(input$alternative == "two.sided",
            paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 - z_{\\alpha} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( - \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round((test_confint$estimate1 - test_confint$estimate2) - qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "; \\(+\\infty\\))"),
              paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 + z_{\\alpha} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( + \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "(-\\(\\infty\\); ", round((test_confint$estimate1 - test_confint$estimate2) + qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{(\\widehat{p}_1 - \\widehat{p}_2) - (p_1 - p_2)}{\\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in proportions is ", "we do not reject the null hypothesis that the true difference in proportions is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two proportions" & input$propx_twoprop == "prop_false" & input$pooledstderr_twoprop == FALSE) {
      if (input$x1_twoprop > input$n1_twoprop | input$x2_twoprop > input$n2_twoprop) return("Invalid input: number of successes cannot exceed sample size")
      test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      test_confint <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        tags$b("Data"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\widehat{p}_1 = \\dfrac{x_1}{n_1} = \\) ", test$x1, " \\( / \\) ", test$n1, " \\( = \\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\widehat{p}_2 = \\dfrac{x_2}{n_2} = \\) ", test$x2, " \\( / \\) ", test$n2, " \\( = \\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\widehat{q}_1 = 1 - \\widehat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\widehat{q}_2 = 1 - \\widehat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\widehat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\widehat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\widehat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\widehat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\widehat{p}_1 \\geq 5\\), \\( n_1(1-\\widehat{p}_1) \\geq 5\\), \\( n_2\\widehat{p}_2 \\geq 5\\) and \\( n_2(1-\\widehat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0(
          (1 - input$alpha) * 100,
          ifelse(input$alternative == "two.sided",
            paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 - z_{\\alpha} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( - \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round((test_confint$estimate1 - test_confint$estimate2) - qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "; \\(+\\infty\\))"),
              paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 + z_{\\alpha} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( + \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "(-\\(\\infty\\); ", round((test_confint$estimate1 - test_confint$estimate2) + qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{(\\widehat{p}_1 - \\widehat{p}_2) - (p_1 - p_2)}{\\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in proportions is ", "we do not reject the null hypothesis that the true difference in proportions is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two proportions" & input$propx_twoprop == "prop_true" & input$pooledstderr_twoprop == TRUE) {
      test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = TRUE)
      test_confint <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        tags$b("Data"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\widehat{p}_1 =\\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\widehat{p}_2 =\\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\widehat{q}_1 = 1 - \\widehat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\widehat{q}_2 = 1 - \\widehat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\widehat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\widehat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\widehat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\widehat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\widehat{p}_1 \\geq 5\\), \\( n_1(1-\\widehat{p}_1) \\geq 5\\), \\( n_2\\widehat{p}_2 \\geq 5\\) and \\( n_2(1-\\widehat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0(
          (1 - input$alpha) * 100,
          ifelse(input$alternative == "two.sided",
            paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 - z_{\\alpha} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( - \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round((test_confint$estimate1 - test_confint$estimate2) - qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "; \\(+\\infty\\))"),
              paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 + z_{\\alpha} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( + \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "(-\\(\\infty\\); ", round((test_confint$estimate1 - test_confint$estimate2) + qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0("2. Test statistic : \\(z_{obs} = \\dfrac{(\\widehat{p}_1 - \\widehat{p}_2) - (p_1 - p_2)}{\\sqrt{\\widehat{p}(1-\\widehat{p})\\Big(\\frac{1}{n_1} + \\frac{1}{n_2}\\Big)}} \\) "),
        br(),
        paste0("where ", "\\( \\widehat{p} = \\dfrac{n_1 \\widehat{p}_1 + n_2 \\widehat{p}_2}{n_1 + n_2} = \\) ", "(", test$n1, " * ", round(test$estimate1, 3), " + ", test$n2, " * ", round(test$estimate2, 3), ") / (", test$n1, " + ", test$n2, ") = ", round(test$pooled.phat, 3)),
        br(),
        paste0(
          "\\( \\Rightarrow z_{obs} = \\dfrac{(\\widehat{p}_1 - \\widehat{p}_2) - (p_1 - p_2)}{\\sqrt{\\widehat{p}(1-\\widehat{p})\\Big(\\frac{1}{n_1} + \\frac{1}{n_2}\\Big)}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in proportions is ", "we do not reject the null hypothesis that the true difference in proportions is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two proportions" & input$propx_twoprop == "prop_false" & input$pooledstderr_twoprop == TRUE) {
      if (input$x1_twoprop > input$n1_twoprop | input$x2_twoprop > input$n2_twoprop) return("Invalid input: number of successes cannot exceed sample size")
      test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = TRUE)
      test_confint <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        tags$b("Data"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\widehat{p}_1 = \\dfrac{x_1}{n_1} = \\) ", test$x1, " \\( / \\) ", test$n1, " \\( = \\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\widehat{p}_2 = \\dfrac{x_2}{n_2} = \\) ", test$x2, " \\( / \\) ", test$n2, " \\( = \\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\widehat{q}_1 = 1 - \\widehat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\widehat{q}_2 = 1 - \\widehat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\widehat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\widehat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\widehat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\widehat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\widehat{p}_1 \\geq 5\\), \\( n_1(1-\\widehat{p}_1) \\geq 5\\), \\( n_2\\widehat{p}_2 \\geq 5\\) and \\( n_2(1-\\widehat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0(
          (1 - input$alpha) * 100,
          ifelse(input$alternative == "two.sided",
            paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 - z_{\\alpha} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( - \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "[", round((test_confint$estimate1 - test_confint$estimate2) - qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "; \\(+\\infty\\))"),
              paste0("% CI for \\(p_1 - p_2 = \\widehat{p}_1 - \\widehat{p}_2 + z_{\\alpha} \\sqrt{\\frac{\\widehat{p}_1(1-\\widehat{p}_1)}{n_1} + \\frac{\\widehat{p}_2(1-\\widehat{p}_2)}{n_2}} = \\) ", round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( + \\) ", "\\( ( \\)", round(qnorm(input$alpha, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ", "(-\\(\\infty\\); ", round((test_confint$estimate1 - test_confint$estimate2) + qnorm(input$alpha, lower.tail = FALSE) * test_confint$stderr, 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0("2. Test statistic : \\(z_{obs} = \\dfrac{(\\widehat{p}_1 - \\widehat{p}_2) - (p_1 - p_2)}{\\sqrt{\\widehat{p}(1-\\widehat{p})\\Big(\\frac{1}{n_1} + \\frac{1}{n_2}\\Big)}} \\) "),
        br(),
        paste0("where ", "\\( \\widehat{p} = \\dfrac{n_1 \\widehat{p}_1 + n_2 \\widehat{p}_2}{n_1 + n_2} = \\) ", "(", test$n1, " * ", round(test$estimate1, 3), " + ", test$n2, " * ", round(test$estimate2, 3), ") / (", test$n1, " + ", test$n2, ") = ", round(test$pooled.phat, 3)),
        br(),
        paste0(
          "\\( \\Rightarrow z_{obs} = \\dfrac{(\\widehat{p}_1 - \\widehat{p}_2) - (p_1 - p_2)}{\\sqrt{\\widehat{p}(1-\\widehat{p})\\Big(\\frac{1}{n_1} + \\frac{1}{n_2}\\Big)}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in proportions is ", "we do not reject the null hypothesis that the true difference in proportions is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      "loading..."
    }
  })

  output$results_onevar <- renderUI({
    if (input$input_type_onevar == "raw") {
      dat <- extract(input$sample_onevar)
      if (anyNA(dat) | length(dat) < 2) return("Invalid input or not enough observations")
      n_val <- length(dat)
      s2_val <- var(dat)
      s_val <- sd(dat)
    } else {
      n_val <- input$n_onevar_sum
      s2_val <- input$s2_onevar_sum
      s_val <- sqrt(max(s2_val, 0))
      if (is.na(n_val) | n_val < 2) return("Invalid input: n must be \u2265 2")
      if (is.na(s2_val) | s2_val < 0) return("Invalid input: s\u00b2 must be \u2265 0")
    }
    if (input$h0 <= 0) {
      return(withMathJax(sprintf("\\( \\sigma^2_0 \\) must be > 0")))
    }
    if (input$inference == "one variance") {
      if (input$input_type_onevar == "raw") {
        test_confint <- varTest(x = dat, sigma.squared = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha)
        test <- varTest(x = dat, sigma.squared = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      } else {
        test_confint <- vartest_sum(n_val, s2_val, input$h0, input$alpha, "two.sided")
        test <- vartest_sum(n_val, s2_val, input$h0, input$alpha, input$alternative)
      }
      withMathJax(
        tags$b("Data"),
        br(),
        if (input$input_type_onevar == "raw") paste(c(paste(dat, collapse = ", ")), collapse = " "),
        if (input$input_type_onevar == "raw") br(),
        paste0("\\(n =\\) ", n_val),
        br(),
        paste0("\\(s^2 =\\) ", round(s2_val, 3)),
        br(),
        paste0("\\(s =\\) ", round(s_val, 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0(
          (1 - input$alpha) * 100,
          ifelse(input$alternative == "two.sided",
            paste0("% CI for \\(\\sigma^2 = \\Bigg[ \\dfrac{(n-1)s^2}{\\chi^2_{\\alpha/2, n-1}} ; \\dfrac{(n-1)s^2}{\\chi^2_{1-\\alpha/2, n-1}} \\Bigg] = \\) ", "[(", round((n_val - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = FALSE), 3), ") ; (", round((n_val - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = TRUE), 3), ")] = ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative == "greater",
              paste0("% CI for \\(\\sigma^2 = \\Bigg[ \\dfrac{(n-1)s^2}{\\chi^2_{\\alpha, n-1}} ; +\\infty \\Bigg) = \\) ", "[(", round((n_val - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha, df = test$parameters, lower.tail = FALSE), 3), ") ; +\\(\\infty\\)) = ", "[", round(test$conf.int[1], 3), "; \\(+\\infty\\))"),
              paste0("% CI for \\(\\sigma^2 = \\Bigg( 0 ; \\dfrac{(n-1)s^2}{\\chi^2_{1-\\alpha, n-1}} \\Bigg] = \\) ", "(0 ; (", round((n_val - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha, df = test$parameters, lower.tail = TRUE), 3), ")] = ", "(0; ", round(test$conf.int[2], 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\sigma^2 = \\) ", test$null.value, " and \\(H_1 : \\sigma^2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(\\chi^2_{obs} = \\dfrac{(n-1)s^2}{\\sigma^2_0} = \\) ",
          "[(", n_val, " - 1) * ", round(test$estimate, 3), "] / ", test$null.value, " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        if (input$alternative == "two.sided") {
          withMathJax(
            paste0("3. Critical values : \\( \\chi^2_{1-\\alpha/2, n - 1} \\) and \\( \\chi^2_{\\alpha/2, n - 1} =\\) "),
            paste0("\\( \\chi^2 \\)(", 1 - input$alpha / 2, ", ", test$parameters, ") and \\( \\chi^2 \\)(", input$alpha / 2, ", ", test$parameters, ") = "),
            paste0(round(qchisq(1 - input$alpha / 2, df = test$parameters, lower.tail = FALSE), 3), " and ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = FALSE), 3))
          )
        } else if (input$alternative == "greater") {
          withMathJax(
            paste0("3. Critical value : \\( \\chi^2_{\\alpha, n - 1} =\\) "),
            paste0("\\( \\chi^2 \\)(", input$alpha, ", ", test$parameters, ") = "),
            paste0(round(qchisq(input$alpha, df = test$parameters, lower.tail = FALSE), 3))
          )
        } else {
          withMathJax(
            paste0("3. Critical value : \\( \\chi^2_{1-\\alpha, n - 1} =\\) "),
            paste0("\\( \\chi^2 \\)(", 1 - input$alpha, ", ", test$parameters, ") = "),
            paste0(round(qchisq(1 - input$alpha, df = test$parameters, lower.tail = FALSE), 3))
          )
        },
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true variance is equal to ", "we do not reject the null hypothesis that the true variance is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      "loading..."
    }
  })

  output$results_twovar <- renderUI({
    if (input$input_type_twovar == "raw") {
      dat1 <- extract(input$sample1_twovar)
      dat2 <- extract(input$sample2_twovar)
      if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) return("Invalid input or not enough observations")
      n1_val <- length(dat1); n2_val <- length(dat2)
      s21_val <- var(dat1); s22_val <- var(dat2)
    } else {
      n1_val <- input$n1_twovar_sum; n2_val <- input$n2_twovar_sum
      s21_val <- input$s21_twovar_sum; s22_val <- input$s22_twovar_sum
      if (is.na(n1_val) | n1_val < 2 | is.na(n2_val) | n2_val < 2) return("Invalid input: n must be \u2265 2")
      if (is.na(s21_val) | s21_val < 0 | is.na(s22_val) | s22_val < 0) return("Invalid input: s\u00b2 must be \u2265 0")
    }
    if (input$inference == "two variances") {
      if (input$input_type_twovar == "raw") {
        test_confint <- var.test(x = dat1, y = dat2, ratio = 1, alternative = "two.sided", conf.level = 1 - input$alpha)
        test <- var.test(x = dat1, y = dat2, ratio = 1, alternative = input$alternative_twovar, conf.level = 1 - input$alpha)
      } else {
        test_confint <- ftest_sum(n1_val, n2_val, s21_val, s22_val, input$alpha, "two.sided")
        test <- ftest_sum(n1_val, n2_val, s21_val, s22_val, input$alpha, input$alternative_twovar)
      }
      withMathJax(
        tags$b("Data"),
        br(),
        if (input$input_type_twovar == "raw") paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        if (input$input_type_twovar == "raw") br(),
        if (input$input_type_twovar == "raw") paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        if (input$input_type_twovar == "raw") br(),
        paste0("\\(n_1 =\\) ", n1_val),
        br(),
        paste0("\\(n_2 =\\) ", n2_val),
        br(),
        paste0("\\(s^2_1 =\\) ", round(s21_val, 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(s22_val, 3)),
        br(),
        paste0("\\(s_1 =\\) ", round(sqrt(s21_val), 3)),
        br(),
        paste0("\\(s_2 =\\) ", round(sqrt(s22_val), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        tags$em(ifelse(input$alternative_twovar == "two.sided", "(two-sided)", "(one-sided)")),
        br(),
        paste0(
          (1 - input$alpha) * 100,
          ifelse(input$alternative_twovar == "two.sided",
            paste0("% CI for \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = \\Bigg[ \\dfrac{s^2_1}{s^2_2}\\dfrac{1}{F_{\\alpha/2, n_1 - 1, n_2-1}} ; \\dfrac{s^2_1}{s^2_2}F_{\\alpha/2, n_2 - 1, n_1-1} \\Bigg] = \\) ", "\\( \\big[ \\)", round(test_confint$estimate, 3), " * (1 / ", round(qf(input$alpha / 2, df1 = test_confint$parameter[1], df2 = test_confint$parameter[2], lower.tail = FALSE), 3), "); ", round(test_confint$estimate, 3), " * ", round(qf(input$alpha / 2, df1 = test_confint$parameter[2], df2 = test_confint$parameter[1], lower.tail = FALSE), 3), "\\( \\big] = \\) ", "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
            ifelse(input$alternative_twovar == "greater",
              paste0("% CI for \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = \\Bigg[ \\dfrac{s^2_1}{s^2_2}\\dfrac{1}{F_{\\alpha, n_1 - 1, n_2-1}} ; +\\infty \\Bigg) = \\) ", "\\( \\big[ \\)", round(test_confint$estimate, 3), " * (1 / ", round(qf(input$alpha, df1 = test_confint$parameter[1], df2 = test_confint$parameter[2], lower.tail = FALSE), 3), ") ; \\(+\\infty\\)) = ", "[", round(test$conf.int[1], 3), "; \\(+\\infty\\))"),
              paste0("% CI for \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = \\Bigg( 0 ; \\dfrac{s^2_1}{s^2_2}F_{\\alpha, n_2 - 1, n_1-1} \\Bigg] = \\) ", "(0 ; ", round(test_confint$estimate, 3), " * ", round(qf(input$alpha, df1 = test_confint$parameter[2], df2 = test_confint$parameter[1], lower.tail = FALSE), 3), "] = ", "(0; ", round(test$conf.int[2], 3), "]")
            )
          )
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        if (test$alternative == "two.sided") {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) and \\(H_1 : \\sigma^2_1 \\neq \\sigma^2_2 \\) ")
          )
        } else if (test$alternative == "greater") {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) and \\(H_1 : \\sigma^2_1 > \\sigma^2_2 \\) ")
          )
        } else {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) and \\(H_1 : \\sigma^2_1 < \\sigma^2_2 \\) ")
          )
        },
        br(),
        paste0(
          "2. Test statistic : \\(F_{obs} = \\dfrac{s^2_1}{s^2_2} = \\) ",
          "[", round(s21_val, 3), " / ", round(s22_val, 3), "]", " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        if (test$alternative == "two.sided") {
          withMathJax(
            paste0("3. Critical values : \\( F_{1-\\alpha/2, n_1 - 1, n_2-1} \\) and \\( F_{\\alpha/2, n_1 - 1, n_2-1} =\\) "),
            paste0("\\( \\dfrac{1}{F_{\\alpha/2, n_1 - 1, n_2-1}} \\) and \\( F_{\\alpha/2, n_1 - 1, n_2-1} =\\) "),
            paste0("\\( 1/F \\)(", input$alpha / 2, ", ", test$parameter[1], ", ", test$parameter[2], ") and \\( F \\)(", input$alpha / 2, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
            paste0(round(qf(input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE), 3), " and ", round(qf(input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE), 3))
          )
        } else if (test$alternative == "greater") {
          withMathJax(
            paste0("3. Critical value : \\( F_{\\alpha, n_1 - 1, n_2-1} =\\) "),
            paste0("\\( F \\)(", input$alpha, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
            paste0(round(qf(input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE), 3))
          )
        } else {
          withMathJax(
            paste0("3. Critical values : \\( F_{1-\\alpha, n_1 - 1, n_2-1} = \\) "),
            paste0("\\( \\dfrac{1}{F_{\\alpha, n_1 - 1, n_2-1}} = \\) "),
            paste0("\\( 1/F \\)(", input$alpha, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
            paste0(round(qf(input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE), 3))
          )
        },
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true ratio of variances is equal to ", "we do not reject the null hypothesis that the true ratio of variances is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      "loading..."
    }
  })

  output$plot <- renderPlot({
    if (input$inference == "one mean" & input$popsd_onemean == FALSE) {
      if (input$input_type_onemean == "raw") {
        dat <- extract(input$sample_onemean)
        test <- t.test(x = dat, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      } else {
        test <- t_test1_sum(input$n_onemean_sum, input$xbar_onemean_sum, input$s2_onemean_sum, input$h0, input$alpha, input$alternative)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "one mean" & input$popsd_onemean == TRUE) {
      if (input$input_type_onemean == "raw") {
        dat <- extract(input$sample_onemean)
        test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      } else {
        test <- t.test2(x = rep(input$xbar_onemean_sum, input$n_onemean_sum), V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two means (independent samples)" & input$popsd_twomeans == FALSE & input$var.equal == TRUE) {
      if (input$input_type_twomeans == "raw") {
        dat1 <- extract(input$sample1_twomeans)
        dat2 <- extract(input$sample2_twomeans)
        test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
      } else {
        test <- t_test2_sum(input$n1_twomeans_sum, input$n2_twomeans_sum, input$xbar1_twomeans_sum, input$xbar2_twomeans_sum, input$s21_twomeans_sum, input$s22_twomeans_sum, TRUE, input$h0, input$alpha, input$alternative)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two means (independent samples)" & input$popsd_twomeans == FALSE & input$var.equal == FALSE) {
      if (input$input_type_twomeans == "raw") {
        dat1 <- extract(input$sample1_twomeans)
        dat2 <- extract(input$sample2_twomeans)
        test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      } else {
        test <- t_test2_sum(input$n1_twomeans_sum, input$n2_twomeans_sum, input$xbar1_twomeans_sum, input$xbar2_twomeans_sum, input$s21_twomeans_sum, input$s22_twomeans_sum, FALSE, input$h0, input$alpha, input$alternative)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two means (independent samples)" & input$popsd_twomeans == TRUE) {
      if (input$input_type_twomeans == "raw") {
        dat1 <- extract(input$sample1_twomeans)
        dat2 <- extract(input$sample2_twomeans)
        test <- t.test3(x = dat1, y = dat2, V1 = input$sigma21_twomeans, V2 = input$sigma22_twomeans, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      } else {
        test <- t.test3(x = rep(input$xbar1_twomeans_sum, input$n1_twomeans_sum), y = rep(input$xbar2_twomeans_sum, input$n2_twomeans_sum), V1 = input$sigma21_twomeans, V2 = input$sigma22_twomeans, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two means (paired samples)" & input$popsd_twomeanspaired == FALSE) {
      if (input$input_type_twomeanspaired == "raw") {
        dat1 <- extract(input$sample1_twomeanspaired)
        dat2 <- extract(input$sample2_twomeanspaired)
        test <- t.test(x = dat2, y = dat1, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = TRUE)
      } else {
        test <- t_test1_sum(input$n_twomeanspaired_sum, input$dbar_twomeanspaired_sum, input$s2d_twomeanspaired_sum, input$h0, input$alpha, input$alternative)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two means (paired samples)" & input$popsd_twomeanspaired == TRUE) {
      if (input$input_type_twomeanspaired == "raw") {
        dat1 <- extract(input$sample1_twomeanspaired)
        dat2 <- extract(input$sample2_twomeanspaired)
        test <- t.test2(x = dat2 - dat1, V = input$sigma2_twomeanspaired, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      } else {
        test <- t.test2(x = rep(input$dbar_twomeanspaired_sum, input$n_twomeanspaired_sum), V = input$sigma2_twomeanspaired, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "one proportion") {
      if (input$propx_oneprop == "prop_true") {
        test <- prop.z.test3(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      } else {
        if (input$x_oneprop > input$n_oneprop) return(NULL)
        test <- prop.z.test3(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two proportions") {
      if (input$propx_twoprop == "prop_true" & input$pooledstderr_twoprop == FALSE) {
        test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      } else if (input$propx_twoprop == "prop_false" & input$pooledstderr_twoprop == FALSE) {
        if (input$x1_twoprop > input$n1_twoprop | input$x2_twoprop > input$n2_twoprop) return(NULL)
        test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      } else if (input$propx_twoprop == "prop_true" & input$pooledstderr_twoprop == TRUE) {
        test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = TRUE)
      } else if (input$propx_twoprop == "prop_false" & input$pooledstderr_twoprop == TRUE) {
        if (input$x1_twoprop > input$n1_twoprop | input$x2_twoprop > input$n2_twoprop) return(NULL)
        test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = TRUE)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "one variance") {
      if (input$input_type_onevar == "raw") {
        dat <- extract(input$sample_onevar)
        test <- varTest(x = dat, sigma.squared = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      } else {
        test <- vartest_sum(input$n_onevar_sum, input$s2_onevar_sum, input$h0, input$alpha, input$alternative)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = test$parameters)
          y[x > qchisq(1 - input$alpha / 2, df = test$parameters, lower.tail = FALSE) & x < qchisq(1 - input$alpha / 2, df = test$parameters, lower.tail = TRUE)] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = test$parameters)
          y[x < qchisq(input$alpha, df = test$parameters, lower.tail = FALSE)] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = test$parameters)
          y[x > qchisq(1 - input$alpha, df = test$parameters, lower.tail = FALSE)] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(0, qchisq(0.999, df = test$parameters, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = test$parameters)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.025), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Chi-square distribution (df = ", test$parameters, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two variances") {
      if (input$input_type_twovar == "raw") {
        dat1 <- extract(input$sample1_twovar)
        dat2 <- extract(input$sample2_twovar)
        test <- var.test(x = dat1, y = dat2, ratio = 1, alternative = input$alternative_twovar, conf.level = 1 - input$alpha)
      } else {
        test <- ftest_sum(input$n1_twovar_sum, input$n2_twovar_sum, input$s21_twovar_sum, input$s22_twovar_sum, input$alpha, input$alternative_twovar)
      }
      if (test$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
          y[x > qf(1 - input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE) & x < qf(1 - input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE)] <- NA
          return(y)
        }
      } else if (test$alternative == "greater") {
        funcShaded <- function(x) {
          y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
          y[x < qf(input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE)] <- NA
          return(y)
        }
      } else if (test$alternative == "less") {
        funcShaded <- function(x) {
          y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
          y[x > qf(1 - input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE)] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(0, qf(0.99, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = test$parameter[1], df2 = test$parameter[2])) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("F distribution F(", test$parameter[1], ", ", test$parameter[2], ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else {
      "loading..."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
