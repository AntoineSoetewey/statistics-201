#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(EnvStats)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Statistics 201 - Inference"),
  h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey")),
  withMathJax(),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "inference",
        label = "Inference for:",
        choices = c("one mean", "two means", "two means (paired samples)", "one proportion", "two proportions", "one variance", "two variances"),
        multiple = FALSE,
        selected = "one mean"
      ),
      hr(),
      conditionalPanel(
        condition = "input.inference == 'one mean'",
        textInput("sample_onemean", "Sample", value = "0.9, -0.8, 1.3, -0.3, 1.7", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
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
        condition = "input.inference == 'two means'",
        textInput("sample1_twomeans", "Sample 1", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        textInput("sample2_twomeans", "Sample 2", value = "0.8, -0.9, -0.1, 0.4, 0.1", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
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
        checkboxInput("popsd_twomeans", "Variance of the populations are known", FALSE),
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
        textInput("sample1_twomeanspaired", "Sample 1", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        textInput("sample2_twomeanspaired", "Sample 2", value = "0.8, -0.9, -0.1, 0.4, 0.1", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
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
        tags$b("Sample size"),
        numericInput("n_oneprop", "\\(n = \\)",
          value = 30, min = 0, step = 1
        ),
        hr(),
        radioButtons(
          inputId = "propx_oneprop",
          label = NULL,
          choices = c(
            "Proportion of success \\(\\hat{p}\\)" = "prop_true",
            "Number of successes \\(x\\)" = "prop_false"
          )
        ),
        conditionalPanel(
          condition = "input.propx_oneprop == 'prop_true'",
          tags$b("Proportion of success"),
          numericInput("p_oneprop", "\\(\\hat{p} = \\)",
            value = 0.2, min = 0, max = 1, step = 0.01
          )
        ),
        conditionalPanel(
          condition = "input.propx_oneprop == 'prop_false'",
          tags$b("Number of successes"),
          numericInput("x_oneprop", "\\(x = \\)",
            value = 10, min = 0, step = 1
          )
        )
      ),
      conditionalPanel(
        condition = "input.inference == 'two proportions'",
        tags$b("Sample size 1"),
        numericInput("n1_twoprop", "\\(n_1 = \\)",
          value = 30, min = 0, step = 1
        ),
        tags$b("Sample size 2"),
        numericInput("n2_twoprop", "\\(n_2 = \\)",
          value = 30, min = 0, step = 1
        ),
        hr(),
        radioButtons(
          inputId = "propx_twoprop",
          label = NULL,
          choices = c(
            "Proportion of success \\(\\hat{p}\\)" = "prop_true",
            "Number of successes \\(x\\)" = "prop_false"
          )
        ),
        conditionalPanel(
          condition = "input.propx_twoprop == 'prop_true'",
          tags$b("Proportion of success"),
          numericInput("p1_twoprop", "\\(\\hat{p}_1 = \\)",
            value = 0.2, min = 0, max = 1, step = 0.01
          ),
          numericInput("p2_twoprop", "\\(\\hat{p}_2 = \\)",
            value = 0.3, min = 0, max = 1, step = 0.01
          )
        ),
        conditionalPanel(
          condition = "input.propx_twoprop == 'prop_false'",
          tags$b("Number of successes"),
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
        textInput("sample_onevar", "Sample", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 795, 810, 775, 781, 803, 823, 780, etc.")
      ),
      conditionalPanel(
        condition = "input.inference == 'two variances'",
        textInput("sample1_twovar", "Sample 1", value = "0.9, -0.8, 0.1, -0.3, 0.2, 0.7, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        textInput("sample2_twovar", "Sample 2", value = "0.4, -0.3, -0.1, 0.4, 0.1, 0.2, -0.1, -0.1, 0.4, 0.1", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc.")
      ),
      hr(),
      tags$b("Null hypothesis"),
      conditionalPanel(
        condition = "input.inference == 'one mean'",
        sprintf("\\( H_0 : \\mu = \\)")
      ),
      conditionalPanel(
        condition = "input.inference == 'two means'",
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
      hr(),
      HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/statistics-201/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/statistics-201">code</a>. Back to <a href="https://www.antoinesoetewey.com/">antoinesoetewey.com</a> or <a href="https://statsandr.com/">statsandr.com</a>.</p>'),
      hr(),
      # HTML('<hr style="border:1px solid #ccc;"/>'),
      HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/" target="_blank"><img alt="Licence Creative Commons" style="border-width:0"
        src="http://i.creativecommons.org/l/by/2.0/be/80x15.png"/></a> This work of <span xmlns:cc="http://creativecommons.org/ns#"
        property="cc:attributionName"><font face="Courier">RShiny@UCLouvain</font></span> is made available under the terms of the <a rel="license"
        href="http://creativecommons.org/licenses/by/2.0/be/" target="_blank">Creative Commons Attribution 2.0 Belgium license</a>. Details on the use of this resource on <a href="http://sites.uclouvain.be/RShiny"
        target="_blank"><font face="Courier">RShiny@UCLouvain</font></a>. Source code available on <a href="https://github.com/AntoineSoetewey/statistics-201" target="_blank">GitHub</a>.')
    ),

    mainPanel(
      conditionalPanel(
        condition = "input.inference == 'one mean'",
        uiOutput("results_onemean")
      ),
      conditionalPanel(
        condition = "input.inference == 'two means'",
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
  )
)

server <- function(input, output) {
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
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
    # p <- (1 - pnorm((M-m0)/S))
    LCL <- (M - S * qnorm(1 - alpha / 2))
    UCL <- (M + S * qnorm(1 - alpha / 2))
    value <- list(mean = M, m0 = m0, sigma = sigma, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
    # print(sprintf("P-value = %g",p))
    # print(sprintf("Lower %.2f%% Confidence Limit = %g",
    #               alpha, LCL))
    # print(sprintf("Upper %.2f%% Confidence Limit = %g",
    #               alpha, UCL))
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
    # p <- (1 - pnorm((M-m0)/S))
    LCL <- (M1 - M2 - S * qnorm(1 - alpha / 2))
    UCL <- (M1 - M2 + S * qnorm(1 - alpha / 2))
    value <- list(mean1 = M1, mean2 = M2, m0 = m0, sigma1 = sigma1, sigma2 = sigma2, S = S, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
    # print(sprintf("P-value = %g",p))
    # print(sprintf("Lower %.2f%% Confidence Limit = %g",
    #               alpha, LCL))
    # print(sprintf("Upper %.2f%% Confidence Limit = %g",
    #               alpha, UCL))
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

  output$results_onemean <- renderUI({
    dat <- extract(input$sample_onemean)
    if (anyNA(dat) | length(dat) < 2) {
      "Invalid input or not enough observations"
    } else if (input$inference == "one mean" & input$popsd_onemean == FALSE) {
      test_confint <- t.test(x = dat, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha)
      test <- t.test(x = dat, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      withMathJax(
        paste(c("Your data:", paste(dat, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n =\\) ", length(dat)),
        br(),
        paste0("\\(\\bar{x} =\\) ", round(mean(dat), 3)),
        br(),
        paste0("\\(s =\\) ", round(sd(dat), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(\\mu = \\bar{x} \\pm t_{\\alpha/2, n - 1} \\dfrac{s}{\\sqrt{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr * sqrt(length(dat)), 3), " / ", round(sqrt(length(dat)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
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
      test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      withMathJax(
        paste(c("Your data:", paste(dat, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n =\\) ", length(dat)),
        br(),
        paste0("\\(\\bar{x} =\\) ", round(mean(dat), 3)),
        br(),
        paste0("\\(\\sigma =\\) ", round(sqrt(input$sigma2_onemean), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% Confidence Interval for \\(\\mu = \\bar{x} \\pm z_{\\alpha/2} \\dfrac{\\sigma}{\\sqrt{n}} = \\) ",
          round(test$mean, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$sigma, 3), " / ", round(sqrt(length(dat)), 3), "\\( ) \\) ", "\\( = \\) ",
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
          "(", round(test$mean, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$sigma / sqrt(length(dat)), 3), " \\( = \\) ",
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
      print("loading...")
    }
  })

  output$results_twomeanspaired <- renderUI({
    dat1 <- extract(input$sample1_twomeanspaired)
    dat2 <- extract(input$sample2_twomeanspaired)
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Invalid input or not enough observations"
    } else if (length(dat1) != length(dat2)) {
      "Number of observations must be equal in the two samples"
    } else if (input$inference == "two means (paired samples)" & input$popsd_twomeanspaired == FALSE) {
      test_confint <- t.test(x = dat2, y = dat1, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha, paired = TRUE)
      test <- t.test(x = dat2, y = dat1, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = TRUE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste(c("Difference \\((D) = Sample_2 - Sample_1=\\)", paste(dat2 - dat1, collapse = ", ")), collapse = " "),
        br(),
        paste0("Number of pairs \\(n =\\) ", length(dat1)),
        br(),
        paste0("\\(\\bar{D} =\\) ", round(mean(dat2 - dat1), 3)),
        br(),
        paste0("\\(s^2_D =\\) ", round(var(dat2 - dat1), 3)),
        br(),
        paste0("\\(s_D =\\) ", round(sd(dat2 - dat1), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(\\mu_D = \\bar{D} \\pm t_{\\alpha/2, n - 1} \\dfrac{s_D}{\\sqrt{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr * sqrt(length(dat1)), 3), " / ", round(sqrt(length(dat1)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
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
      test <- t.test2(x = dat2 - dat1, V = input$sigma2_twomeanspaired, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste(c("Difference \\((D) = Sample_2 - Sample_1=\\)", paste(dat2 - dat1, collapse = ", ")), collapse = " "),
        br(),
        paste0("Number of pairs \\(n =\\) ", length(dat1)),
        br(),
        paste0("\\(\\bar{D} =\\) ", round(mean(dat2 - dat1), 3)),
        br(),
        paste0("\\(\\sigma^2_D =\\) ", round(input$sigma2_twomeanspaired, 3)),
        br(),
        paste0("\\(\\sigma_D =\\) ", round(sqrt(input$sigma2_twomeanspaired), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% Confidence Interval for \\(\\mu_D = \\bar{D} \\pm z_{\\alpha/2} \\dfrac{\\sigma_D}{\\sqrt{n}} = \\) ",
          round(test$mean, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$sigma, 3), " / ", round(sqrt(length(dat1)), 3), "\\( ) \\) ", "\\( = \\) ",
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
          "(", round(test$mean, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$sigma / sqrt(length(dat1)), 3), " \\( = \\) ",
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
      print("loading...")
    }
  })

  output$results_twomeans <- renderUI({
    dat1 <- extract(input$sample1_twomeans)
    dat2 <- extract(input$sample2_twomeans)
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Invalid input or not enough observations"
    } else if (input$inference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == TRUE) {
      test_confint <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
      s_p <- sqrt(((length(dat1) - 1) * var(dat1) + (length(dat2) - 1) * var(dat2)) / test_confint$parameter)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(mean(dat1), 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(mean(dat2), 3)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(var(dat1), 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(var(dat2), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0((1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, n_1 + n_2 - 2} (s_p) \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}} \\)"),
        br(),
        paste0("where ", "\\( s_p = \\sqrt{\\dfrac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2}{n_1 + n_2 - 2}} = \\) ", round(s_p, 3)),
        br(),
        br(),
        paste0(
          "\\( \\Rightarrow \\)", (1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\) ", round(test_confint$estimate[1], 3), ifelse(test_confint$estimate[2] >= 0, paste0(" - ", round(test_confint$estimate[2], 3)), paste0(" + ", round(abs(test_confint$estimate[2]), 3))), " \\( \\pm \\) ", "\\( (\\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(s_p, 3), " * ", round(sqrt(1 / length(dat1) + 1 / length(dat2)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{s_p \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}}} = \\) ",
          "(", round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / (", round(s_p, 3), " * ", round(sqrt((1 / length(dat1)) + (1 / length(dat2))), 3), ") \\( = \\) ",
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
    } else if (input$inference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == FALSE) {
      test_confint <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(mean(dat1), 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(mean(dat2), 3)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(var(dat1), 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(var(dat2), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0((1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}} \\)"),
        br(),
        paste0("where ", "\\( \\nu = \\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}\\Bigg)^2}{\\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1}\\Bigg)^2}{n_1-1} + \\dfrac{\\Bigg(\\dfrac{s^2_2}{n_2}\\Bigg)^2}{n_2-1}} = \\) ", round(test$parameter, 3)),
        br(),
        br(),
        paste0(
          "\\( \\Rightarrow \\)", (1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\) ", round(test_confint$estimate[1], 3), ifelse(test_confint$estimate[2] >= 0, paste0(" - ", round(test_confint$estimate[2], 3)), paste0(" + ", round(abs(test_confint$estimate[2]), 3))), " \\( \\pm \\) ", "\\( (\\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
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
    } else if (input$inference == "two means" & input$popsd_twomeans == TRUE) {
      test <- t.test3(x = dat1, y = dat2, V1 = input$sigma21_twomeans, V2 = input$sigma22_twomeans, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(mean(dat1), 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(mean(dat2), 3)),
        br(),
        paste0("\\(\\sigma^2_1 =\\) ", round(input$sigma21_twomeans, 3)),
        br(),
        paste0("\\(\\sigma^2_2 =\\) ", round(input$sigma22_twomeans, 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}} = \\) ",
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
          "2. Test statistic : \\(z_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}}} = \\) ",
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
      print("loading...")
    }
  })

  output$results_oneprop <- renderUI({
    if (input$inference == "one proportion" & input$propx_oneprop == "prop_true") {
      test <- prop.z.test(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test2 <- prop.z.test3(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test_confint <- prop.z.test(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided")
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n =\\) ", round(test$n, 3)),
        br(),
        paste0("\\(\\hat{p} =\\) ", round(test$estimate, 3)),
        br(),
        paste0("\\(\\hat{q} = 1 - \\hat{p} =\\) ", round(1 - test$estimate, 3)),
        br(),
        helpText(paste0("\\( n\\hat{p} = \\) ", round(test$n * test$estimate, 3), " and \\( n(1-\\hat{p}) = \\) ", round(test$n * (1 - test$estimate), 3))),
        helpText(paste0("Assumptions \\( n\\hat{p} \\geq 5\\) and \\( n(1-\\hat{p}) \\geq 5\\)", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p = \\hat{p} \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p = \\) ", test$null.value, " and \\(H_1 : p \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\hat{p} - p_0}{\\sqrt{\\dfrac{p_0(1-p_0)}{n}}} = \\) ",
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
      test <- prop.z.test(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test2 <- prop.z.test3(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test_confint <- prop.z.test(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided")
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n =\\) ", round(test$n, 3)),
        br(),
        paste0("\\(\\hat{p} = \\dfrac{x}{n} = \\) ", test$x, " \\( / \\) ", test$n, " \\( = \\) ", round(test$estimate, 3)),
        br(),
        paste0("\\(\\hat{q} = 1 - \\hat{p} = \\) ", round(1 - test$estimate, 3)),
        br(),
        helpText(paste0("\\( n\\hat{p} = \\) ", round(test$n * test$estimate, 3), " and \\( n(1-\\hat{p}) = \\) ", round(test$n * (1 - test$estimate), 3))),
        helpText(paste0("Assumptions \\( n\\hat{p} \\geq 5\\) and \\( n(1-\\hat{p}) \\geq 5\\)", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p = \\hat{p} \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p = \\) ", test$null.value, " and \\(H_1 : p \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\hat{p} - p_0}{\\sqrt{\\dfrac{p_0(1-p_0)}{n}}} = \\) ",
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
      print("loading...")
    }
  })

  output$results_twoprop <- renderUI({
    if (input$inference == "two proportions" & input$propx_twoprop == "prop_true" & input$pooledstderr_twoprop == FALSE) {
      test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      test_confint <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\hat{p}_1 =\\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{p}_2 =\\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) and \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}} = \\) ",
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
      test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      test_confint <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\hat{p}_1 = \\dfrac{x_1}{n_1} = \\) ", test$x1, " \\( / \\) ", test$n1, " \\( = \\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{p}_2 = \\dfrac{x_2}{n_2} = \\) ", test$x2, " \\( / \\) ", test$n2, " \\( = \\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) and \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}} = \\) ",
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
        paste("Your data:"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\hat{p}_1 =\\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{p}_2 =\\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) and \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0("2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} \\) "),
        br(),
        paste0("where ", "\\( \\hat{p} = \\dfrac{n_1 \\hat{p}_1 + n_2 + \\hat{p}_2}{n_1 + n_2} = \\) ", "(", test$n1, " * ", round(test$estimate1, 3), " + ", test$n2, " * ", round(test$estimate2, 3), ") / (", test$n1, " + ", test$n2, ") = ", round(test$pooled.phat, 3)),
        br(),
        paste0(
          "\\( \\Rightarrow z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} = \\) ",
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
      test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = TRUE)
      test_confint <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\hat{p}_1 = \\dfrac{x_1}{n_1} = \\) ", test$x1, " \\( / \\) ", test$n1, " \\( = \\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{p}_2 = \\dfrac{x_2}{n_2} = \\) ", test$x2, " \\( / \\) ", test$n2, " \\( = \\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) and \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0("2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} \\) "),
        br(),
        paste0("where ", "\\( \\hat{p} = \\dfrac{n_1 \\hat{p}_1 + n_2 + \\hat{p}_2}{n_1 + n_2} = \\) ", "(", test$n1, " * ", round(test$estimate1, 3), " + ", test$n2, " * ", round(test$estimate2, 3), ") / (", test$n1, " + ", test$n2, ") = ", round(test$pooled.phat, 3)),
        br(),
        paste0(
          "\\( \\Rightarrow z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} = \\) ",
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
      print("loading...")
    }
  })

  output$results_onevar <- renderUI({
    dat <- extract(input$sample_onevar)
    if (anyNA(dat) | length(dat) < 2) {
      "Invalid input or not enough observations"
    } else if (input$h0 <= 0) {
      withMathJax(
        sprintf("\\( \\sigma^2_0 \\) must be > 0")
      )
    } else if (input$inference == "one variance") {
      test_confint <- varTest(x = dat, sigma.squared = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha)
      test <- varTest(x = dat, sigma.squared = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      withMathJax(
        paste(c("Your data:", paste(dat, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n =\\) ", length(dat)),
        br(),
        paste0("\\(s^2 =\\) ", round(var(dat), 3)),
        br(),
        paste0("\\(s =\\) ", round(sd(dat), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(\\sigma^2 = \\Bigg[ \\dfrac{(n-1)s^2}{\\chi^2_{\\alpha/2, n-1}} ; \\dfrac{(n-1)s^2}{\\chi^2_{1-\\alpha/2, n-1}} \\Bigg] = \\) ",
          "[(", round((length(dat) - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = FALSE), 3), ") ; (", round((length(dat) - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = TRUE), 3), ")] = ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\sigma^2 = \\) ", test$null.value, " and \\(H_1 : \\sigma^2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(\\chi^2_{obs} = \\dfrac{(n-1)s^2}{\\sigma^2_0} = \\) ",
          "[(", length(dat), " - 1) * ", round(test$estimate, 3), "] / ", test$null.value, " \\( = \\) ",
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
      print("loading...")
    }
  })

  output$results_twovar <- renderUI({
    dat1 <- extract(input$sample1_twovar)
    dat2 <- extract(input$sample2_twovar)
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Invalid input or not enough observations"
    } else if (input$h0 <= 0) {
      withMathJax(
        sprintf("\\( \\sigma^2_1 - \\sigma^2_2 \\) must be > 0")
      )
    } else if (input$inference == "two variances") {
      test_confint <- var.test(x = dat1, y = dat2, ratio = 1, alternative = "two.sided", conf.level = 1 - input$alpha)
      test <- var.test(x = dat1, y = dat2, ratio = 1, alternative = input$alternative_twovar, conf.level = 1 - input$alpha)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(var(dat1), 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(var(dat2), 3)),
        br(),
        paste0("\\(s_1 =\\) ", round(sd(dat1), 3)),
        br(),
        paste0("\\(s_2 =\\) ", round(sd(dat2), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = \\Bigg[ \\dfrac{s^2_1}{s^2_2}\\dfrac{1}{F_{\\alpha/2, n_1 - 1, n_2-1}} ; \\dfrac{s^2_1}{s^2_2}F_{\\alpha/2, n_1 - 1, n_2-1} \\Bigg] = \\) ",
          "\\( \\big[ \\)", round(test_confint$estimate, 3), " * (1 / ", round(qf(input$alpha / 2, df1 = test_confint$parameter[1], df2 = test_confint$parameter[2], lower.tail = FALSE), 3), "); ", round(test_confint$estimate, 3), " * ", round(qf(input$alpha / 2, df1 = test_confint$parameter[1], df2 = test_confint$parameter[2], lower.tail = FALSE), 3), "\\( \\big] = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
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
          "[", round(var(dat1), 3), " / ", round(var(dat2), 3), "]", " \\( = \\) ",
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
      print("loading...")
    }
  })

  output$plot <- renderPlot({
    if (input$inference == "one mean" & input$popsd_onemean == FALSE) {
      dat <- extract(input$sample_onemean)
      test <- t.test(x = dat, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
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
      dat <- extract(input$sample_onemean)
      test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
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
    } else if (input$inference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == TRUE) {
      dat1 <- extract(input$sample1_twomeans)
      dat2 <- extract(input$sample2_twomeans)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
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
    } else if (input$inference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == FALSE) {
      dat1 <- extract(input$sample1_twomeans)
      dat2 <- extract(input$sample2_twomeans)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
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
    } else if (input$inference == "two means" & input$popsd_twomeans == TRUE) {
      dat1 <- extract(input$sample1_twomeans)
      dat2 <- extract(input$sample2_twomeans)
      test <- t.test3(x = dat1, y = dat2, V1 = input$sigma21_twomeans, V2 = input$sigma22_twomeans, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
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
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test(x = dat2, y = dat1, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = TRUE)
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
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test2(x = dat2 - dat1, V = input$sigma2_twomeanspaired, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
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
        test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      } else if (input$propx_twoprop == "prop_true" & input$pooledstderr_twoprop == TRUE) {
        test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = TRUE)
      } else if (input$propx_twoprop == "prop_false" & input$pooledstderr_twoprop == TRUE) {
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
      dat <- extract(input$sample_onevar)
      test <- varTest(x = dat, sigma.squared = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
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
      dat1 <- extract(input$sample1_twovar)
      dat2 <- extract(input$sample2_twovar)
      test <- var.test(x = dat1, y = dat2, ratio = 1, alternative = input$alternative_twovar, conf.level = 1 - input$alpha)
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
      print("loading...")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
