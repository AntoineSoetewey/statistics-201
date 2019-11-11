#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Statistics 201 - Inference"),
    h4(tags$a(href="https://www.antoinesoetewey.com/", "Antoine Soetewey")),
    withMathJax(),

    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "inference",
                label = "Inference for:",
                choices = c("one mean", "two means", "one proportion", "two proportions", "one variance", "two variances"),
                multiple = FALSE,
                selected = "two means"),
            hr(),
            conditionalPanel(
                condition = "input.inference == 'one mean'",
                textInput("sample_onemean", "Sample", value = "4.9, 4.8, 5, 5.3, 5.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
                hr(),
                checkboxInput("popsd_onemean", "Variance of the population is known", FALSE),
                conditionalPanel(
                    condition = "input.popsd_onemean == 1",
                    numericInput("sigma2_onemean", "\\(\\sigma^2 = \\)",
                                 value = 1, min = 0, step = 1)
                ),
                hr(),
                # helpText("Hypothesis test:"),
                numericInput("h0", "Null hypothesis \\(H_0 :\\mu = \\)",
                             value = 0, step = 1),
                radioButtons(
                    inputId = "alternative",
                    label = "Alternative",
                    choices = c(
                        "\\( \\neq \\)" = "two.sided",
                        "\\( > \\)" = "greater",
                        "\\( < \\)" = "less")
                )
            ),
            conditionalPanel(
                condition = "input.inference == 'two means'",
                textInput("sample1_twomeans", "Sample 1", value = "1,2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
                textInput("sample2_twomeans", "Sample 2", value = "3,5", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
                hr(),
                conditionalPanel(
                    condition = "input.popsd_twomeans == 0",
                    radioButtons(
                        inputId = "var.equal",
                        label = "Assume",
                        choices = c(
                            "\\( \\sigma^2_1 = \\sigma^2_2 \\)" = TRUE,
                            "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = FALSE)
                    )
                ),
                checkboxInput("popsd_twomeans", "Variance of the populations are known", FALSE),
                conditionalPanel(
                    condition = "input.popsd_twomeans == 1",
                    numericInput("sigma21_twomeans", "\\(\\sigma^2_1 = \\)",
                                 value = 1, min = 0, step = 1),
                    numericInput("sigma22_twomeans", "\\(\\sigma^2_2 = \\)",
                                 value = 1, min = 0, step = 1)
                ),
                hr(),
                # helpText("Hypothesis test:"),
                numericInput("h0", "Null hypothesis \\(H_0 :\\mu_1 - \\mu_2 = \\)",
                             value = 0, step = 1),
                radioButtons(
                    inputId = "alternative",
                    label = "Alternative",
                    choices = c(
                        "\\( \\neq \\)" = "two.sided",
                        "\\( > \\)" = "greater",
                        "\\( < \\)" = "less")
                )
            ),
            hr(),
            sliderInput("alpha",
                        "Significance level \\(\\alpha = \\)",
                        min = 0.01,
                        max = 0.20,
                        value = 0.05),
            hr(),
            HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/statistics-201/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/statistics-201/blob/master/app.R">code</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a>.</p>')
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
                condition = "input.inference == 'one proportion'",
                uiOutput("results_oneproportion")
            ),
            conditionalPanel(
                condition = "input.inference == 'two proportions'",
                uiOutput("results_twoproportions")
            ),
            conditionalPanel(
                condition = "input.inference == 'one variance'",
                uiOutput("results_onevariance")
            ),
            conditionalPanel(
                condition = "input.inference == 'two variances'",
                uiOutput("results_twovariances")
            ),
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
    t.test2 <- function(x,V,m0 = 0,alpha = 0.05, alternative = "two.sided") {
        M<-mean(x)
        n<-length(x)
        sigma<-sqrt(V)
        S<-sqrt(V/n)
        statistic <- (M - m0) / S
        p <- if (alternative == "two.sided") {
            2*pnorm(abs(statistic), lower.tail = FALSE)
        } else if (alternative == "less") {
            pnorm(statistic, lower.tail = TRUE)
        } else {
            pnorm(statistic, lower.tail = FALSE)
        }
        # p <- (1 - pnorm((M-m0)/S))
        LCL<-(M - S*qnorm(1-alpha/2))
        UCL<-(M + S*qnorm(1-alpha/2))
        value<-list(mean=M,m0=m0,sigma=sigma,statistic=statistic,p.value=p,LCL=LCL,UCL=UCL,alternative=alternative)
        # print(sprintf("P-value = %g",p))
        # print(sprintf("Lower %.2f%% Confidence Limit = %g",
        #               alpha, LCL))
        # print(sprintf("Upper %.2f%% Confidence Limit = %g",
        #               alpha, UCL))
        return(value)
    }
    
    output$results_onemean <- renderUI({
        dat <- extract(input$sample_onemean)
        if (anyNA(dat) | length(dat) < 2) {
            "Invalid input or not enough observations"
        } else if (input$inference == "one mean" & input$popsd_onemean == FALSE) {
            test_confint <- t.test(x = dat, mu = input$h0, alternative = "two.sided", conf.level = 1-input$alpha)
            test <- t.test(x = dat, mu = input$h0, alternative = input$alternative, conf.level = 1-input$alpha)
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
                paste0((1-input$alpha)*100, "% CI for \\(\\mu = \\bar{x} \\pm t_{\\alpha/2, n - 1} \\frac{s}{\\sqrt{n}} = \\) ",
                       round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha/2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr*sqrt(length(dat)), 3), " / ", round(sqrt(length(dat)), 3), "\\( ) \\) ", "\\( = \\) ",
                       "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
                br(),
                br(),
                tags$b("Hypothesis test"),
                br(),
                paste0("1. \\(H_0 : \\mu = \\) ", test$null.value, " and \\(H_1 : \\mu \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
                br(),
                paste0("2. Test statistic : \\(t_{obs} = \\tfrac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = \\) ",
                       "(", round(test$estimate, 3), " - ", test$null.value, ") / ", round(test$stderr, 3), " \\( = \\) ",
                       round(test$statistic, 3)),
                br(),
                paste0("3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", ifelse(input$alternative == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
                       ifelse(input$alternative == "two.sided", input$alpha/2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
                       ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")), 
                       ifelse(input$alternative == "two.sided", round(qt(input$alpha/2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))),
                br(),
                paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
                br(),
                br(),
                tags$b("Interpretation"),
                br(),
                paste0("At the ", input$alpha*100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", "we do not reject the null hypothesis that the true mean is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
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
                paste0((1-input$alpha)*100, "% Confidence Interval for \\(\\mu = \\bar{x} \\pm z_{\\alpha/2} \\frac{\\sigma}{\\sqrt{n}} = \\) ",
                       round(test$mean, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha/2, lower.tail = FALSE), 3), " * ", round(test$sigma, 3), " / ", round(sqrt(length(dat)), 3), "\\( ) \\) ", "\\( = \\) ",
                       "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"),
                br(),
                br(),
                tags$b("Hypothesis test"),
                br(),
                paste0("1. \\(H_0 : \\mu = \\) ", input$h0, " and \\(H_1 : \\mu \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
                br(),
                paste0("2. Test statistic : \\(z_{obs} = \\tfrac{\\bar{x} - \\mu_0}{\\sigma / \\sqrt{n}} = \\) ",
                       "(", round(test$mean, 3), " - ", input$h0, ") / ", round(test$sigma / sqrt(length(dat)), 3), " \\( = \\) ",
                       round(test$statistic, 3)),
                br(),
                paste0("3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
                       ifelse(input$alternative == "two.sided", input$alpha/2, input$alpha), "\\()\\)", " \\( = \\) ",
                       ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")), 
                       ifelse(input$alternative == "two.sided", round(qnorm(input$alpha/2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))),
                br(),
                paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
                br(),
                br(),
                tags$b("Interpretation"),
                br(),
                paste0("At the ", input$alpha*100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", "we do not reject the null hypothesis that the true mean is "), input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
            )
        } else {
            print("in progress")
        }
    })
    
    output$results_twomeans <- renderUI({
        dat1 <- extract(input$sample1_twomeans)
        dat2 <- extract(input$sample2_twomeans)
        if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
            "Invalid input or not enough observations"
        } else if (input$inference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == TRUE) {
            test_confint <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = "two.sided", conf.level = 1-input$alpha, paired = FALSE, var.equal = TRUE)
            test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1-input$alpha, paired = FALSE, var.equal = TRUE)
            s_p <- sqrt( ((length(dat1)-1)*var(dat1) + (length(dat2)-1)*var(dat2)) / test_confint$parameter )
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
                paste0((1-input$alpha)*100, "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, n_1 + n_2 - 2} (s_p) \\sqrt{\\frac{1}{n_1} + \\frac{1}{n_2}} \\)"),
                br(),
                paste0("where ", "\\( s_p = \\sqrt{\\frac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2}{n_1 + n_2 - 2}} = \\)", round(s_p, 3)),
                br(),
                paste0("So ", (1-input$alpha)*100, "% CI \\( = \\) ", round(test_confint$estimate[1], 3), " - ", round(test_confint$estimate[2], 3), " \\( \\pm \\) ", "\\( (\\)" , round(qt(input$alpha/2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(s_p, 3), " * ", round(sqrt(1/length(dat1) + 1/length(dat2)), 3), "\\( ) \\) ", "\\( = \\) ",
                       "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"),
                br(),
                br(),
                tags$b("Hypothesis test"),
                br(),
                br(),
                tags$b("Interpretation"),
                br()
            )
        } else if (input$inference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == FALSE) {
            print("two means with unequal variances")
        } else if (input$inference == "two means" & input$popsd_twomeans == TRUE) {
            print("two means with known variances")
        } else {
            print("in progress")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
