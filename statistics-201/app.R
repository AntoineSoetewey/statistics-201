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

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "inference",
                label = "Inference for:",
                choices = c("one mean", "two means", "one proportion", "two proportions", "one variance", "two variances"),
                multiple = FALSE,
                selected = "one mean"),
            hr(),
            conditionalPanel(
                condition = "input.inference == 'one mean'",
                textInput("sample_onemean", "Values", value = "1,2,3,4,5", placeholder = "Enter values separated by a comma with decimals as points"),
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
            hr(),
            sliderInput("alpha",
                        "Significance level \\(\\alpha = \\)",
                        min = 0.01,
                        max = 0.20,
                        value = 0.05),
            hr(),
            HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/statistics-201/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/statistics-201/blob/master/app.R">code</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a>.</p>')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            uiOutput("results_onemean"),
            br(),
            br()
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    extract <- function(text) {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
    }
    t.test2 <- function(x,V,m0 = 0,alpha = 0.05) {
        M<-mean(x)
        n<-length(x)
        sigma<-sqrt(V)
        S<-sqrt(V/n)
        p<-(1 - pnorm((M-m0)/S))
        LCL<-(M - S*qnorm(1-alpha/2))
        UCL<-(M + S*qnorm(1-alpha/2))
        value<-list(mean=M,sigma=sigma,p.value=p,LCL=LCL,UCL=UCL)
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
                       ifelse(input$alternative == "two.sided", "\\( \\pm \\) ", ifelse(input$alternative == "greater", "", "\\( - \\) ")), 
                       ifelse(input$alternative == "two.sided", round(qt(input$alpha/2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))),
                br(),
                paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "\\(RH_0\\)", "\\(NRH_0\\)")),
                br(),
                br(),
                tags$b("Interpretation"),
                br(),
                paste0("At the ", input$alpha*100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", "we do not reject the null hypothesis that the true mean is "), test$null.value, ".")
            )
        } else {
            test <- t.test2(x = dat, V = input$sigma2_onemean, alpha = input$alpha)
            withMathJax(
                paste(c("Your data:", paste(dat, collapse = ", ")), collapse = " "),
                br(),
                br(),
                tags$b("Confidence interval"),
                br(),
                paste0((1-input$alpha)*100, "% Confidence Interval for \\(\\mu = \\bar{x} \\pm z_{\\alpha/2} \\frac{\\sigma}{\\sqrt{n}} = \\) ",
                       round(test$mean, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha/2, lower.tail = FALSE), 3), " * ", round(test$sigma, 3), " / ", round(sqrt(length(dat)), 3), "\\( ) \\) ", "\\( = \\) ",
                       "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]")
            )
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
