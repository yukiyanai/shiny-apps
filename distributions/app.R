## shiny-app: statistical-distributions

library("shiny")
library("ggplot2")

ui <- fluidPage(
    
    tabsetPanel(
        tabPanel(
            title = "Normal",
            tags$h2("Normal Distribution"),
            fluidRow(
                column(8,
                    plotOutput("norm"),
                    tags$br(),
                    tags$p("The red curve shows the theoretical probability density given the parameter values."),
                    tags$br(),
                    tags$h4("Sample statistics"),
                    verbatimTextOutput("normstats")
                ),
                column(4,
                    tags$br(), tags$br(),
                    wellPanel(
                        tags$h3("Parameters"),
                        sliderInput("norm1", label = "Mean",
                                    min = -50, max = 50, value = 0),
                        sliderInput("norm2", label = "Standard Deviation",
                                    min = 1, max = 20, value = 5)
                    ),
                    sliderInput("norm0", label = "Sample Size (n)",
                                min = 1, max = 500, value = 200),
                    tags$hr(),
                    actionButton("renorm", label = "Resample")
                )
            )
        ),
        tabPanel(
            title = "Uniform",
            tags$h2("Continuous Uniform Distribution"),
            fluidRow(
                column(8,
                    plotOutput("unif"),
                    tags$br(),
                    tags$p("The red line shows the theoretical probability density given the parameter values."),
                    tags$br(),
                    tags$h4("Sample statistics"),
                    verbatimTextOutput("unifstats")
                ),
                column(4,
                    tags$br(), tags$br(),
                    wellPanel(
                        tags$h3("Parameters"),
                        sliderInput("unif1", label = "Minimum and Maximum",
                                    min = -10, max = 10, value = c(0, 1))
                    ),
                    sliderInput("unif0", label = "Sample Size (n)",
                                min = 1, max = 500, value = 200),
                    tags$hr(),
                    actionButton("reunif", label = "Resample")
                )
            )
        ),
        tabPanel(
            title = "t",
            tags$h2("Student's t Distribution"),
            fluidRow(
                column(8,
                    plotOutput("t"),
                    tags$br(),
                    tags$p("The red curve shows the theoretical probability density given the parameter values."),
                    tags$p("The blue dashed curve shows the PDF of the standard normal distribution."),
                    tags$br(),
                    tags$h4("Sample statistics"),
                    verbatimTextOutput("tstats")
                ),
                column(4,
                    tags$br(), tags$br(),
                    wellPanel(
                        tags$h3("Parameter"),
                        sliderInput("t1", label = "Degree of Freedom",
                                    min = 1, max = 200, value = 10)
                    ),
                    sliderInput("t0", label = "Sample Size (n)",
                                min = 1, max = 500, value = 200),
                    tags$hr(),
                    actionButton("ret", label = "Resample")
                )
            )
        ),
        tabPanel(
            title = "Chi-squared",
            tags$h2("Chi-squared Distribution"),
            fluidRow(
                column(8,
                    plotOutput("chisq"),
                    tags$br(),
                    tags$p("The red curve shows the theoretical probability density given the parameter values."),
                    tags$br(),
                    tags$h4("Sample statistics"),
                    verbatimTextOutput("chisqstats")
                ),
                column(4,
                    tags$br(), tags$br(),
                    wellPanel(
                        tags$h3("Parameter"),
                        sliderInput("chisq1", label = "Degree of Freedom",
                                    min = 1, max = 100, value = 1)
                    ),
                    sliderInput("chisq0", label = "Sample Size (n)",
                                min = 1, max = 500, value = 200),
                    tags$hr(),
                    actionButton("rechisq", label = "Resample")
                )
            )
        )
    )
)

server <- function(input, output) {
    
    rv <- reactiveValues(
        norm = rnorm(200, mean = 0, sd = 5),
        normx = seq(-20, 20, length = 200),
        normy = dnorm(seq(-20, 20, length = 200), 0, 5),
        unif = runif(200, min = 0, max = 1),
        unifx = seq(0, 1, length = 200),
        unify = rep(1, 200),
        t = rt(200, df = 10),
        tx = seq(-4, 4, length = 200),
        ty = dt(seq(-4, 4, length = 200), df = 10),
        stdnorm = dnorm(seq(-4, 4, length = 200), 0, 1),
        chisq = rchisq(200, df = 1),
        chisqx = seq(.1, 8, length = 200),
        chisqy = dchisq(seq(.1, 8, length = 200), df = 1)
    )
    
    
    ## Normal Distribution
    observeEvent(input$renorm, {
            rv$norm <- rnorm(input$norm0, input$norm1, input$norm2)
            rv$normx <- seq(min(rv$norm), max(rv$norm), length = input$norm0)
            rv$normy <- dnorm(rv$normx, input$norm1, input$norm2)
    })
    
    output$norm <- renderPlot({
        df <- data.frame(sample = rv$norm, x = rv$normx, y = rv$normy)
        p <- ggplot(df, aes(x = sample, y = ..density..)) +
            geom_histogram(color = "gray70", fill = "gray20") +
            geom_line(aes(x = x, y = y), color = "firebrick", size = 1.5) +
            xlab("random variable") + ylab("probability density") +
            isolate(ggtitle(paste0("Normal sample: mean = ", input$norm1,
                                   ", sd = ", input$norm2,
                                   ", n = ", input$norm0)))
        print(p)
    })
    
    output$normstats <- renderPrint({
        sdnorm <- sd(rv$norm)
        names(sdnorm) <- "sd"
        round(c(summary(rv$norm), sdnorm), 3)
    })
    
    
    ## Uniform Distribution
    observeEvent(input$reunif, {
        rv$unif <- runif(input$unif0, input$unif1[1], input$unif1[2])
        rv$unifx <- seq(input$unif1[1], input$unif1[2], length = input$unif0)
        rv$unify <- dunif(rv$unifx, input$unif1[1], input$unif1[2])
    })
    
    output$unif <- renderPlot({
        df <- data.frame(sample = rv$unif, x = rv$unifx, y = rv$unify)
        p <- ggplot(df, aes(x = sample, y = ..density..)) +
            geom_histogram(color = "gray70", fill = "gray20") +
            geom_line(aes(x = x, y = y), color = "firebrick", size = 1.5) +
            xlab("random variable") + ylab("probability density") +
            isolate(ggtitle(paste0("Uniform sample: min = ", input$unif1[1],
                                   ", max = ", input$unif1[2],
                                   ", n = ", input$unif0)))
        print(p)
    })
    
    output$unifstats <- renderPrint({
        sdunif <- sd(rv$unif)
        names(sdunif) <- "sd"
        round(c(summary(rv$unif), sdunif), 3)
    })
    
    
    ## t Distribution
    observeEvent(input$ret, {
        rv$t <- rt(input$t0, input$t1)
        rv$tx <- seq(min(rv$t), max(rv$t), length = input$t0)
        rv$ty <- dt(rv$tx, df = input$t1)
        rv$stdnorm <- dnorm(rv$tx, 0, 1)
    })
    
    output$t <- renderPlot({
        df <- data.frame(sample = rv$t, x = rv$tx, y = rv$ty, stdnorm = rv$stdnorm)
        p <- ggplot(df, aes(x = sample, y = ..density..)) +
            geom_histogram(color = "gray70", fill = "gray20") +
            geom_line(aes(x = x, y = stdnorm), color = "blue", linetype = "dashed", size = 1.5) +
            geom_line(aes(x = x, y = y), color = "firebrick", size = 1.5) +
            xlab("random variable") + ylab("probability density") +
            isolate(ggtitle(paste0("t sample: df = ", input$t1,
                                   ", n = ", input$t0)))
        print(p)
    })
    
    output$tstats <- renderPrint({
        sdt <- sd(rv$t)
        names(sdt) <- "sd"
        round(c(summary(rv$t), sdt), 3)
    })
    
    ## Chi-squared Distribution
    observeEvent(input$rechisq, {
        rv$chisq <- rchisq(input$chisq0, input$chisq1)
        rv$chisqx <- seq(0, max(rv$chisq), length = input$chisq0)
        rv$chisqy <- dchisq(rv$chisqx, df = input$chisq1)
    })
    
    output$chisq <- renderPlot({
        df <- data.frame(sample = rv$chisq, x = rv$chisqx, y = rv$chisqy)
        p <- ggplot(df, aes(x = sample, y = ..density..)) +
            geom_histogram(color = "gray70", fill = "gray20") +
            geom_line(aes(x = x, y = y), color = "firebrick", size = 1.5) +
            xlab("random variable") + ylab("probability density") +
            isolate(ggtitle(paste0("Chi-squared sample: df = ", input$chisq1,
                                   ", n = ", input$chisq0)))
        print(p)
    })
    
    output$chisqstats <- renderPrint({
        sdchisq <- sd(rv$chisq)
        names(sdchisq) <- "sd"
        round(c(summary(rv$chisq), sdchisq), 3)
    })
}

shinyApp(ui = ui, server = server)
