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
                    sliderInput("norm0", label = "Sample Size",
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
                    sliderInput("unif0", label = "Sample Size",
                                min = 1, max = 500, value = 200),
                    tags$hr(),
                    actionButton("reunif", label = "Resample")
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
        unify = rep(1, 200)
    )
    
    
    ## Normal Distribution
    observeEvent(input$renorm, {
            rv$norm <- rnorm(input$norm0, input$norm1, input$norm2)
            rv$normx <- seq(min(rv$norm), max(rv$norm), length = input$norm0)
            rv$normy <- dnorm(rv$normx, input$norm1, input$norm2)
          }
    )
    
    output$norm <- renderPlot({
        df <- data.frame(sample = rv$norm, x = rv$normx, y = rv$normy)
        p <- ggplot(df, aes(x = sample, y = ..density..)) +
            geom_histogram(color = "grey", fill = "darkblue") +
            geom_line(aes(x = x, y = y), color = "firebrick", size = 1.5) +
            xlab("random variable") + ylab("probability density") +
            isolate(ggtitle(paste("Normal sample: mean =", input$norm1, ", sd =", input$norm2)))
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
    }
    )
    
    output$unif <- renderPlot({
        df <- data.frame(sample = rv$unif, x = rv$unifx, y = rv$unify)
        p <- ggplot(df, aes(x = sample, y = ..density..)) +
            geom_histogram(color = "grey", fill = "darkblue") +
            geom_line(aes(x = x, y = y), color = "firebrick", size = 1.5) +
            xlab("random variable") + ylab("probability density") +
            isolate(ggtitle(paste("Uniform sample: min =", input$unif1[1], ", max =", input$unif1[2])))
        print(p)
    })
    
    output$unifstats <- renderPrint({
        sdunif <- sd(rv$unif)
        names(sdunif) <- "sd"
        round(c(summary(rv$unif), sdunif), 3)
    })
}

shinyApp(ui = ui, server = server)
