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
library(dplyr)
library(reshape2)
library(nortest)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("정규분포확인"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-value, text/plain",
                          ".csv")
                      ),
            # tableOutput('table'),
            # plotOutput('plot'),
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            hr(),
            # checkboxInput("header","Header", TRUE),
            tableOutput("normal")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           tableOutput("contents"),
           plotOutput("boxplot"),
           plotOutput("qqplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dataframe <- reactive({
        if (is.null(input$file1))
            return(NULL)                
        data <- read.csv(input$file1$datapath)
        data <- data %>% melt(measure.vars = c("April", "May", "June"))
        data
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        if (!is.null(dataframe()))
        {
            x    <- dataframe()
            x    <- x$value
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
        }

        # draw the histogram with the specified number of bins
        if (!is.null(dataframe()))
            hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$contents <- renderTable({
        head(dataframe())
    })
    
    output$boxplot <- renderPlot({
        if (!is.null(dataframe()))
            ggplot(dataframe(), aes(x=variable, y=value)) + geom_boxplot() + ylab("RMS25")
    })
    
    output$qqplot <- renderPlot({
        if (!is.null(dataframe())){
            # 30개 평균
            x    <- dataframe()
            x    <- x$value
            qqnorm(x)
            qqline(x,col="red",lwd=2)
        }
    })
    
    output$normal <- renderPrint({
        #Anderson-Darling test
        if (!is.null(dataframe())){
            x    <- dataframe()
            x    <- x$value
            y <- ad.test(x)
            cat(y$method, y$p.value, sep = '\n')
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
