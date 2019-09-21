## app.R ##
library(shinydashboard)
library(plotly)
library(ggplot2)

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(plotlyOutput("plot1", height = 250)),
            
            box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
            )
        )
    )
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlotly({
        plot_ly(mtcars, x = ~mpg, y = ~wt)
    })
}

shinyApp(ui, server)