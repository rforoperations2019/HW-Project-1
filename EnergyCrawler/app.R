## app.R ##
library(shinydashboard)
library(plotly)
library(ggplot2)
library(DT)
library(tidyr)

# Increase maximum upload file size to 100Mb
options(shiny.maxRequestSize = 100*1024^2)

# Load Chicago energy data
#energyData <- read.csv("Chicago_Energy.csv")
#energyData<- dplyr::group_by(energyData, COMMUNITY.AREA.NAME)

#energySummary <- summary(energyData)

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard")
    ## Sidebar content
    ,dashboardSidebar(
        sidebarMenu(
            fileInput("file", "CSV file")
            ,actionButton("submit", label = "Submit")
            ,sliderInput("slider", "Number of observations:", 1, 100, 50)
            ,menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
            ,menuItem("Widgets", tabName = "widgets", icon = icon("th"))
            ,menuItem("Data", tabName = "data", icon = icon("table"))
            #,uiOutput("field_chooser_ui")
            )
        )
    ## Body content
    ,dashboardBody(
        fluidRow(
            # A static valueBox
            valueBox(10 * 2, "New Orders", icon = icon("credit-card"))
            # Dynamic valueBoxes
            ,valueBoxOutput("progressBox")
            ,valueBoxOutput("approvalBox")
        )
        ,fluidRow(
            tabBox(width = 500, height = 500
            ,tabPanel(title= "Energy Overview", plotlyOutput("plot1"))
            ,tabPanel(title= "Month Over Month Trends", plotlyOutput("plot2"))
            ,tabPanel(title= "Savings", plotlyOutput("plot3"))
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
    
    output$plot2 <- renderPlotly({
        plot_ly(mtcars, x = ~mpg, y = ~wt)
    })
    
    output$plot3 <- renderPlotly({
        plot_ly(mtcars, x = ~mpg, y = ~wt)
    })
    
    
    output$tabset1Selected <- renderPlotly({
        plot_ly(mtcars, x = ~mpg, y = ~wt)
    })
    
    output$table <- renderDataTable(mtcars
                                    , options = list(pageLength = 25
                                                    )
                                    )
    
    output$progressBox <- renderValueBox({
        valueBox(
            paste0(25, "%"), "Progress", icon = icon("list"),
            color = "purple"
        )
    })
    
    output$approvalBox <- renderValueBox({
        valueBox(
            "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })    
    
}

shinyApp(ui, server)