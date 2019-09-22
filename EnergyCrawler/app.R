## app.R ##
library(shinydashboard)
library(plotly)
library(ggplot2)
library(DT)
library(tidyr)
library(ECharts2Shiny)

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
            ,menuItem("Widgets", tabName = "widgets", icon = icon("th"))
            ,menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
            ,menuItem("Data", tabName = "data", icon = icon("table"))
            #,uiOutput("field_chooser_ui")
            )
        )
    ## Body content
    ,dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard"
        ,fluidRow(
            tabBox(width = 500, height = 500
            ,tabPanel(title= "Energy Overview", plotlyOutput("plot1"))
            ,tabPanel(title= "Month Over Month Trends", plotlyOutput("plot2"))
            ,tabPanel(title= "Savings", plotlyOutput("plot3"))
            )
            )
        )
        , tabItem(tabName = "widgets"
                  ,fluidRow(
                      #column(6,
                      # A static valueBox
                      #adjust height of value boxes for presentation
                      tags$head(tags$style(HTML(".small-box {height: 200px}")))
                      ,valueBox(10 * 2, "New Orders", icon = icon("credit-card"))
                      # Dynamic valueBoxes
                      ,valueBoxOutput("progressBox")
                      ,valueBoxOutput("approvalBox")
                      ,    # We MUST load the ECharts javascript library in advance
                      loadEChartsLibrary(),
                      tags$div(id="test", style="width:50%;height:400px;"),
                      deliverChart(div_id = "test")
                  )
                  #)
                )
        
        , tabItem(tabName = "data",
                  # Show data table ---------------------------------------------
                  checkboxInput(inputId = "show_data",
                                label = "Show data table",
                                value = TRUE)
                  , fluidRow(
                      # Show data table ---------------------------------------------
                      DT::dataTableOutput(outputId = "energytable")
                      )
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
    
    # Call functions from ECharts2Shiny to render charts
    renderGauge(div_id = "test",rate = 99, gauge_name = "Finish Rate")
    
    # Print data table if checked -------------------------------------
    output$energytable <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = mtcars,
                          
                          # Enable Buttons --------------------------------
                          extensions = 'Buttons',
                          options = list(pageLength = 10,
                                         
                                         # Turn off search ----------------
                                         dom = "Btp",
                                         
                                         # Buttons available --------------
                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
                          rownames = FALSE) %>% 
                
                # Format text example ---------------------------------------
            formatStyle(
                columns = 5, 
                valueColumns = 5, 
                color = styleEqual(c("R","G", "PG", "PG-13"), c("red", "green", "blue", "yellow"))
            ) %>%
                
                # Format background example ---------------------------------
            formatStyle(
                columns = 8,
                background = styleColorBar(range(mtcars), '#cab2d6'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')
        }
    )
    
}

shinyApp(ui, server)