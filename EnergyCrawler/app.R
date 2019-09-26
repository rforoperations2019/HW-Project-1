## app.R ##
library(shinydashboard)
library(plotly)
library(ggplot2)
library(DT)
library(tidyr)
library(ECharts2Shiny)

# Load Data for Shiny Visualization
#file.directory <- "C:/Users/hmdsa/Documents/GitHub/hw1_asanda/Chicago_Energy.csv"
CEnergy <- read.csv("Chicago_Energy.csv")
#View(CEnergy)

# Clean data by removing incomplete reords
na.FindAndRemove <- function(mydata)
{
    # We need to find the number of na's per row. Write code that would loop through each column and find if the number of NAs is equal to the number # of rows and then remove any column with all NA's
    for (i in NCOL(mydata)) 
    {
        if (nrow(mydata[i]) == sum(sapply(mydata[i], is.na))) 
        {
            mydata <- mydata[-i]
        }
    }
    mydata <- na.omit(mydata)
    return(mydata)
}
CEnergy <- na.FindAndRemove(CEnergy)
#View(CEnergy)

#Pull Column names as list to input in dashboard
columns <- as.data.frame(colnames(CEnergy))
#View(columns)

columns.list <- split(columns, seq(nrow(columns)), rownames(columns))


column.names <- c("COMMUNITY.AREA.NAME", "CENSUS.BLOCK", "BUILDING.TYPE", "BUILDING_SUBTYPE", "KWH.JANUARY.2010", "KWH.FEBRUARY.2010", "KWH.MARCH.2010", "KWH.APRIL.2010", "KWH.MAY.2010", "KWH.JUNE.2010", "KWH.JULY.2010", "KWH.AUGUST.2010",
                  "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010", "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010", "TOTAL.KWH", "ELECTRICITY.ACCOUNTS", "ZERO.KWH.ACCOUNTS", "THERM.JANUARY.2010", "THERM.FEBRUARY.2010", "THERM.MARCH.2010", "THERM.APRIL.2010",
                  "THERM.MAY.2010", "THERM.JUNE.2010", "THERM.JULY.2010", "THERM.AUGUST.2010", "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010", "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010", "TOTAL.THERMS", "GAS.ACCOUNTS", "KWH.TOTAL.SQFT", "THERMS.TOTAL.SQFT",
                  "KWH.MEAN.2010", "KWH.STANDARD.DEVIATION.2010", "KWH.MINIMUM.2010", "KWH.1ST.QUARTILE.2010", "KWH.2ND.QUARTILE.2010", "KWH.3RD.QUARTILE.2010", "KWH.MAXIMUM.2010", "KWH.SQFT.MEAN.2010", "KWH.SQFT.STANDARD.DEVIATION.2010", "KWH.SQFT.MINIMUM.2010",
                  "KWH.SQFT.1ST.QUARTILE.2010", "KWH.SQFT.2ND.QUARTILE.2010", "KWH.SQFT.3RD.QUARTILE.2010", "KWH.SQFT.MAXIMUM.2010", "THERM.MEAN.2010", "THERM.STANDARD.DEVIATION.2010", "THERM.MINIMUM.2010", "THERM.1ST.QUARTILE.2010", "THERM.2ND.QUARTILE.2010",
                  "THERM.3RD.QUARTILE.2010", "THERM.MAXIMUM.2010", "THERMS.SQFT.MEAN.2010", "THERMS.SQFT.STANDARD.DEVIATION.2010", "THERMS.SQFT.MINIMUM.2010", "THERMS.SQFT.1ST.QUARTILE.2010", "THERMS.SQFT.2ND.QUARTILE.2010", "THERMS.SQFT.3RD.QUARTILE.2010",
                  "THERMS.SQFT.MAXIMUM.2010", "TOTAL.POPULATION", "TOTAL.UNITS", "AVERAGE.STORIES", "AVERAGE.BUILDING.AGE", "AVERAGE.HOUSESIZE", "OCCUPIED.UNITS", "OCCUPIED.UNITS.PERCENTAGE", "RENTER.OCCUPIED.HOUSING.UNITS", "RENTER.OCCUPIED.HOUSING.PERCENTAGE", "OCCUPIED.HOUSING.UNITS")
column.names <- as(column.names, "list")


# Increase maximum upload file size to 1 GB
options(shiny.maxRequestSize = 1000*1024^2)


ui <- dashboardPage(
    dashboardHeader(title = "Energy Crawler")
    ## Sidebar content
    ,dashboardSidebar(
        sidebarMenu(
            fileInput("file", "CSV file")
            ,actionButton("submit", label = "Submit")
            # Select sample size ----------------------------------------------------
            ,numericInput(inputId = "n_samp", 
                         label = "Sample size:", 
                         min = 1, max = nrow(CEnergy), 
                         value = 1)
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
                        tabBox(width = 450, height = 500
                               ,tabPanel(title= "Energy Overview"
                                         ,tags$head(tags$style(".selectize-dropdown {position: static}"))
                                         ,splitLayout(
                                             selectInput(inputId = "yplot1"
                                                         ,label = "Y-axis:"
                                                         ,choices = column.names
                                                         ,selected = "KWH.JANUARY.2010")
                                             ,selectInput(inputId = "xplot1"
                                                          ,label = "X-axis:"
                                                          ,choices = column.names
                                                          ,selected = "COMMUNITY.AREA.NAME"
                                                          )
                                             ,selectInput(inputId = "zplot1"
                                                          ,label = "Color by"
                                                          ,choices = column.names
                                                          ,selected = "BUILDING.TYPE")
                                             )
                                         , fluidRow(plotlyOutput("plot1"))
                                         )
                               ,tabPanel(title= "Month Over Month Trends"
                                         ,tags$head(tags$style(".selectize-dropdown {position: static}"))
                                         ,splitLayout(
                                             selectInput(inputId = "yplot2"
                                                         ,label = "Y-axis:"
                                                         ,choices = column.names
                                                         ,selected = "KWH.JANUARY.2010")
                                             ,selectInput(inputId = "xplot2"
                                                          ,label = "X-axis:"
                                                          ,choices = column.names
                                                          ,selected = "COMMUNITY.AREA.NAME"
                                             )
                                             ,selectInput(inputId = "zplot2"
                                                          ,label = "Color by"
                                                          ,choices = column.names
                                                          ,selected = "BUILDING.TYPE")
                                         )
                                         , fluidRow(plotlyOutput("plot2"))
                                         )
                               ,tabPanel(title= "Savings"
                                         ,tags$head(tags$style(".selectize-dropdown {position: static}"))
                                         ,splitLayout(
                                             selectInput(inputId = "yplot3"
                                                         ,label = "Y-axis:"
                                                         ,choices = column.names
                                                         ,selected = "KWH.JANUARY.2010")
                                             ,selectInput(inputId = "xplot3"
                                                          ,label = "X-axis:"
                                                          ,choices = column.names
                                                          ,selected = "COMMUNITY.AREA.NAME"
                                             )
                                             ,selectInput(inputId = "zplot3"
                                                          ,label = "Color by"
                                                          ,choices = column.names
                                                          ,selected = "BUILDING.TYPE")
                                         )
                                         , fluidRow(plotlyOutput("plot3"))
                               )
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
                      )
                  ,fluidRow(align = "center"
                            # We MUST load the ECharts javascript library in advance
                            ,loadEChartsLibrary()
                            ,tags$div(id="test", style="width:70%;height:500px;")
                            ,deliverChart(div_id = "test")
                            )
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
    
    Energy_subset <- reactive({
        req(input$selected_type) # ensure availablity of value before proceeding
        filter(CEnergy, BUILDING.TYPE  %in% input$selected_type)
    })
    
    # Create new df that is n_samp obs from selected type properties ------
    Energy_sample <- reactive({ 
        req(input$n_samp) # ensure availablity of value before proceeding
        sample_n(Energy_subset(), input$n_samp)
    })  
    
    # Update the maximum allowed n_samp for selected type movies ------
    observe({
        updateNumericInput(session,
                           inputId = "n_samp",
                           value = min(10, nrow(Energy_subset())),
                           max = nrow(Energy_subset())
        )
    })
    
    # Create scatterplot object the plotOutput function is expecting --
    output$plot1 <- renderPlotly({
        ggplotly(
        ggplot(data = CEnergy, aes_string(x = input$xplot1, y = input$yplot1, color = input$zplot1))
            #+geom_point( alpha = input$alpha) 
            #+labs(title = pretty_plot_title()
            )
    })
    
    output$plot2 <- renderPlotly({
        plot_ly(mtcars, x = ~mpg, y = ~wt)
    })
    
    output$plot3 <- renderPlotly({
        plot_ly(mtcars, x = ~mpg, y = ~wt)
    })
    
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
    renderGauge(div_id = "test",rate = 99, gauge_name = "Energy Usage Intensity", animation = T, show.tools = F)
    
    # Print data table if checked -------------------------------------
    output$energytable <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = CEnergy,
                          
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