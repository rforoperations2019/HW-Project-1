#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## app.R ##
library(shinydashboard)
library(plotly)
library(ggplot2)
library(DT)
library(tidyr)
library(ECharts2Shiny)


# Load Data for Shiny Visualization
doCoerce <- function(x, class) {
    if (canCoerce(x, class))
        as(x, class)
    else {
        result <- try(match.fun(paste("as", class, sep="."))(x), silent=TRUE);
        if (inherits(result, "try-error"))
            result <- match.fun(class)(x)
        result;
    }
}

expandClasses <- function (x) {
    unknowns <- character(0)
    result <- lapply(strsplit(as.character(x), NULL, fixed = TRUE),
                     function(y) {
                         sapply(y, function(z) switch(z,
                                                      i = "integer", n = "numeric",
                                                      l = "logical", c = "character", x = "complex",
                                                      r = "raw", f = "factor", D = "Date", P = "POSIXct",
                                                      t = "POSIXlt", N = NA_character_, {
                                                          unknowns <<- c(unknowns, z)
                                                          NA_character_
                                                      }), USE.NAMES = FALSE)
                     })
    if (length(unknowns)) {
        unknowns <- unique(unknowns)
        warning(sprintf(ngettext(length(unknowns), "code %s not recognized",
                                 "codes %s not recognized"), dqMsg(unknowns)))
    }
    result
}

# Clean data by removing incomplete reords
na.FindAndRemove <- function(mydata){
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




#CEnergy <- read.csv("Chicago_Energy.csv")
#CEnergy <- na.FindAndRemove(CEnergy)
# Convert columns to appropriate data formats dynamically with helper functions doCoerce and expandClasses
#data.frame(mapply(doCoerce, CEnergy, expandClasses("Dif")[[1L]], SIMPLIFY=FALSE), stringsAsFactors=FALSE)

#View(CEnergy)
#lapply(CEnergy, class)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Dynamic Filter Test App")
    ,sidebarLayout(
        sidebarPanel(
            uiOutput("Areaname")
            ,uiOutput("Censusblock")
            ,uiOutput("Buildingtype")
            ,uiOutput("Buildingsubtype")
            ,uiOutput("Avgstories")
            ,uiOutput("Avgbldgage")
            ,uiOutput("Avghousesize")
            ),
        mainPanel(
            div(style = 'overflow-x: scroll'
            ,dataTableOutput("table")
            )
            )
        )
    )

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    CEnergy <- read.csv("Chicago_Energy.csv")
    CEnergy <- na.FindAndRemove(CEnergy)
    
    output$Areaname <- renderUI({
        arealist <- sort(unique(as.vector(CEnergy$COMMUNITY.AREA.NAME)), decreasing = FALSE)
        arealist <- append(arealist, "All", after =  0)
        selectizeInput("Areaname", "Area Name:", arealist)
        })
    
    
    output$Censusblock <- renderUI({
        cblocklist <- sort(unique(as.vector(CEnergy$CENSUS.BLOCK)), decreasing = FALSE)
        cblocklist <- append(cblocklist, "All", 0)
        selectizeInput("Censusblock", "Census Block:", cblocklist)
        })
    
    output$Buildingtype <- renderUI({
        Buildingtype <- sort(unique(as.vector(CEnergy$BUILDING.TYPE)), decreasing = FALSE)
        Buildingtype <- append(Buildingtype, "All", 0)
        selectizeInput("Buildingtype", "Building Type:", Buildingtype)
    })
    
    output$Buildingsubtype <- renderUI({
        Buildingsubtype <- sort(unique(as.vector(CEnergy$BUILDING_SUBTYPE)), decreasing = FALSE)
        Buildingsubtype <- append(Buildingsubtype, "All", 0)
        selectizeInput("Buildingsubtype", "Building Sub-Type:", Buildingsubtype)
    })
    
    output$Avgstories <- renderUI({
        Avgstories <- sort(unique(as.vector(CEnergy$AVERAGE.STORIES)), decreasing = FALSE)
        Avgstories <- append(Avgstories, "All", 0)
        sliderInput("Avgstories"
                    ,"Average Stories:"
                    , min = min(CEnergy$AVERAGE.STORIES)
                    , max = max(CEnergy$AVERAGE.STORIES)
                    , value = c(1,3)
                    , step = 0.1 )
    })
    
    output$Avgbldgage <- renderUI({
        Avgbldgage <- sort(unique(as.vector(CEnergy$AVERAGE.BUILDING.AGE)), decreasing = FALSE)
        Avgbldgage <- append(Avgbldgage, "All", 0)
        sliderInput("Avgbldgage"
                    ,"Average Building Age:"
                    , min = min(CEnergy$AVERAGE.BUILDING.AGE)
                    , max = max(CEnergy$AVERAGE.BUILDING.AGE)
                    , value = c(5,10)
                    , step = 1 )
    })
    
    output$Avghousesize <- renderUI({
        Avghousesize <- sort(unique(as.vector(CEnergy$AVERAGE.HOUSESIZE)), decreasing = FALSE)
        Avghousesize <- append(Avghousesize, "All", 0)
        sliderInput("Avghousesize"
                    ,"Average House Size:"
                    , min = min(CEnergy$AVERAGE.HOUSESIZE)
                    , max = max(CEnergy$AVERAGE.HOUSESIZE)
                    , value = c(2,3)
                    , step = 1 )
    })
    
    data <- reactive({
        req(input$Areaname)
        req(input$Censusblock)
        req(input$Buildingtype)
        req(input$Buildingsubtype)
        req(input$Avgstories)
        req(input$Avgbldgage)
        req(input$Avghousesize)

        
        if(input$Areaname == "All") {
            filt1 <- quote(COMMUNITY.AREA.NAME != "@?><")
            } else {
                filt1 <- quote(COMMUNITY.AREA.NAME == input$Areaname) 
                }
         
        if (input$Censusblock == "All") {
            filt2 <- quote(CENSUS.BLOCK != "@?><")
            } else {
                filt2 <- quote(CENSUS.BLOCK == input$Censusblock)
            }
        
        if (input$Buildingtype == "All") {
            filt3 <- quote(BUILDING.TYPE != "@?><")
        } else {
            filt3 <- quote(BUILDING.TYPE == input$Buildingtype)
        }
        
        if (input$Buildingsubtype == "All") {
            filt4 <- quote(BUILDING_SUBTYPE != "@?><")
        } else {
            filt4 <- quote(BUILDING_SUBTYPE == input$Buildingsubtype)
        }
        
        if (input$Avgstories == "All") {
            filt5 <- quote(AVERAGE.STORIES != "@?><")
        } else {
            filt5 <- quote(AVERAGE.STORIES >= min(input$Avgstories) & AVERAGE.STORIES <= max(input$Avgstories))
        }
        
        if (input$Avgbldgage == "All") {
            filt6 <- quote(AVERAGE.BUILDING.AGE != "@?><")
        } else {
            filt6 <- quote(AVERAGE.BUILDING.AGE >= min(input$Avgbldgage) & AVERAGE.BUILDING.AGE <= max(input$Avgbldgage))
        }
        
        if (input$Avghousesize == "All") {
            filt7 <- quote(AVERAGE.HOUSESIZE != "@?><")
        } else {
            filt7 <- quote(AVERAGE.HOUSESIZE >= min(input$Avghousesize) & AVERAGE.HOUSESIZE <= max(input$Avghousesize))
        }
        
        CEnergy %>%
            filter_(filt1) %>%
            filter_(filt2) %>%
            filter_(filt3) %>%
            filter_(filt4) %>%
            filter_(filt5) %>%
            filter_(filt6) %>%
            filter_(filt7)
        })
    
    output$table <- renderDataTable({
        data()
        })
    })



# Run the application 
shinyApp(ui = ui, server = server)
