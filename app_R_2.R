library(leaflet)
library(shiny)
library(shinyjs)
library(readxl)
library(ggplot2)
library(readr)
library(shinythemes)
library(DT)
library(tidyr)
library(dplyr)

ui <- navbarPage("Interactive Map",
                 
## Map subpage ##
    
    tabPanel("Map", 
     div(class="outer", 
         
         tags$head(
           includeCSS("styles.css")
         ),
         
      fluidPage(
      leafletOutput('myMap', width = "100%", height="100vh"),
      useShinyjs(),
      theme = shinytheme("sandstone"),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
        
        fluidRow(
          fileInput("file1", "Choose CSV File", accept = ".csv"),
          checkboxInput("header", "Header", TRUE)
        ),
        
        radioButtons("disp", "Display",
                     choices = c("First 100 rows" = "100_rows",
                                 "First 1000 rows" = "1000_rows",
                                 "All" = "all"),
                     selected = "100_rows"),
        
        h5(strong("Additional information displayed on the map")),
        
        fluidRow( 
          column(6, selectInput("popup_1", label=NULL,
                                choices = NULL)), 
          column(6, selectInput("popup_2",label=NULL,
                      choices = NULL))
        ),
        
        p(strong("Plot")),
        
        fluidRow(
          column(6, selectInput("x", "X", choices = NULL)),
          column(6, selectInput("y", "Y", choices = NULL))
        ),
        
        fluidRow(
          tableOutput("output"),
          plotOutput("scatterPlot", width = "100%", height = "200px")
        ),
        
        actionButton("help_window", "HELP")
      )
    )
  )),
  

## Dataset subpage ##
  
  tabPanel("Dataset",
           fluidPage(mainPanel(width = 12,
                               DT::dataTableOutput("contents")))
           )

)

server <- function(input, output, session) {
  
  
  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header)
  })
  
  observe({
    req(data())
    updateSelectInput(session, "x", choices = colnames(data()))
    updateSelectInput(session, "y", choices = colnames(data()))
    updateSelectInput(session, "popup_1", choices = colnames(data()))
    updateSelectInput(session, "popup_2", choices = colnames(data()))
  })
  

  output$myMap <- renderLeaflet({
    data <- reactive({ 
      
      req(input$file1)
      
      data <- read.csv(input$file1$datapath, header =  input$header, sep = ",", dec = ".") %>% 
        mutate_at(c('LAT', 'LON'), as.numeric) %>% 
        drop_na()
      
      if(input$disp == "100_rows") {
        return(data[1:100,])
      }
      else if(input$disp == "1000_rows"){
        return(data[1:1000,])
      }
      else {
        return(data)
      }
      
      })
    

    map <- leaflet(data()) %>% 
      addTiles() %>%  
      addCircleMarkers(lat =  ~LAT, lng = ~LON, 
                       color = 'darkred',
                       radius = 5, 
                       popup = paste0(strong(paste0(input$popup_1,": " )), data()[[input$popup_1]],"<br>",
                                      strong(paste0(input$popup_2,": " )), data()[[input$popup_2]]),
                       stroke = FALSE, fillOpacity = 0.8
                       )
    map
  })
  
  output$scatterPlot <- renderPlot({
    req(input$file1,input$x,input$y)
    data <- data()
    colnames(data) <- gsub(";", "", colnames(data))
    
    ggplot(data, aes(x=data[, input$x], y=data[, input$y])) +
    geom_point(na.rm=TRUE) +
    labs(title=paste(input$x, "vs", input$y),
         x=input$x, y = input$y) +
    theme(plot.background = element_rect(fill='transparent', color=NA),
          text=element_text(face = "bold"))

  },bg="transparent")
  

  
  observeEvent(input$help_window, {
    shinyjs::runjs("var helpWindow = window.open('data:text/html,<html><body><h1>Hi, please remember that your dataset must contain these variables: ID, State, CrimeType and NumerOfCrimes.</h1><h2>Have fun :)</h2></body></html>', 'Help', 'dependent=TRUE,resizable=TRUE');helpWindow.document.title = 'Help';")
  })

  
## Interactive table showing same rows as on the map ##
  
  output$contents <- DT::renderDataTable(
    data <- data(),
    options=list(lengthMenu=list("10","50","100","1000","10000","ALL"),pageLength=50)
    
    #showDT::datatable(data)
    
  })
}

shinyApp(ui, server)
