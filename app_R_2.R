library(leaflet)
library(shiny)
library(shinyjs)
library(readxl)
library(ggplot2)
library(readr)
library(shinythemes)

ui <- fluidPage(
  leafletOutput('myMap', width = "100%", height="100vh"),
  useShinyjs(),
  theme = shinytheme("sandstone"),
  div(
    style = "position: absolute; top: 5px; right: 40px;",
    width = "300px",
    
    fluidRow(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      checkboxInput("header", "Header", TRUE)
    ),
    
    radioButtons("disp", "Display",
                 choices = c("First 100 rows" = "100_rows",
                             "First 1000 rows" = "1000_rows",
                             "All" = "all"),
                 selected = "100_rows"),
    
    fluidRow(
      selectInput("x", "X", choices = NULL),
      selectInput("y", "Y", choices = NULL)
    ),
    
    fluidRow(
      tableOutput("output"),
      plotOutput("scatterPlot", width = "100%", height = "200px")
    ),
    
    actionButton("help_window", "HELP")
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
  })
  

  output$myMap <- renderLeaflet({
    data <- reactive({ 
      req(input$file1)
      data<-read.csv(input$file1$datapath, header =  input$header)
      colnames(data) <- gsub(";", "", colnames(data))
      data$LAT <- as.numeric(gsub("[^0-9.-]", "", data$LAT))
      data$LON <- as.numeric(gsub("[^0-9.-]", "", data$LON))
      data <- na.omit(data[c("LAT", "LON")])
      data.frame(Lat =as.numeric(data$LAT), Lon =as.numeric(data$LON))
      
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
    
    
    map <- leaflet() %>% 
      addTiles() %>% 
      addCircles(data = data(), radius = 10, color = 'darkred') %>%
      setView(lng = -86.00, lat = 34.0000, zoom = 4)
    
    map
  })
  
  output$scatterPlot <- renderPlot({
    req(input$file1)
    data <- data()
    colnames(data) <- gsub(";", "", colnames(data))
    
    ggplot(data, aes(x=data[, input$x], y=data[, input$y])) +
    geom_point() +
    labs(title=paste(input$x, "vs", input$y),
         x=input$x, y = input$y) +
    theme(plot.background = element_rect(fill='transparent', color=NA),
          text=element_text(face = "bold"))

  },bg="transparent")
  

  
  observeEvent(input$help_window, {
    shinyjs::runjs("var helpWindow = window.open('data:text/html,<html><body><h1>Hi, please remember that your dataset must contain these variables: ID, State, CrimeType and NumerOfCrimes.</h1><h2>Have fun :)</h2></body></html>', 'Help', 'dependent=TRUE,resizable=TRUE');helpWindow.document.title = 'Help';")
  })

  output$contents <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    read.csv(file$datapath, header = input$header)
    
    if(input$disp == "100_rows") {
      return(file[1:100,])
    }
    else if(input$disp == "1000_rows"){
      return(file[1:1000,])
    }
    else {
      return(file)
    }
  })
}

shinyApp(ui, server)