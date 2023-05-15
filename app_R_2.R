library(leaflet)
library(shiny)
library(shinyjs)
library(readxl)
library(ggplot2)
library(readr)

ui <- fluidPage(
  leafletOutput('myMap', width = "100%", height = "550px"),
  useShinyjs(),
  actionButton("help_window", "HELP"),
  div(
    style = "position: absolute; top: 5px; right: 40px;",
    width = "300px",
    fluidRow(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      checkboxInput("header", "Header", TRUE)
    ),
    fluidRow(
      selectInput("x", "X", choices = NULL),
      selectInput("y", "Y", choices = NULL)
    ),
    fluidRow(
      tableOutput("output"),
      plotOutput("scatterPlot", width = "100%", height = "200px")
    )
  )
)



server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=3000*1024^2)
  
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
      data.frame(Lat =as.numeric(data$LAT), Lon =as.numeric(data$LON))})
   
    
    map <- leaflet() %>% 
      addTiles() %>% 
      addCircles(data = data(), radius = 10, color = 'green') %>%
      setView(lng = -86.00, lat = 34.0000, zoom = 4)
    
    map
  })
  
  output$scatterPlot <- renderPlot({
    req(input$file1)
    data <- data()
    colnames(data) <- gsub(";", "", colnames(data))
    plot(x = data[, input$x], y = data[, input$y],
         xlab = input$x, ylab = input$y, main = paste(input$x, "vs", input$y))
  })
  

  
  observeEvent(input$help_window, {
    shinyjs::runjs("var helpWindow = window.open('data:text/html,<html><body><h1>Hi, please remember that your dataset must contain these variables: ID, State, CrimeType and NumerOfCrimes.</h1><h2>Have fun :)</h2></body></html>', 'Help', 'dependent=TRUE,resizable=TRUE');helpWindow.document.title = 'Help';")
  })

  output$contents <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    read.csv(file$datapath, header = input$header)
  })
}

shinyApp(ui, server)