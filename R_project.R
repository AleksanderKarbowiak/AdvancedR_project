library(shiny)
library(shinyjs)
library(readxl)
library(ggplot2)

ui <- fluidPage( 
  useShinyjs(), 
  actionButton("help_window", "HELP"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose XLSX File", accept = ".xlsx"),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$help_window, {
    shinyjs::runjs("var helpWindow = window.open('data:text/html,<html><body><h1>Hi, please remember that your dataset must contain these variables: ID, State, CrimeType and NumerOfCrimes.</h1><h2>Have fun :)</h2></body></html>', 'Help', 'dependent=TRUE,resizable=TRUE');helpWindow.document.title = 'Help';")
  })
  
  output$contents <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    
    read_excel(file$datapath, col_names = input$header)
  })
}

shinyApp(ui, server)