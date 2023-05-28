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
library(tidyverse)
library(iskanalytics)
library(Rcpp)
sourceCpp("Codes_functions/countNaValuesRcpp.cpp")

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
                 ),
                 
                 ## Subpage with more details ##
                 
                 tabPanel("Details",
                          fluidPage(
                            
                            mainPanel(width=8,
                                      fluidRow(tableOutput('table_summ'),
                                               plotOutput('plot'))),
                            
                            sidebarPanel(width = 4,
                                         
                                         fluidRow(
                                           
                                           h5(strong("Variables:")),
                                           fluidRow(
                                             column(6,selectInput("numeric_var", label=h6("Numeric:"), choices = NULL)),
                                             column(6,selectInput("categorical_var", label=h6("Categorical:"), choices = NULL))),
                                           
                                           h3("Table"),
                                           radioButtons("table_type", label = "Table Type", 
                                                        choices = list("Basic Statistics" = "summary_table", 
                                                                       "Number of unique values of each column" = "unique_values_table",
                                                                       "Levels and Frequency of Categorical Values" = "lvl_freq",
                                                                       "Number of NULLs" = "na_count"),
                                                        
                                                        selected = "summary_table"),
                                           
                                           actionButton("create_table", label = "Create Table"),
                                           
                                           h3("Plot"),
                                           radioButtons("plot_types", label = "Plot Type", 
                                                        choices = list("Box Plot" = "boxplot", "Histogram for numeric variable" = "histogram", "Bar Plot for categorical variable"="barplot", "Dot Plot" = "dot_plot"),
                                                        selected = "histogram"),
                                           
                                           actionButton("create_plot", label = "Create Plot")
                                           
                                         ))
                          )
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
    updateSelectInput(session, "numeric_var", choices = variablesNames(data(),'num'))
    updateSelectInput(session, "categorical_var", choices = variablesNames(data(),'char'))
  })
  
  
  output$myMap <- renderLeaflet({
    data <- reactive({ 
      
      req(input$file1)
      
      data<-read.csv(input$file1$datapath, header =  input$header) %>% drop_na(last_col())
      colnames(data) <- gsub(";", "", colnames(data))
      data$LAT <- as.numeric(gsub("[^0-9.-]", "", data$LAT))
      data$LON <- as.numeric(gsub("[^0-9.-]", "", data$LON))
      
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
    data <- data() %>% drop_na(last_col()),
    options=list(lengthMenu=list("10","50","100","1000"),pageLength=50)
    
    #showDT::datatable(data)
    
  )
  
  ## Table with statistics 
  
  observeEvent(input$create_table,{
    output$table_summ <- renderTable({
      req(input$numeric_var,input$categorical_var)
      df_to_cleanNull <- data() %>% drop_na(last_col())
      df_input <- data.frame(df_to_cleanNull[[input$numeric_var]],df_to_cleanNull[[input$categorical_var]]) 
      colnames(df_input) <- c("numeric_var", "categorical_var")
      
      if("summary_table" %in% input$table_type){
        df_input %>% group_by(categorical_var) %>%
          summarise(
            !!paste0("Mean ", input$numeric_var) := mean(numeric_var),
            !!paste0("Median ", input$numeric_var) := median(numeric_var),
            !!paste0("Mode ", input$numeric_var) := getmode(numeric_var),
            !!paste0("Count Unique ", input$numeric_var) := n_distinct(numeric_var)
          ) %>%
          rename(!!input$categorical_var := "categorical_var")
        }
      else if ("unique_values_table" %in% input$table_type) {
        getUniqueNumValues(df_to_cleanNull)
      }
      else if ("lvl_freq" %in% input$table_type) {
        getVarLevels(df_to_cleanNull,input$categorical_var)
      }
      else if ("na_count" %in% input$table_type) {
        rbind.data.frame(
          countNaValuesRcpp(df_input$numeric_var,input$numeric_var),
          countNaValuesRcpp(df_input$numeric_var,input$categorical_var)
          )
      }
      
    })
  })
  
  observeEvent(input$create_plot,{
    output$plot <- renderPlot({
      req(input$numeric_var,input$categorical_var)
      df_to_cleanNull <- data() %>% drop_na(last_col())
      df_input <- data.frame(df_to_cleanNull[[input$numeric_var]],df_to_cleanNull[[input$categorical_var]])
      colnames(df_input) <- c("numeric_var", "categorical_var")
      df_to_cleanNull <- factorCatVars(df_to_cleanNull)
      if("histogram" %in% input$plot_types){
        hist(df_to_cleanNull[[input$numeric_var]], labels=TRUE, xlab=input$numeric_var, main=paste0("Histogram of ",input$numeric_var))
      }
      else if("barplot" %in% input$plot_types){
        barplot(table(df_to_cleanNull[[input$categorical_var]]), main=paste0("Histogram of ",input$categorical_var), xlab=input$categorical_var, ylab="Frequency")}
      else if("boxplot" %in% input$plot_types){
       bp_formula = paste0(input$numeric_var,'~',input$categorical_var)
       boxplot(as.formula(bp_formula),data=df_to_cleanNull, notch=TRUE, ylab=input$numeric_var, main=paste0("Boxplot for",input$numeric_var," x ",input$categorical_var)) }
      else if("dot_plot" %in% input$plot_types){
        dotchart(x=df_to_cleanNull[[input$numeric_var]], groups=df_to_cleanNull[[input$categorical_var]], main=paste0(input$numeric_var,' for ',input$categorical_var),xlab=input$numeric_var,ylab=input$categorical_var)}
      
    })
  })
  
  
}

shinyApp(ui, server)