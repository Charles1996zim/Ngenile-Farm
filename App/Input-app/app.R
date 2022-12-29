library(shiny)
library(shinythemes)
library(googlesheets4)
library(tidyverse)
library(dplyr)
library(shinyalert)
# Define UI -----------
# ---------------------

gs4_auth(email = "cmr.randell@gmail.com", cache = ".secret")

cows <- googlesheets4::read_sheet(ss = "1q0eE1j1fr7pn7haLnU5DT4ZRjDB6g_vUCapYi8zTez4", range = "Sheet3")

ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # header
                headerPanel("My Shiny Data entry app"),
                
                sidebarLayout(
                  # sidebar for form
                  sidebarPanel(
                    h3("Information",""),
                    numericInput("Number", "Cow Number",""),
                    numericInput("Production", "Production",""),
                    radioButtons("time", "Morning/Afternoon",
                                 c("Morning",
                                   "Afternoon")),
                    dateInput("date1", "Date:"),
                    radioButtons("status", "Cow Status",
                                 c("Fine",
                                   "C",
                                   "M",
                                   "Dead")),
                    actionButton("update", "Next"),
                    
                    textOutput("id")
                  )
                  ,
                  
                  # output for viewing
                  mainPanel(
                    
                    fluidPage(
                      DT::dataTableOutput("tableDT"),
                      actionButton("do", "Click Me")
                    ),
                    
                    
                  )   
                )
)

# Define server logic ------
# --------------------------

server <- function(input, output) {
  
  # process the textinput
  table_1 <- reactive({  
    
    
    validate(
      need(is.numeric(input$Number), "Please input a number")
    )
    
    # creating table
    
    aniRoi2 <- 
      
      data.frame(Animal_ID = input$Number,
                 Date = input$date1,
                 Age_weeks = input$time, 
                 Production = input$Production, 
                 Status = input$status,
                 stringsAsFactors = FALSE) %>%
      mutate(total = Production*7)
    
    return(aniRoi2)
    
    
  })
  
  
  
  observeEvent(input$update, {
    
    if(any(cows)!=input$Number){
    # Show a modal when the button is pressed
    shinyalert("Oops!", "Something went wrong.",  showCancelButton = TRUE)}
  })
  
  # process the text file and download
  
  observeEvent(input$update,{
    
    googlesheets4::sheet_append(table_1(), ss = "1q0eE1j1fr7pn7haLnU5DT4ZRjDB6g_vUCapYi8zTez4")
    
  })    
  
  
  
  # merge two function as data.frame
  mytable2 <-eventReactive(input$update,{
    
    googlesheets4::read_sheet(ss = "1q0eE1j1fr7pn7haLnU5DT4ZRjDB6g_vUCapYi8zTez4", range = "Sheet2") %>% tail(10)
    
    #cbind.data.frame(table_1(), mytable2())
    
  })
  
  
  # output as data table      
  output$tableDT <- DT::renderDataTable(
    mytable2()
  )
  
  
}

# Run the app ----------
# ----------------------
shinyApp(ui, server)
