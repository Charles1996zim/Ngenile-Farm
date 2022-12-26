library(shiny)
library(shinythemes)
library(googlesheets4)
# Define UI -----------
# ---------------------

ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # header
                headerPanel("My Shiny Data entry app"),
                
                sidebarLayout(
                    # sidebar for form
                    sidebarPanel(
                        h3("Information",""),
                        numericInput("Number", "Cow Number",""),
                        numericInput("Production", "Production",""),
                        radioButtons("status", "Cow Staus",
                                     c("Fine",
                                       "C",
                                       "M",
                                       "Dead")),
                        radioButtons("time", "Morning/Afternoon",
                                     c("Morning",
                                       "Afternoon")),
                        actionButton("update", "Next"),
                        
                        textOutput("id")
                    )
                    ,
                    
                    # output for viewing
                    mainPanel(
                        
                        DT::dataTableOutput("tableDT"),
                        
                        
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
                       Date = Sys.Date(),
                       Age_weeks = input$time, 
                       Production = input$Production, 
                       Status = input$status,
                       stringsAsFactors = FALSE)
        
        return(aniRoi2)
        
        
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
