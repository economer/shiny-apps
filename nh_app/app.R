library(shiny)
library(dplyr)
library(stringr)
library(readr)


ui <- fluidPage(
    fluidRow(
        column(
            9, 
            "Explore the datasets",
            shiny::selectInput(inputId = "exp",label = "You can find the names of the datasets available and choose the dataset(s) of your choice for downloding",choices =file_list3_exp$file_name, multiple = T,selectize = T)
        ),
        
        column(4,
               "Year",
               shiny::selectInput(inputId = "year",label = "Select Year(s)",choices = file_list_year$Year,multiple = T,selectize = T,selected = "1999-2000")
        ), 
        column(
            4,
            "Variables Names to Labels?",
            shiny::checkboxInput(inputId = "label",label = "Variables Names to Labels?",value = TRUE)
        )
        
    ), 
    mainPanel(width = 12,
              fluidRow(
                  column(
                      12,
                      "An overview of the dataset(s)\n\n",
                      tableOutput(outputId = "str")
                  ),
                  
                  column(12,
                         "The first 10 observations",
                         tableOutput(outputId = "table"),
                         downloadButton(outputId = "down",label = "Would You Like to Download the Results?")
                         
                  )
              )
    )
    
)

server <- function(input, output, session) {
    
    
    library(dplyr)
    library(skimr)
    library(purrr)
    
    thedata <- reactive(as.data.frame(download_nh(data_name = input$exp,year = input$year,name_to_label = input$label)))
    
    output$str <- renderTable({
        if (!is.na(thedata())) { 
            skimr::skim(data = thedata()) 
        } else {
            print("It does not exist for this year")
        }
    })
    
    
    output$table <- renderTable({
        head(x = thedata() ,10)
        
        
    })
    
    
    output$down <- downloadHandler(
        filename = function(){
            
            paste("data_",Sys.Date(),".csv",sep = "")
            
        }, 
        content = function(fname){
            write.csv(thedata(), fname)
        }
    )
    
    
}


shinyApp(ui = ui,server =  server)

