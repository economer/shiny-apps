#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


shinyUI(fluidPage(
    fluidRow(
        column(
            9, 
            "Explore the datasets",
            shiny::selectInput(inputId = "exp",label = "You can find the names of the datasets available and choose the dataset(s) of your choice for downloding",choices =file_list3_exp$file_name,multiple = T,selectize = T)
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
    mainPanel(
        fluidRow(
            column(
                12,
                "An overview of the dataset(s)",
                tableOutput(outputId = "str")
            ),
            
            "The first 10 observations",
            column(12,
                   "Table", 
                   tableOutput(outputId = "table"),
                   downloadButton(outputId = "down",label = "Would You Like to Download the Results?")
                   
            )
        )
    )
    
    
))