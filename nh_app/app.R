library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(shinythemes)
# sudo su - -c "R -e \"install.packages('shinydashboard', repos='http://cran.rstudio.com/')\""

file_list3 <- read_csv("https://raw.githubusercontent.com/economer/NHANES/master/file_list3.csv",progress = F) %>%
    mutate(file_name = tolower(file_name)) %>%
    as.data.frame() %>%
    mutate(file_name = str_replace_all(string = file_name,pattern = " & ", " and "))

file_list_year <- file_list3 %>%
    distinct(Year) %>%
    as.data.frame()

name_list3 <- read_csv("https://raw.githubusercontent.com/economer/NHANES/master/name_list3.csv",progress = F) %>%
    as.data.frame()


file_list3_exp <- file_list3 %>%
    mutate(file_name = stringr::str_replace_all(string = file_name,pattern = " & ", " and ")) %>%
    group_by(name,file_name) %>%
    count() %>%
    select(-n) %>%
    arrange(file_name)


ui <- fluidPage(theme = shinytheme("cyborg"),
                titlePanel("National Health and Nutrition Examination Survey (NHANES) Downloader"),
                fluidRow(
                    
                    h3("Explore the datasets\n"),
                    column(
                        9, 
                        shiny::selectInput(inputId = "exp",label = "You can find the names of the datasets available and choose the dataset(s) of your choice for downloding",choices =file_list3_exp$file_name, multiple = T,selectize = T,selected = "blood pressure")
                    ),

                    column(9,
                    h3("Select the Year(s)"),
                           
                           shiny::selectInput(inputId = "year",label = "",choices = file_list_year$Year,multiple = T,selectize = T,selected = "1999-2000")
                    ), 
                    column(
                        9,
                        h5("Variables Names to Labels"),
                        shiny::checkboxInput(inputId = "label",label = "Change Variables Names to Variables Labels",value = TRUE)
                    )
                    
                ), 
                mainPanel(width = 12,
                          fluidRow(
                              
                              h3("An overview of the varaibles"),
                              column(
                                  12,
                                  tableOutput(outputId = "str"),
                                  h3("The first 10 observations")
                              ),
                              column(12,
                                     tableOutput(outputId = "table"),
                                     downloadButton(outputId = "down",label = "Would You Like to\n Download the Results?")
                                     
                              )
                          )
                )
                
)

server <- function(input, output, session) {
    source(file = "download_nh.R")
    
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

