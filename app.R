library(dplyr)
library(stringr)
library(readr)

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

ui <- fluidPage(
    fluidRow(
        column(
            9, 
            "Explore the datasets",
            shiny::selectInput(inputId = "exp",label = "You can find the names of the datasets available and choose the dataset(s) of your choice for downloding",choices =file_list3_exp$file_name,selected = "blood pressure", multiple = T,selectize = T)
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
    download_nh <- function(data_name=NULL,year=NULL,name_to_label=FALSE) {
        
        library(haven)
        library(dplyr)
        library(purrr)
        library(janitor)
        library(readr)
        library(glue)
        library(rvest)
        library(stringr)
        library(data.table)
        
        file_list3 <- read_csv("https://raw.githubusercontent.com/economer/NHANES/master/file_list3.csv",progress = F) %>%
            mutate(file_name = tolower(file_name)) %>%
            as.data.frame() %>%
            mutate(file_name = str_replace_all(string = file_name,pattern = " & ", " and "))
        
        file_list_year <- file_list3 %>%
            distinct(Year) %>%
            as.data.frame()
        
        name_list3 <- read_csv("https://raw.githubusercontent.com/economer/NHANES/master/name_list3.csv",progress = F) %>%
            as.data.frame()
        
        ## function to download xpt files 
        import_xpt <- function(links) {
            df <- haven::read_xpt(links)
        }
        
        # use str_detect to filter the files required
        selected_data <-  file_list3 %>%
            filter(file_name %in% {{data_name}},
                   Year %in% {{year}}
            ) 
        # download the data selected 
        selected_data_map <- selected_data %>%
            mutate(data_files = purrr::map(download_links,import_xpt))
        
        ## select year and data files
        selected_data_map1 <- selected_data_map %>%
            select(Year,file_name, data_files) 
        
        ## convert the nested data frames to a list
        ## 
        selected_data_map_list <- selected_data_map1 %>%
            purrr::map(~list(.))
        
        ## keep the datasets' lists 
        selected_data_map_list <- selected_data_map_list[["data_files"]][[1]]
        
        ## Join the datasets by SEQN
        
        final_dataset <- plyr::join_all(dfs = selected_data_map_list,by = "SEQN",type = "full")
        
        if(name_to_label == TRUE) {
            
            name_list3 <- name_list3 %>%
                distinct(var_name,.keep_all = T)
            setnames(x = final_dataset,old = as.character(name_list3$var_name),new =
                         as.character(name_list3$var_label),skip_absent = T)
            final_dataset <- final_dataset %>%
                as_tibble(.name_repair = "minimal")
            
            final_dataset
            
        } else {
            final_dataset
        }
        
    } 
    
    library(dplyr)
    
    file_list3_exp <- file_list3 %>%
        mutate(file_name = stringr::str_replace_all(string = file_name,pattern = " & ", " and ")) %>%
        group_by(name,file_name) %>%
        count() %>%
        select(-n) %>%
        arrange(file_name)
    
    
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

