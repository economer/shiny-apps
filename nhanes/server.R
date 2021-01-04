library(shiny)
shinyServer(function(input, output, session) {
    
    thedata <- reactive(download_nh(data_name = input$exp,year = input$year,name_to_label = input$label) %>%
                            clean_names()
    )
    
    output$str <- renderTable({
        if (thedata() ) { 
            thedata() %>%
                skimr::skim()
        } else {
            print("It does not exist for this year")
        }
    })
    
    output$table <- renderTable({
        thedata() %>%
            head(10)
        
    })
    
    
    output$down <- downloadHandler(
        filename = function(){
            
            paste("data_",Sys.Date(),".csv",sep = "")
            
        }, 
        content = function(fname){
            write.csv(thedata(), fname)
        }
    )
    
    
})
