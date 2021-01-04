#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyApp(
    
    ui = fluidPage(
        selectInput("region", "Region TEST23:", 
                    choices = colnames(WorldPhones)),
        plotOutput("phonePlot", height=270)
    ),
    
    server = function(input, output) {
        output$phonePlot <- renderPlot({
            barplot(WorldPhones[,input$region]*1000, 
                    ylab = "Number of Telephones TEST2", xlab = "Year")
        })
    },
    
    options = list(height = 345)
)