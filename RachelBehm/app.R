library(shiny)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)

# get da data
oph <- read_csv("AllOphioninaeEntries.csv") 

oph$genus[is.na(oph$genus)] <- "Not Specified"

#create user interface
ui<- fluidPage(
  
  theme = shinytheme("slate"),
  titlePanel("Coastal Southern California Counties"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("county",
                   "Choose a county",
                   c("San Diego",
                     "Orange",
                     "Los Angeles",
                     "Riverside",
                     "San Bernardino",
                     "Ventura",
                     "Santa Barbara"
                     ))
    ),
    mainPanel(
      plotOutput(outputId ="ophplot")
    )
  )
  
  
)

server<- function(input, output) {
  
  output$ophplot<- renderPlot({
    
    ggplot(filter(oph, county== input$county), aes(x=year))+
      geom_bar(aes(fill=genus), position= "fill")+
      theme_dark()
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)

