library(shiny)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)

# get da data
bug <- read_csv("asof252019.csv") 
#wrangle the data
bugsimple<- bug %>% 
  select(order, year, country, stateProvince, county)


#create user interface
ui<- fluidPage(
  
  theme = shinytheme("slate"),
  titlePanel("Exploring the UCSB Invertebrate Zoology Collection"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("order",
                   "Choose an order",
                   c("Coleoptera",
                     "Diptera",
                     "Hemiptera",
                     "Hymenoptera",
                     "Lepidoptera",
                     "Odonata",
                     "Orthoptera"
                     ))
    ),
    mainPanel(
      plotOutput(outputId ="bugplot")
    )
  )
  
  
)

server<- function(input, output) {
  
  output$bugplot<- renderPlot({
    
    ggplot(bugsimple, aes(x=year)) +
      geom_bar(aes(fill=order), position= "fill")+
      theme_dark() + scale_x_continuous()
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)

