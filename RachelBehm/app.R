library(shiny)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)

# get da data
bug <- read_csv("asof252019.csv") 
#wrangle the data
bugsimple<- bug %>% 
  select(order, year, country, stateProvince, county) %>%
  filter(order != "N/A") %>%
  filter(year >= 1919)

#now lets try to make one for each order so can call them seperately for the graphs
Diptera <- bugsimple %>%
  filter(order == "Diptera")
Coleoptera<- bugsimple %>%
  filter(order == "Coleoptera")
Hemiptera<- bugsimple %>%
  filter(order == "Hemiptera")
Hymenoptera <-bugsimple %>%
  filter(order == "Hymenoptera")
Lepidoptera<- bugsimple %>%
  filter(order == "Lepidoptera")
Odonata <- bugsimple %>%
  filter(order == "Odonata")
Orthoptera <- bugsimple %>%
  filter(order == "Orthoptera")
Trichoptera<- bugsimple %>%
  filter(order == "Trichoptera")






#create user interface
ui<- fluidPage(
  
  theme = shinytheme("cerulean"),
  titlePanel("Exploring the UCSB Invertebrate Zoology Collection"),
  navbarPage("UCSB IZC",
             #this tab only has text
             tabPanel("Summary of the App",
                      h1("The purpose of this App"),
                      h2("Uses of this app"),
                      p("paragraph 1"),
                      p("paragraph 2"),
                      h1("Then another header"),
                      p("paragraph3")
                      
             ),
             #histogram panel- like what you wanna do 
             #the histogram is made in the server code and called in this code
             tabPanel("Exploration",
                      
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
                                "Orthoptera",
                                "Trichoptera"
                              ))
               ),
               mainPanel(
                 plotOutput(outputId ="bugplot")
               )
             ))))
             
             
  
  
  server<- function(input, output) {
    
    output$bugplot<- renderPlot({
      
      ggplot(data = input$radioButtons, aes(x = year)) +
        geom_histogram(aes(fill=order))+
        theme_bw() + labs(x= "Year Collected", y= "Number of Specimens", title= "Specimen Aquisitions in the Last 100 Years (1919-2019")
    })
    
  }
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 



# Run the application 
shinyApp(ui = ui, server = server)

