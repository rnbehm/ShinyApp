
library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(ggplot2)


#to have an image in your app you must firt put it in a folder called www that you make


# get da data
bug <- read_csv("asof352019.csv") 
#wrangle the data
bugsimple<- bug %>% 
  select(order, year, country, stateProvince, county) %>%
  filter(order != "N/A") %>%
  filter(year >= 1919)

#wanted to check to make sure its numeric and yes it is
#bugsimple$year = as.numeric(bugsimple$year)
################################################################################################################################3

#create user interface
ui<- fluidPage(
  
  theme = shinytheme("cerulean"),
  titlePanel("Exploring the UCSB Invertebrate Zoology Collection"),
  navbarPage("UCSB IZC",
             #this tab only has text
             tabPanel("Summary",
                      h1("Purpose of this App"),
                      p("This app is designed to be a user-friendly way to explore the University of California, Invertebrate Zoology Collection. The data that this app uses is from the IZC database located at www.symbiota.ccber.ucsb.edu, which is a SCAN portal." ),
                      h1("History of the Invertebrate Zoology Collection"),
                      h4("Introduction"),
                      p("The University of California, Santa Barbara (UCSB) Natural History Museum at the Cheadle Center for Biodiversity and Ecological Restoration (CCBER) has formed an Invertebrate Zoology Collection from 10,000 specimens rediscovered in a basement on campus. Since its discovery, this collection hasgrown rapidly through coastal California arthropod survey efforts, donated student collections,and faculty research projects.These surveys, conducted by CCBER for conservation and restoration monitoring, are hugely valuable as the coastal regions of Santa Barbara and Ventura County are critically endangered habitats, withover 95% of these areas lost to human disturbance, and online records about insects from these areas is presently uncommon."),
                      p("The creation of this collection has inspired new interest in entomology on campus. Undergraduate students, graduate students, and staff are learning basic entomology in newly formed classes and workshops, students are using the collection as a reference, and the inclusion of arthropods in faculty research is on the rise. The collection is providing space to voucher invertebrate research from UC Santa Barbara, and it has additional specimens and online data from our survey traps for researchers interested in California invertebrates"),
                      h4("Background / Revitilazation Process"),
                      p("The Invertebrate Zoology Collection originated as a teaching collection from an entomology class taught by Dr. Adrian Wenner in the 1950's. The collection was expanded to include specimens collected by Dr. Wenner in the 1960's. After he retired during the 1980's, the collection was abandoned. In 2015, it was rediscovered and incorporated into the museum at the CCBER. The collection began to grow again from student-donated specimens from the presentday UCSB entomology and invertebrate zoology courses, accessions from the UCSB Natural Reserve System, and regional arthropod survey projects"),
                      p("The historic collection had out-of-date identifications, if any, and were coarsely organized by family. Thanks to funding from the Institute of Museum and Library Services (IMLS) and the UCSB Coastal Fund, new drawers and unit trays were purchased,  nomenclature was coarsely updated, and specimen determinations re-examined. Specimen are determined to the lowest rank possible with a focus on bees, ants, tiger beetles, and dune insects."),
                      p("Starting in April 2017, databasing the specimens began. Specimens were given a barcode that acts as a unique identifier for the specimen. Once imaged, the photo is colorcorrected, cropped, and renamed using a custom Gimp python plugin called BugFlipper, and bulk-uploaded into our Symbiota data portal."),
                      img(src = "ccber.png", height=200)
                      
             ),
###############################################################################################################################################             
             
             #histogram panel- like what you wanna do 
             #the histogram is made in the server code and called in this code
             tabPanel("Taxonomic Representation",
                      #added in images and adjusted height to reasonable size
                      img(src = "beetleshiny.png", height= 200), img(src = "dipforshiny.png", height=200), img(src = "hemipteraforshiny.png", height= 200), img(src = "hymenopteraforshiny.png", height=200), img(src = "lepidopteraforshiny.png", height=200),img(src = "odonataforshiny.png",height=200), img(src = "orthopforshiny.png", height=200), 
                      img(src= "whiteborder.png"),
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
                          plotOutput(
                            outputId ="bugplot")
                        )
                      )),

  ##################################################################################################################################################           
             #this is how you start a new tab
             tabPanel("Specimen Records Through Time",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          
                          sliderInput("histyear", "Choose Time Frame:",
                                      min = 1920, max = 2019,
                                      value = 100, step = 10,sep = "")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("hist")
                        )
                      ))



###########################################################################################################################################  

#tabPanel("Basic DataTable",
  
  # Create a new Row in the UI for selectInputs
 # fluidRow(
  #  column(4,
   #        selectInput("orderdata",
    #                   "Order:",
     #                  c("All",
      #                   unique(as.character(bugsimple$order))))
    #),
    #column(4,
     #      selectInput("statedata",
      #                 "State:",
       #                c("All",
        #                 unique(as.character(bugsimple$stateProvince))))
    #),
    #column(4,
     #      selectInput("yeardata",
      #                 "Year:",
       #                c("All",
        #                 unique(as.character(bugsimple$year))))
    #)
  #),
  # Create a new row for the table.
  #DT::dataTableOutput("table")
#)
  )
  
)

                        




server<- function(input, output) {
  
 
 
  # Define server logic required to draw a histogram
  output$bugplot<- renderPlot({
    
    # Creating the reactive output
    bugs_hist <- bugsimple %>% 
      filter(order == input$order) # Filter based on input selection from height widget
    
    #now create the graph
    ggplot(bugs_hist, 
           aes
           (x = year)) +
      geom_histogram(
        fill="lightskyblue")+
      theme_bw() +
      labs(x= "Year Collected", y= "Number of Specimens", title= "Specimen Collection Events in the Last 100 Years (1919-2019)") +
      scale_y_continuous(expand=c(0,0)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
    
    
    
    
    
    
  })
  #################################################################################################################################
  
  output$hist<- renderPlot({
  
  #ggplot(year_hist, aes(x = order)) +
  #geom_histogram(fill= "blue")+
  #theme_bw() + labs(x= "Year Collected", y= "Number of Specimens", title= "Specimen Collection Events in the Last 100 Years (1919-2019)") 
 
  timedata <- bugsimple %>%
    filter(year <= input$histyear)
  
  ggplot(mutate
         (timedata, 
           order = fct_infreq(order))) +
    geom_bar(aes
             (x= order, 
               fill = order), show.legend = FALSE)+
    theme_bw() + 
    labs(x= "Order", y= "Number of Specimens", title= "Specimen Collection Events Through Time")+
    scale_y_continuous(expand=c(0,0))+
    theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  })
  #################################################################################################################################
  
  
    #the code for the data table tab wasnt working, try again
 # function(input, output) {
    
    # Filter data based on selections
  #  output$table <- DT::renderDataTable(DT::datatable({
      
   #   data <- bugsimple
      
    #  if (input$orderdata != "All") {
       # data <- data[data$order== input$orderdata,]
     # }
      
      #if (input$statedata != "All") {
       # data <- data[data$stateProvince == input$statedata,]
      #}
      
      #if (input$yeardata != "All") {
       # data <- data[data$year == input$yeardata,]
      #}
      #data
    #}))
    
  #}
}



















# Run the application 
shinyApp(ui = ui, server = server)
