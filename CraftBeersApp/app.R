source("helpers.R")
# Load APIs
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)
library(stringr)
library(tidyr)
library(class)
library(caret)
library(e1071)
library(shiny)

# Define UI ----
ui <- fluidPage(
  navbarPage("U.S. Craft Beers Exploratory App",
   # Input Data Tab
   tabPanel(
     "Input Data",
     sidebarLayout(
       # Creates side panel with Help Text and a File Input to accept only one CSV file
       sidebarPanel(
         helpText("Use the following form to upload a CSV to input the data for app to analyze."),
         fileInput("beersInput", "Beers", multiple = F, accept = c('text/csv','text/comma-separated-values,text/plain','.csv')),
         checkboxInput("useDefaultBeers","Use a provided Beers.csv?", value = F),
         strong("Note: The beers dataset will be joined with a brewery dataset using a shared Brewery_id and the joined dataframe will be displayed on this tabs")
         ),
       # Outputs contained in navigation bar
       mainPanel(
         dataTableOutput("beersDf")
       )
     )
   ),
   # Medians by State for IBU or ABV tab 
   tabPanel(
     "Inspecting Medians by State",
     sidebarLayout(
       sidebarPanel(
         "Options",
         helpText("The following module aims at exploring each state's median alcohol content by volume (ABV) and it's international bitterness units (IBU). Use the options below to change the graphs"),
         radioButtons("abvIbuMediansChoice","Dependent Variable:",
                      c("None Selected" = "","Alcohol By Volume" = "ABV", "International Bitterness Units" = "IBU"))
       ),
       mainPanel(
         plotOutput("stateMedians")
       )
     )
   )
  ),
)
# Define server logic
server <- function(input, output) {
  breweriesDf <- read.csv("data/Breweries.csv")
  breweriesDf$State <- str_replace_all(breweriesDf$State, "(\\s+)", "")
  
  # Joined Datasets Dataframe Output
  output$beersDf <- renderDataTable(
    {
      df <- NULL
      call.me = beersAndBreweries()
    })
  
    beersAndBreweries <- reactive({
      inputFile <- input$beersInput
      # Don't store anything if input file is null and useDefaultBeers is false
      if(is.null(inputFile) & !input$useDefaultBeers) return(NULL)
      # Use app defaults if a user does not have a Beers dataset to use and a file was not provided
      else if(input$useDefaultBeers & is.null(inputFile)) {
        beers = read.csv("data/Beers.csv")
      }
      # Lastly if a file is provided, read and transform to a dataframe
      else {
      beers <- read.csv(inputFile$datapath)
      }
      # Perform merge by Brewer_id and Brew_ID
      beerAndBreweries = merge(beers, breweriesDf, by.x="Brewery_id", by.y="Brew_ID")
      colnames(beerAndBreweries)[2] = "Beer_Name"
      colnames(beerAndBreweries)[8] = "Brewery_Name"
      
      # Return the joined dataset
      return(beerAndBreweries)
    }
  )
  
  # Medians By State Outputs
  output$stateMedians <- renderPlot(
    {
      # Check if option is ABV
      if(str_detect(input$abvIbuMediansChoice,"ABV")) {
        mergedDf <- beersAndBreweries() %>% drop_na(IBU, ABV)
        medianABVbyState = aggregate(mergedDf$ABV,
                                     list(mergedDf$State),
                                     median)
        colnames(medianABVbyState) = c("State", "ABV")
        ggplot(medianABVbyState, aes(x=State,y=ABV)) + 
          geom_col(fill = "#f28e1c") +
          labs(title = "Median Alcohol by Volume (ABV) for Each State") +
          theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))
      } # Else check for IBU choice
      else if (str_detect(input$abvIbuMediansChoice,"IBU")) { 
        mergedDf <- beersAndBreweries() %>% drop_na(IBU, ABV)
        medianIBUbyState = aggregate(mergedDf$IBU,
                                     list(mergedDf$State),
                                     median)
        colnames(medianIBUbyState) = c("State", "IBU")
        ggplot(medianIBUbyState, aes(x=State,y=IBU)) + 
          geom_col(fill = "#f28e1c") + 
          labs(title = "Median International Bitterness Units (IBU) for Each State") +
          theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))
      } # Lastly do nada 
      else return(NULL)
    }
  )
}

shinyApp(ui = ui, server = server)