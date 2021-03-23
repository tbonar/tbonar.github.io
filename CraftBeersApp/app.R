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
   tabPanel(
     "Input Data",
     sidebarLayout(
       # Creates side panel with Help Text and a File Input to accept only one CSV file
       sidebarPanel(
         helpText(p("Use the following form to upload a CSV to input the data for app to analyze.")),
         fileInput("beersInput", "Beers", multiple = F, accept = c('text/csv','text/comma-separated-values,text/plain','.csv')),
         checkboxInput("useDefaultBeers","Use a provided Beers.csv?", value = F),
         strong("Note: The beers dataset will be joined with a brewery dataset using a shared Brewery_id and the joined dataframe will be displayed on this tabs")
         ),
       # Outputs contained in navigation bar
       mainPanel(
         dataTableOutput("beersDf")
       )
     )
   )),
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  breweriesDf <- read.csv("data/Breweries.csv")
  breweriesDf$State <- str_replace_all(breweriesDf$State, "(\\s+)", "")
  
  output$beersDf <- renderDataTable(
    {
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
      
      beerAndBreweries = merge(beers, breweriesDf, by.x="Brewery_id", by.y="Brew_ID")
      colnames(beerAndBreweries)[2] = "Beer_Name"
      colnames(beerAndBreweries)[8] = "Brewery_Name"
      
      return(beerAndBreweries)
    })
}

shinyApp(ui = ui, server = server)