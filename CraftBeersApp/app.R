library(shiny)

# Define UI ----
ui <- fluidPage(
  h1("U.S. Craft Beers Exploratory App"),
  h5("Author: Taylor Bonar"),
  
  sidebarLayout(
    # Creates side panel with Help Text and a File Input to accept only one CSV file
    sidebarPanel(
      helpText(
        strong("Inputting Data"),
        p("Use the following form to insert a CSV to input the data for Craft Beers to analyze.")),
      fileInput("beersInput", "Beers", multiple = F, accept = c('text/csv','text/comma-separated-values,text/plain','.csv'))),
    # Outputs
    mainPanel(
      dataTableOutput("beersDf")
      )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$beersDf <- renderDataTable(
    {
      inputFile <- input$beersInput
      if(is.null(inputFile)) return(NULL)
      df <- read.csv(inputFile$datapath)
    })
}

shinyApp(ui = ui, server = server)