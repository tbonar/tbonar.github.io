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
                   fileInput("beersInput", "Beers", multiple = F, accept = c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                 # Outputs contained in navigation bar
                 mainPanel(
                   dataTableOutput("beersDf")
                 )
               )
             )),
  
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