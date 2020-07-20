library(shiny)
library(tidyverse)

ui <- fluidPage(
  sidebarPanel(
    fileInput("rawdata", "Choose CSV File", multiple = T, 
              accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    radioButtons("sep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
  ),
  mainPanel(
    tableOutput("contents")
  ),
)

server <- function(input, output){
  output$contents <- renderTable({
    req(input$rawdata)
    df <- read.csv(input$rawdata$datapath)
    df <- as_tibble(df)
    head(df)
  })
}
shinyApp(ui = ui, server = server)