library(shiny)

fluidPage(
  h3("Gapfilling regression model"),
  sidebarLayout(
    sidebarPanel(
      
    ), mainPanel(
      textOutput("sumtab"),
      textOutput("sumplot")
    )
  )
)