library(shiny)

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "my.css")
  ),
  titlePanel("Visualizing the Spread of the Defense"),
  sidebarLayout(
    sidebarPanel(
      selectInput("which",
                  "Pick a play",
                  plays)
    ),
    mainPanel(
     imageOutput("play")
    )
  )
))

