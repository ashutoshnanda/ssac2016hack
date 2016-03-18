library(shiny)

shinyServer(function(input, output) {
  output$play <- renderImage({
    list(src = input$which, alt = "Play")
  }, deleteFile = F)
})

