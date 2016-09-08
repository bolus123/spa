library(shiny)

shinyServer(function(input, output) {

  arl0 <- reactive({
    L <- as.numeric(input$l)
    1/(2-(2*pnorm(L)))
  })
  
  tb <- reactive({
    data.frame(
      Metric = c("ARL0"),
      Value = as.numeric(c(arl0())),
      stringsAsFactors = FALSE
    )
  })
  
  output$sum <- renderTable({
    tb()
  })
  
  output$plot <- renderPlot({

  })
  
})