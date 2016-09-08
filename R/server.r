library(shiny)

shinyServer(function(input, output) {

  arl0 <- reactive({
    L <- as.numeric(input$l)
    1/(2-(2*pnorm(L)))
  })
  
  q10 <- reactive({
    L <- as.numeric(input$l)
    qgeom(0.1, 2-(2*pnorm(L)))
  })
  
  cdfrl0 <- reactive({
    L <- as.numeric(input$l)
    t <- 370
    pgeom(t,2-(2*pnorm(L)))
  })
  
  tb <- reactive({
    data.frame(
      Metric = c("ARL0", "Percentile: 10%", "CDFRL0"),
      Value = as.numeric(c(arl0(), q10(), cdfrl0())),
      stringsAsFactors = FALSE
    )
  })
  
  output$sum <- renderTable({
    tb()
  })
  
  output$plot <- renderPlot({

  })
  
})