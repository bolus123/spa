library(shiny)

shinyServer(function(input, output) {

  cc <- reactive({
    input$cc
  })
  
  case <- reactive({
    input$case
  })
  
  est <- reactive({
    input$est
  })
  
  sub <- reactive({
    input$sub
  })
  
  n <- reactive({
      input$obs
  })
  
  L <- reactive({
    input$l
  })
  
  d <- reactive({
    input$delta
  })
  
  output$summary <- renderPrint({
    arl0 <- 1/(2-(2*pnorm(L)))
    arl0
  })
  
  output$plot <- renderPlot({

  })
  

  
  
})