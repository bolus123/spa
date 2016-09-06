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
    stop
  })
  
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })
  

  
  
})