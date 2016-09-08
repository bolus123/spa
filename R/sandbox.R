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
  as.numeric(input$l)
})

d <- reactive({
  input$delta
})