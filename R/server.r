library(shiny)

shinyServer(function(input, output) {

  arl0 <- reactive({
    L <- as.numeric(input$l)
    1/(2 - (2 * pnorm(L)))
  })
  
  far0 <- reactive({
    L <- as.numeric((input$l))
    2 - (2 * pnorm(L))
  })
  
  cdfrl0 <- reactive({
    L <- as.numeric(input$l)
    t <- as.integer(arl0())
    pgeom(t, 2 - (2 * pnorm(L)))
  })
  
  pdfrl0 <- reactive({
    L <- as.numeric(input$l)
    t <- as.integer(arl0())
    dgeom(t, 2 - (2 * pnorm(L)))
  })
  

  q5 <- reactive({
    L <- as.numeric(input$l)
    qgeom(0.05, 2 - (2 * pnorm(L)))
  })
  
  q25 <- reactive({
    L <- as.numeric(input$l)
    qgeom(0.25, 2 - (2 * pnorm(L)))
  })
  
  q50 <- reactive({
    L <- as.numeric(input$l)
    qgeom(0.50, 2 - (2 * pnorm(L)))
  })
  
  q75 <- reactive({
    L <- as.numeric(input$l)
    qgeom(0.75, 2 - (2 * pnorm(L)))
  })
  
  q95 <- reactive({
    L <- as.numeric(input$l)
    qgeom(0.95, 2 - (2 * pnorm(L)))
  })
  
  tbq <- reactive({
    data.frame(
      Percentile = c("5%", "25%", "50%", "75%", "95%"),
      Value = as.numeric(c(q5(), q25(), q50(), q75(), q95())),
      stringsAsFactors = FALSE
    )
  })
  
  tb <- reactive({
    data.frame(
      Metric = c("ARL0", "FAR", "CDFRL0", "PDFRL0"),
      Value = as.numeric(c(arl0(), far0(), cdfrl0(), pdfrl0())),
      stringsAsFactors = FALSE
    )
  })
  
  output$psum <- renderTable({
    tb()
  }, digits = 5)
  
  output$qsum <- renderTable({
    tbq()
  }, digits = 2)
  
  output$cdf <- renderPlot({
    L <- as.numeric(input$l)
    cdfrlc <- function(t, L){
      cdfrl0 <- pgeom(t, 2 - (2 * pnorm(L)))
      Vectorize(cdfrl0)
    }
    cdf <- curve(cdfrlc(x, L), 1, 2000, n = 2000, ylim = c(0,1),
              xlab = "t", ylab = "", cex.axis = 1.5, type = "l",
              lty = 1, lwd = 3, yaxs="i", xaxs="i", 
              main = paste("P(In-Control RL <= t)","for", "L =", L))
    cdf
  })
  
  output$pdf <- renderPlot({
    L <- as.numeric(input$l)
    pdfrlc <- function(t, L){
      pdfrl0 <- dgeom(t, 2 - (2 * pnorm(L)))
      Vectorize(pdfrl0)
    }
    pdf <- curve(pdfrlc(x, L), 1, 2000, n = 2000, xlab = "t", ylab = "",
                 cex.axis = 1.5, type = "l", lty = 1, lwd = 3, yaxs = "i", 
                 xaxs = "i",main = paste("PDF of the In-Control RL","for", "L =", L))
    pdf
  })
  
})