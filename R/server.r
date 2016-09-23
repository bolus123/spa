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
      Quantile = c("Q-0.05", "Q-0.25", "Q-0.50 (MRL0)", "Q-0.75", "Q-0.95"),
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
    
    arl0 <- function (L) {
      a <- 1/(2-(2*pnorm(L)))
      return(a)
    }
    
    cdfrl0 <- function (t,L) {
      b <- t - 1
      a <- pgeom(b, 2-(2*pnorm(L)))
      return(a)
    }
    
    quantileRL0 <- function (p,L) {
      a<-log(1-p)/log(1-(2-(2*pnorm(L))))
      g<-ceiling(a)
      return(g)
    }
    
    arl0r <- round(arl0(L), 2)
    
    cdfmeanr <- round(cdfrl0(arl0(L), L), 2)
    
    median0 <- quantileRL0(0.5,L)
    
    cdfmedianr <-round(cdfrl0(quantileRL0(0.5,L),L),2)
    
    cdf <- curve(cdfrlc(x, L), 1, 2000, n = 10000, ylim = c(0,1),
              xlab = "t", ylab = "", cex.axis = 1.5, type = "l",
              lty = 1, lwd = 3, yaxs="i", xaxs="i", xaxt="n", yaxt="n",
              main = paste("P(In-Control RL <= t)","for", "L =", L))
              xvalues<-c(0,200,400,600,800,1000,1200,1400,1600,1800,2000)
              yvalues<-c(0,0.2,0.4,0.6,0.8,1)
              axis(1,at=xvalues,cex.axis=1.5,las=1)
              axis(2,at=yvalues,cex.axis=1.5,las=1)
              axis(3,arl0r,cex.axis=1,las=1)
              axis(4,cdfmeanr,cex.axis=1,las=1)
              abline(v=arl0(L),lty=5.5,col="blue")
              abline(h=cdfrl0(arl0(L),L),lty=5.5,col="blue")
              axis(1,median0,cex.axis=1,las=1, line=1)
              axis(4,cdfmedianr,cex.axis=1,las=1)
              abline(v=median0 ,lty=5.5,col="red")
              abline(h=cdfrl0(quantileRL0(0.5,L),L),lty=5.5,col="red")
              legend(1250, 0.4, c( paste("MRL0 =", median0) , 
                  paste("ARL0 =", arl0r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),
                  col=c("red","blue"))
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