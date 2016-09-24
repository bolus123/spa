library(shiny)

shinyServer(function(input, output) {

########################################################
# In Control Cases
########################################################
  

  
  arl0 <- reactive({
    L <- as.numeric(input$l)
    1/(2 - (2 * pnorm(L)))
  })
  
  far0 <- reactive({
    L <- as.numeric((input$l))
    2 - (2 * pnorm(L))
  })
  
  arl_t <- reactive({
    target <- as.numeric(input$t_0)
    pnorm((2*target - 1)/(2*target))
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
  
  output$box_ic <- renderPlot({
    L <- as.numeric(input$l)
    q1 <- qgeom(0.05, 2 - (2 * pnorm(L)))
    q2 <- qgeom(0.25, 2 - (2 * pnorm(L)))
    q3 <- qgeom(0.50, 2 - (2 * pnorm(L)))
    q4 <- qgeom(0.75, 2 - (2 * pnorm(L)))
    q5 <- qgeom(0.95, 2 - (2 * pnorm(L)))
    m <-  1/(2 - (2 * pnorm(L)))
    
    y <- seq(q1 - 1, q5 + 1, 1)
    x <- seq(1, length(y), 1)
    
    box <- plot(x, y, type = "n", ylab = "", xlab = "", frame.plot = F, axes = F)
        segments(length(x)/2 - q2, q5, length(x)/2 + q2, q5)
        segments(length(x)/2 - q2, q1, length(x)/2 + q2, q1)
        segments(length(x)/2 - q3, q4, length(x)/2 + q3, q4)
        segments(length(x)/2 - q3, q2, length(x)/2 + q3, q2)
        segments(length(x)/2 - q3, q3, length(x)/2 + q3, q3)
        segments(length(x)/2 - q3, q2, length(x)/2 - q3, q4)
        segments(length(x)/2 + q3, q2, length(x)/2 + q3, q4)
        segments(length(x)/2, q4, length(x)/2, q5)
        segments(length(x)/2, q1, length(x)/2, q2)
        points(length(x)/2, m, type = "b", col = "red")
        #text(length(x)/2 + q4, q5, labels = c(paste("Q95 =", q5)))
        #text(length(x)/2 + q4, q4, labels = c(paste("Q75 =", q4)))
        #text(length(x)/2 + q4, q3, labels = c(paste("Q50 =", q3)))
        #text(length(x)/2 + q4, q2, labels = c(paste("Q25 =", q2)))
        #text(length(x)/2 + q4, q1, labels = c(paste("Q5 =", q1)))
        #text(length(x)/2 + q4, m, labels = c(paste("Mean =", m)))
    
    box
    
  })
  
  tb <- reactive({
    data.frame(
      Metric = c("ARL", "FAR", "P(ARL < Target ARL)", "Q-0.05", "Q-0.25", "Q-0.50 (MRL0)", "Q-0.75", "Q-0.95"),
      Value = as.numeric(c(arl0(), far0(), arl_t(), q5(), q25(), q50(), q75(), q95())),
      stringsAsFactors = FALSE
    )
  })
  
  output$psum <- renderTable({
    tb()
  }, digits = 5)
  

####################################################
  
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
              legend(1250, 0.4, c( paste("MRL0 =", median0), 
                  paste("ARL0 =", arl0r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),
                  col=c("red","blue"))
    cdf
  })
  
####################################################################
  
  output$pdf <- renderPlot({
    L <- as.numeric(input$l)
    
    arl0 <- function (L) {
      a <- 1/(2-(2*pnorm(L)))
      return(a)
    }
    
    pdfrl0 <- function (t,L) {
      b<-t-1
      a<-dgeom(b,2-(2*pnorm(L)))
      return(a)
    }
    
    pdf2 <- Vectorize(pdfrl0)
    t <- seq(from = 1, to = 2000, by = 10)
    pdfv <- vector(, length(t))
    for (i in 1:length(t)) {
      pdfv[i] <- pdfrl0(t[i],L)
    }
    
    quantileRL0 <- function (p,L) {
      a<-log(1-p)/log(1-(2-(2*pnorm(L))))
      g<-ceiling(a)
      return(g)
    }
    
    arl0r <- round(arl0(L),2)
    
    median0 <- quantileRL0(0.5,L)
    
    pdf <- plot(t,pdfv,xlab="t",ylab="",cex.axis=1.5,type="h",lty=1,lwd=1,yaxs="i",xaxs="i",xaxt="n")
           title(main=paste("P(IC RL = t)","for", "L=", L), line=+2.5)
           xvalues<-c(0,200,400,600,800,1000,1200,1400,1600,1800,2000)
           axis(1,at=xvalues,cex.axis=1.5,las=1)
           axis(3,arl0r,cex.axis=1,las=1)
           abline(v=arl0(L),lty=5.5,col="blue")
           axis(1,median0,cex.axis=1,las=1,line=1)
           abline(v=median0 ,lty=5.5,col="red")
           legend(1250, 0.4, c( paste("MRL0 =", median0) , paste("ARL0 =", arl0r)),
                  cex=1, lty=c(5.5,5.5),lwd=c(1,1),col=c("red","blue"))
    
    pdf
    
  })
  
####################################################################
# Out of Control
####################################################################
  
  arl1 <- reactive({
    L <- as.numeric(input$l)
    delta <- as.numeric(input$delta)
    n <- as.numeric(input$obs)
    1/(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
  })
  
  far1 <- reactive({
    L <- as.numeric(input$l)
    delta <- as.numeric(input$delta)
    n <- as.numeric(input$obs)
    (1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
  })
  
  arl_td <- reactive({
    target <- as.numeric(input$t_d)
    pnorm((2*target - 1)/(2*target))
  })
  
  cdfrl1 <- reactive({
    L <- as.numeric(input$l)
    delta <- as.numeric(input$delta)
    t <- as.integer(arl1())
    n <- as.numeric(input$obs)
    b <- t-1
    pgeom(b,1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
  })
  
  pdfrl1 <- reactive({
    L <- as.numeric(input$l)
    delta <- as.numeric(input$delta)
    t <- as.integer(arl0())
    n <- as.numeric(input$obs)
    b<-t-1
    a<-dgeom(b,1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
  })
  
  
  q5_1 <- reactive({
    L <- as.numeric(input$l)
    delta <- as.numeric(input$delta)
    n <- as.numeric(input$obs)
    p <- 0.05
    a<-log(1-p)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L))))
    g<-ceiling(a)
    return(g)
  })
  
  q25_1 <- reactive({
    L <- as.numeric(input$l)
    delta <- as.numeric(input$delta)
    n <- as.numeric(input$obs)
    p <- 0.25
    a<-log(1-p)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L))))
    g<-ceiling(a)
    return(g)
  })
  
  q50_1 <- reactive({
    L <- as.numeric(input$l)
    delta <- as.numeric(input$delta)
    n <- as.numeric(input$obs)
    p <- 0.50
    a<-log(1-p)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L))))
    g<-ceiling(a)
    return(g)
  })
  
  q75_1 <- reactive({
    L <- as.numeric(input$l)
    delta <- as.numeric(input$delta)
    n <- as.numeric(input$obs)
    p <- 0.75
    a<-log(1-p)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L))))
    g<-ceiling(a)
    return(g)
  })
  
  q95_1 <- reactive({
    L <- as.numeric(input$l)
    delta <- as.numeric(input$delta)
    n <- as.numeric(input$obs)
    p <- 0.95
    a<-log(1-p)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L))))
    g<-ceiling(a)
    return(g)
  })
  
  output$box_oc <- renderPlot({
    L <- as.numeric(input$l)
    delta <- as.numeric(input$delta)
    n <- as.numeric(input$obs)
    q1 <- ceiling(log(1-0.05)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))))
    q2 <- ceiling(log(1-0.25)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))))
    q3 <- ceiling(log(1-0.50)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))))
    q4 <- ceiling(log(1-0.75)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))))
    q5 <- ceiling(log(1-0.95)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))))
    m <- 1/(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
    
    y <- seq(q1 - 1, q5 + 1, 1)
    x <- seq(1, length(y), 1)
    
    box <- plot(x, y, type = "n", ylab = "", xlab = "", frame.plot = F, axes = F)
    segments(length(x)/2 - q2, q5, length(x)/2 + q2, q5)
    segments(length(x)/2 - q2, q1, length(x)/2 + q2, q1)
    segments(length(x)/2 - q3, q4, length(x)/2 + q3, q4)
    segments(length(x)/2 - q3, q2, length(x)/2 + q3, q2)
    segments(length(x)/2 - q3, q3, length(x)/2 + q3, q3)
    segments(length(x)/2 - q3, q2, length(x)/2 - q3, q4)
    segments(length(x)/2 + q3, q2, length(x)/2 + q3, q4)
    segments(length(x)/2, q4, length(x)/2, q5)
    segments(length(x)/2, q1, length(x)/2, q2)
    points(length(x)/2, m, type = "b", col = "red")
    #text(length(x)/2 + q4, q5, labels = c(paste("Q95 =", q5)))
    #text(length(x)/2 + q4, q4, labels = c(paste("Q75 =", q4)))
    #text(length(x)/2 + q4, q3, labels = c(paste("Q50 =", q3)))
    #text(length(x)/2 + q4, q2, labels = c(paste("Q25 =", q2)))
    #text(length(x)/2 + q4, q1, labels = c(paste("Q5 =", q1)))
    #text(length(x)/2 + q4, m, labels = c(paste("Mean =", m)))
    
    box
    
  })
  
  tb_1 <- reactive({
    data.frame(
      Metric = c("ARL", "FAR", "P(ARL < Target ARL)", "Q-0.05", "Q-0.25", "Q-0.50 (MRL0)", "Q-0.75", "Q-0.95"),
      Value = as.numeric(c(arl1(), far1(), arl_td(), q5_1(), q25_1(), q50_1(), q75_1(), q95_1())),
      stringsAsFactors = FALSE
    )
  })
  
  output$psum_1 <- renderTable({
    tb_1()
  }, digits = 5)
  
  output$cdfrl1 <- renderPlot({
    
    L <- as.numeric(input$l)  
    n <- as.numeric(input$obs)
    delta <- as.numeric(input$delta)
    
    CDFRL1 <- function (t,L,delta,n) {
      b<-t-1
      a<-pgeom(b,1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
      return(a)
    }
    
    ARL1 <- function (L,delta,n) {
      a <- 1/(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
      return(a)
    }
    
    quantileRL1 <-function (p,L,delta,n) {
      a<-log(1-p)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L))))
      g<-ceiling(a)
      return(g)
    }
    
    ARL1r <- round(ARL1(L,delta,n),2)
    CDFmeanr <-round(CDFRL1(ARL1(L,delta,n),L,delta,n),2)
    Median1 <- quantileRL1(0.5,L,delta,n)
    CDFmedianr <-round(CDFRL1(quantileRL1(0.5,L,delta,n),L,delta,n),2)
    

    cdfrl1 <- CDFRL12 <- Vectorize(CDFRL1)
              curve(CDFRL12(x,L,delta,n),0,100 ,n=1000,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
              title(main=paste("P(OOC RL <= t)","for", "L=",L, "n=",n,"delta=", delta), line=+2.5)
              xvalues<-c(0,20,40,60,80,100)
              yvalues<-c(0,0.2,0.4,0.6,0.8,1)
              axis(1,at=xvalues,cex.axis=1.5,las=1)
              axis(2,at=yvalues,cex.axis=1.5,las=1)
              axis(3,ARL1r,cex.axis=1,las=1)
              axis(4,CDFmeanr,cex.axis=1,las=1)
              abline(v=ARL1(L,delta,n),lty=5.5,col="blue")
              abline(h=CDFRL1(ARL1(L,delta,n),L,delta,n),lty=5.5,col="blue")
              axis(1,Median1,cex.axis=1,las=1, line= 1)
              axis(4,CDFmedianr,cex.axis=1,las=1)
              abline(v=Median1 ,lty=5.5,col="red")
              abline(h=CDFRL1(quantileRL1(0.5,L,delta,n),L,delta,n),lty=5.5,col="red")
              legend(60, 0.4, c( paste("MRL1 =", Median1) , paste("ARL1 =", ARL1r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),col=c("red","blue"))
              
    cdfrl1
    
  })
  
  output$pdfrl1 <- renderPlot({
    
    L <- as.numeric(input$l)  
    n <- as.numeric(input$obs)
    delta <- as.numeric(input$delta)
    
    PDFRL1 <- function (t,L,delta,n) {
      b<-t-1
      a<-dgeom(b,1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
      return(a)
    }
    
    ARL1 <- function (L,delta,n) {
      a <- 1/(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
      return(a)
    }
    
    quantileRL1 <-function (p,L,delta,n) {
      a<-log(1-p)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L))))
      g<-ceiling(a)
      return(g)
    }
    
    ARL1r <- round(ARL1(L,delta,n),2)
    Median1 <- quantileRL1(0.5,L,delta,n)
    
    pdfrl1 <- PDF2 <- Vectorize(PDFRL1)
              curve(PDF2(x,L,delta,n),0,100,n=101,xlab="t",ylab="",cex.axis=1.5,type="h",lty=1,lwd=3,yaxs="i",xaxs="i")
              title(main=paste("P(OOC RL = t)","for", "L=",L, "n=",n,"delta=", delta ), line=+2.5)
              axis(3,ARL1r,cex.axis=1,las=1,col="blue")
              abline(v=ARL1(L,delta,n),lty=5.5,col="blue")
              axis(1,Median1,cex.axis=1,las=1, line = 1,col="red")
              abline(v=Median1 ,lty=5.5,col="red")
    pdfrl1
    
  })
  
})