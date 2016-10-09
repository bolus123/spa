#install.packages("numDeriv")
library(numDeriv)

secantc <- function(fun, x0, x1, tol=1e-10, niter=100000){
  for ( i in 1:niter ) {
    funx1 <- fun(x1)
    funx0 <- fun(x0)
    x2 <- ( (x0*funx1) - (x1*funx0) )/( funx1 - funx0 )
    funx2 <- fun(x2)
    if (abs(funx2) < tol) {
      return(x2)
    }
    if (funx2 < 0) 
      x0 <- x2
    else
      x1 <- x2
  }
  stop("exceeded allowed number of iteractions")
}

ARL0 <- function (m,n,L) {
  
  CARL <- function (U) {
    a <- 1/(2*pnorm(-1*(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1))
    return(a)
  }
  a <- integrate(CARL,0,1)$va
  return(a)
  
}

ARL0(10,3,3)

CDFRL0 <- function (a,m,n,L) {
  t <- floor(a)
  inside <- function (U) {
    a <- 1-((1-2*pnorm(-1*(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1))^(t))
    return(a)
  }
  b <- integrate(inside,0,1)$va
  return (b)
}

PDFRL0 <- function (a,m,n,L) {
  b <- CDFRL0(a,m,n,L)-CDFRL0(a-1,m,n,L)
  return (b)
}

quantileRL0 <-function (p,m,n,L) {
  CDFRL0 <- function (a,m,n,L) {
    t <- a
    inside <- function (U) {
      a <- ((1-2*pnorm(-1*(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1))^(t))
      return(a)
    }
    b <- 1 - integrate(inside,0,1)$va
    return (b)
  }
  CDFm <- function (a) {
    a <- CDFRL0(a,m,n,L) - p
    return(a)
  }
  g<-ceiling(secantc(CDFm,30,1000))
  return(g)
}

plotCDFRL0 <- function (m,n,L) {
  CDFRL02 <- Vectorize(CDFRL0)
  curve(CDFRL02(x,m,n,L),1,2000 ,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
  title(main=paste("P(IC RL <= t)","for", "L=",L, "m=",m, "n=",n ), line=+2.5)
  xvalues<-c(0,200,400,600,800,1000,1200,1400,1600,1800,2000)
  yvalues<-c(0,0.2,0.4,0.6,0.8,1)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  axis(2,at=yvalues,cex.axis=1.5,las=1)
  
  ARL0r <- round(ARL0(m,n,L),2)
  CDFmeanr <-round(CDFRL0(ARL0(m,n,L),m,n,L),2)
  axis(3,ARL0r,cex.axis=1,las=1)
  axis(4,CDFRL0(ARL0(m,n,L),m,n,L),cex.axis=1,las=1)
  abline(v=ARL0(m,n,L),lty=5.5,col="blue")
  abline(h=CDFRL0(ARL0(m,n,L),m,n,L),lty=5.5,col="blue")
  
  Median0 <- quantileRL0(0.5,m,n,L)
  CDFmedianr <-round(CDFRL0(quantileRL0(0.5,m,n,L),m,n,L),2)
  axis(1,Median0,cex.axis=1,las=1, line=1)
  axis(4,CDFRL0(quantileRL0(0.5,m,n,L),m,n,L),cex.axis=1,las=1)
  abline(v=Median0 ,lty=5.5,col="red")
  abline(h=CDFRL0(quantileRL0(0.5,m,n,L),m,n,L),lty=5.5,col="red")
  legend(1250, 0.4, c( paste("MRL0 =", Median0) , paste("ARL0 =", ARL0r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),col=c("red","blue"))
}

plotPDFRL0 <- function (m,n,L) {
  t <- seq(from = 1, to = 2000, by = 10)
  PDFv <- vector(,length(t))
  for (i in 1:length(t)) {
    PDFv[i] <- PDFRL0(t[i],m,n,L)
  }
  
  plot(t,PDFv,xlab="t",ylab="",cex.axis=1.5,type="h",lty=1,lwd=1,yaxs="i",xaxs="i",xaxt="n")
  title(main=paste("P(IC RL = t)","for", "L=",L , "m=",m, "n=",n  ), line=+2.5)
  xvalues<-c(0,200,400,600,800,1000,1200,1400,1600,1800,2000)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  
  ARL0r <- round(ARL0(m,n,L),2)
  axis(3,ARL0r,cex.axis=1,las=1)
  abline(v=ARL0(m,n,L),lty=5.5,col="blue")
  
  Median0 <- quantileRL0(0.5,m,n,L)
  axis(1,Median0,cex.axis=1,las=1,line=1)
  abline(v=Median0 ,lty=5.5,col="red")
}

plotPDFRL0(10,3,3)

CDFCARL0 <- function (t,m,n,L) {
  v <- m*(n-1)
  zt2 <- -1*qnorm(1/(2*t))
  a <- pchisq( v*((zt2/L)^2), v)
  return(a)
}

quantileCARL0 <- function (x,m,n,L) {
  g <- 1/(2*pnorm(-1*L*sqrt(qchisq(x,m*(n-1))/(m*(n-1))),0,1))
  return(g)
}

plotCDFCARL0 <- function (m,n,L) {
  
  CDFCARL02 <- Vectorize(CDFCARL0)
  curve(CDFCARL02(x,m,n,L),1,2000 ,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
  title(main=paste("P(IC CARL <= t)","for", "L=",L, "m=",m, "n=",n ), line=+2.5)
  xvalues<-c(0,200,400,600,800,1000,1200,1400,1600,1800,2000)
  yvalues<-c(0,0.2,0.4,0.6,0.8,1)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  axis(2,at=yvalues,cex.axis=1.5,las=1)
  
  ARL0r <- round(ARL0(m,n,L),2)
  CDFmeanr <-round(CDFCARL0(ARL0(m,n,L),m,n,L),2)
  axis(3,ARL0r,cex.axis=1,las=1)
  axis(4,CDFCARL0(ARL0(m,n,L),m,n,L),cex.axis=1,las=1)
  abline(v=ARL0(m,n,L),lty=5.5,col="blue")
  abline(h=CDFCARL0(ARL0(m,n,L),m,n,L),lty=5.5,col="blue")
  
  Median0 <- round(quantileCARL0(0.5,m,n,L),2)
  CDFmedianr <- round(CDFCARL0(quantileCARL0(0.5,m,n,L),m,n,L),2)
  axis(1,Median0,cex.axis=1,las=1, line=1)
  axis(4,CDFmedianr,cex.axis=1,las=1)
  abline(v=Median0 ,lty=5.5,col="red")
  abline(h=CDFCARL0(quantileCARL0(0.5,m,n,L),m,n,L),lty=5.5,col="red")
  legend(1250, 0.4, c( paste("MCARL0 =", Median0) , paste("ARL0 =", ARL0r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),col=c("red","blue"))
}

plotPDFCARL0 <- function (m,n,L) {
  CDF <- function (h) {
    g <- CDFCARL0(h,m,n,L)
    return(g)
  }
  PDF <- function (x) {
    f <- grad(CDF, x)
    return(f)
  }
  PDF2 <- Vectorize(PDF)
  curve(PDF2,1,2000,xlab="t",ylab="",n=1000,cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",xaxt="n")
  title(main=paste("pdf of the IC CARL","for", "L=",L, "m=",m, "n=",n  ), line=+2.5)
  xvalues<-c(0,200,400,600,800,1000,1200,1400,1600,1800,2000)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  
  ARL0r <- round(ARL0(m,n,L),2)
  axis(3,ARL0r,cex.axis=1,las=1)
  abline(v=ARL0(m,n,L),lty=5.5,col="blue")
  
  Median0 <- round(quantileCARL0(0.5,m,n,L),2)
  axis(1,Median0,cex.axis=1,las=1,line=1)
  abline(v=quantileCARL0(0.5,m,n,L) ,lty=5.5,col="red")
}
