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

ARL1 <- function (delta,m,n,L) {
  CARL <- function (U) {
    a <- 1/(1 - pnorm((-delta*sqrt(n))+((1/sqrt(m))*qnorm(U,0,1))+L,0,1) + pnorm((-delta*sqrt(n))+((1/sqrt(m))*qnorm(U,0,1))-L,0,1))
    return(a)
  }
  a <- integrate(CARL,0,1)$va
  return(a)
}

PS <- function (delta,m,n,L) {
  CARL <- function (U) {
    a <- (1 - pnorm((-delta*sqrt(n))+((1/sqrt(m))*qnorm(U,0,1))+L,0,1) + pnorm((-delta*sqrt(n))+((1/sqrt(m))*qnorm(U,0,1))-L,0,1))
    return(a)
  }
  a <- integrate(CARL,0,1)$va
  return(a)
}

PS(0.5,25,5,3)

ARL1(0.5,25,5,3)

CDFRL1 <- function (a,delta,m,n,L) {
  t <- floor(a)
  inside <- function (U) {
    a <- 1 - (((pnorm((-delta*sqrt(n))+((1/sqrt(m))*qnorm(U,0,1))+L,0,1) - pnorm((-delta*sqrt(n))+((1/sqrt(m))*qnorm(U,0,1))-L,0,1))^(t)))
    return(a)
  }
  b <- integrate(inside,0,1)$va
  return (b)
}

CDFRL1(37,0.5,25,5,3)

PDFRL1 <- function (a,delta,m,n,L) {
  b <- CDFRL1(a,delta,m,n,L)-CDFRL1(a-1,delta,m,n,L)
  return (b)
}

quantileRL1 <-function (p,delta,m,n,L) {
  CDFRL1 <- function (a,delta,m,n,L) {
    t <- a
    inside <- function (U) {
      a <- 1 - (((pnorm((-delta*sqrt(n))+((1/sqrt(m))*qnorm(U,0,1))+L,0,1) - pnorm((-delta*sqrt(n))+((1/sqrt(m))*qnorm(U,0,1))-L,0,1))^(t)))
      return(a)
    }
    b <- integrate(inside,0,1)$va
    return (b)
  }
  
  CDFm <- function (a) {
    a <- CDFRL1(a,delta,m,n,L) - p
    return(a)
  }
  g<-ceiling(secantc(CDFm,1,500))
  return(g)
}

plotCDFRL1 <- function (delta,m,n,L) {
  CDFRL12 <- Vectorize(CDFRL1)
  curve(CDFRL12(x,delta,m,n,L),0,100 ,n=1000,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
  title(main=paste("P(OOC RL <= t)","for", "L=",L,"m=",m, "n=",n,"delta=", delta), line=+2.5)
  xvalues<-c(0,20,40,60,80,100)
  yvalues<-c(0,0.2,0.4,0.6,0.8,1)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  axis(2,at=yvalues,cex.axis=1.5,las=1)
  
  
  ARL1c <- ARL1(delta,m,n,L)
  CDFmeanc <- CDFRL1(ARL1(delta,m,n,L),delta,m,n,L)
  ARL1r <- round(ARL1c,2)
  CDFmeanr <-round(CDFmeanc,2)
  axis(3,ARL1r,cex.axis=1,las=1)
  axis(4,CDFmeanc,cex.axis=1,las=1)
  abline(v=ARL1c,lty=5.5,col="blue")
  abline(h=CDFmeanc,lty=5.5,col="blue")
  
  CDFmedianc <-CDFRL1(quantileRL1(0.5,delta,m,n,L),delta,m,n,L)
  Median1 <- quantileRL1(0.5,delta,m,n,L)
  CDFmedianr <-round(CDFmedianc ,2)
  axis(1,Median1,cex.axis=1,las=1, line=1)
  axis(4,CDFmedianc ,cex.axis=1,las=1)
  abline(v=Median1 ,lty=5.5,col="red")
  abline(h=CDFmedianc ,lty=5.5,col="red")
  legend(60, 0.4, c( paste("MRL1 =", Median1) , paste("ARL1 =", ARL1r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),col=c("red","blue"))
}   

dev.new()
plotCDFRL1(0.5,25,5,3)

plotPDFRL1 <- function (delta,m,n,L) {
  
  t <- seq(from = 1, to = 100, by = 1)
  PDFv <- vector(,length(t))
  for (i in 1:length(t)) {
    PDFv[i] <- PDFRL1(t[i],delta,m,n,L)
  }
  
  plot(t,PDFv,xlab="t",ylab="",cex.axis=1.5,type="h",lty=1,lwd=1,yaxs="i",xaxs="i",xaxt="n")
  title(main=paste("P(OOC RL = t)","for", "L=",L,"m=",m, "n=",n,"delta=", delta ), line=+2.5)
  xvalues<-c(0,20,40,60,80,100)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  
  ARL1r <- round(ARL1(delta,m,n,L),2)
  axis(3,ARL1r,cex.axis=1,las=1)
  abline(v=ARL1(delta,m,n,L),lty=5.5,col="blue")
  
  Median1 <- quantileRL1(0.5,delta,m,n,L)
  axis(1,Median1,cex.axis=1,las=1,line=1)
  abline(v=Median1 ,lty=5.5,col="red")
}

dev.new()
plotPDFRL1(0.5,25,5,3)

CARL1 <- function (Z,delta,m,n,L) {
  a <- 1/(1 - pnorm((-delta*sqrt(n))+((1/sqrt(m))*Z)+L,0,1) + pnorm((-delta*sqrt(n))+((1/sqrt(m))*Z)-L,0,1))
  return(a)
}

CDFCARL0 <- function (x,delta,m,n,L) {
  l <- 1/(1 - pnorm((-delta*sqrt(n))+((1/sqrt(m))*0)+L,0,1) + pnorm((-delta*sqrt(n))+((1/sqrt(m))*0)-L,0,1))
  CARL <- function (Z) {
    a <- 1/(1 - pnorm((-delta*sqrt(n))+((1/sqrt(m))*Z)+L,0,1) + pnorm((-delta*sqrt(n))+((1/sqrt(m))*Z)-L,0,1))
    return(a)
  }
  ARL0 <- function (t) {
    k <- CARL(t)-x
    return(k)
  }
  if (x>l) {
    c <- 1
  }
  if ((x<=l) & (x>=1) ) {
    b <- secantc(ARL0,-100000000,0)
    c <- 2*pnorm(-1*abs(b),0,1)
  }
  if (x<1) {
    c <- 0
  }
  return(c)
}

quantileCARL1 <-function (p,delta,m,n,L) {
  CDFm <- function (a) {
    a <- CDFCARL1(a,delta,m,n,L) - p
    return(a)
  }
  g<-secantc(CDFm,1,500)
  return(g)
}

plotCDFCARL1 <- function (delta,m,n,L) {
  
  CDFCARL12 <- Vectorize(CDFCARL1)
  curve(CDFCARL12(x,delta,m,n,L),1,100 ,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
  title(main=paste("P(OOC CARL <= t)","for", "L=",L, "m=",m, "n=",n,"delta=", delta ), line=+2.5)
  xvalues<-c(0,20,40,60,80,100)
  yvalues<-c(0,0.2,0.4,0.6,0.8,1)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  axis(2,at=yvalues,cex.axis=1.5,las=1)
  
  ARL1r <- round(ARL1(delta,m,n,L),2)
  CDFmeanr <-round(CDFCARL1(ARL1(delta,m,n,L),delta,m,n,L),2)
  axis(3,ARL1r,cex.axis=1,las=1)
  axis(4,CDFCARL1(ARL1(delta,m,n,L),delta,m,n,L),cex.axis=1,las=1)
  abline(v=ARL1(delta,m,n,L),lty=5.5,col="blue")
  abline(h=CDFCARL1(ARL1(delta,m,n,L),delta,m,n,L),lty=5.5,col="blue")
  
  Median1 <- round(quantileCARL1(0.5,delta,m,n,L),2)
  CDFmedianr <-round(CDFCARL1(quantileCARL1(0.5,delta,m,n,L),delta,m,n,L),2)
  axis(1,Median1,cex.axis=1,las=1, line=1)
  axis(4,CDFCARL1(quantileCARL1(0.5,delta,m,n,L),delta,m,n,L),cex.axis=1,las=1)
  abline(v=quantileCARL1(0.5,delta,m,n,L) ,lty=5.5,col="red")
  abline(h=CDFCARL1(quantileCARL1(0.5,delta,m,n,L),delta,m,n,L),lty=5.5,col="red")
  legend(60, 0.4, c( paste("MCARL1 =", Median1) , paste("ARL1 =", ARL1r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),col=c("red","blue"))
}  

dev.new()
plotCDFCARL1(0.5,25,5,3)

plotPDFCARL1 <- function (delta,m,n,L) {
  CDF <- function (h) {
    g <- CDFCARL1(h,delta,m,n,L)
    return(g)
  }
  CDF2 <- Vectorize(CDF )
  PDF <- function (x) {
    f <- grad(CDF2, x)
    return(f)
  }
  PDF2 <- Vectorize(PDF)
  curve(PDF2,2,100,n=100,xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",xaxt="n")
  title(main=paste("pdf of the OOC CARL","for", "L=",L, "m=",m, "n=",n,"delta=", delta   ), line=+2.5)
  xvalues<-c(0,20,40,60,80,100)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  
  ARL1r <- round(ARL1(delta,m,n,L),2)
  axis(3,ARL1r,cex.axis=1,las=1)
  abline(v=ARL1(delta,m,n,L),lty=5.5,col="blue")
  
  Median1 <- round(quantileCARL1(0.5,delta,m,n,L),2)
  axis(1,Median1,cex.axis=1,las=1,line=1)
  abline(v=quantileCARL1(0.5,delta,m,n,L) ,lty=5.5,col="red")
}

dev.new()
plotPDFCARL1(0.5,25,2,3)