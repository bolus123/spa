ARL1 <- function (L,delta,n) {
    a <- 1/(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
    return(a)
}

CDFRL1 <- function (t,L,delta,n) {
  b<-t-1
  a<-pgeom(b,1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
  return(a)
}

PDFRL1 <- function (t,L,delta,n) {
  b<-t-1
  a<-dgeom(b,1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
  return(a)
}

quantileRL1 <-function (p,L,delta,n) {
  a <- qgeom(p, 1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L))) +1
  return(a)
}

plotCDFRL1 <- function (L,delta,n) {
  CDFRL12 <- Vectorize(CDFRL1)
  curve(CDFRL12(x,L,delta,n),0,100 ,n=1000,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
  title(main=paste("P(In-Control RL <= t)","for", "L=",L, "n=",n,"delta=", delta), line=+2.5)
  xvalues<-c(0,20,40,60,80,100)
  yvalues<-c(0,0.2,0.4,0.6,0.8,1)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  axis(2,at=yvalues,cex.axis=1.5,las=1)
  ARL1r <- round(ARL1(L,delta,n),2)
  CDFmeanr <-round(CDFRL1(ARL1(L,delta,n),L,delta,n),2)
  axis(3,ARL1r,cex.axis=1,las=1)
  axis(4,CDFmeanr,cex.axis=1,las=1)
  abline(v=ARL1(L,delta,n),lty=5.5,col="blue")
  abline(h=CDFRL1(ARL1(L,delta,n),L,delta,n),lty=5.5,col="blue")
  Median1 <- quantileRL1(0.5,L,delta,n)
  CDFmedianr <-round(CDFRL1(quantileRL1(0.5,L,delta,n),L,delta,n),2)
  axis(3,Median1,cex.axis=1,las=1)
  axis(4,CDFmedianr,cex.axis=1,las=1)
  abline(v=Median1 ,lty=5.5,col="red")
  abline(h=CDFRL1(quantileRL1(0.5,L,delta,n),L,delta,n),lty=5.5,col="red")
  legend(60, 0.4, c( paste("MRL1 =", Median1) , paste("ARL1 =", ARL1r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),col=c("red","blue"));
}

plotPDFRL1 <- function (L,delta,n) {
  PDF2 <- Vectorize(PDFRL1)
  curve(PDF2(x,L,delta,n),0,100,n=101,xlab="t",ylab="",cex.axis=1.5,type="h",lty=1,lwd=3,yaxs="i",xaxs="i")
  title(main=paste("PDF of the In-Control RL","for", "L=",L, "n=",n,"delta=", delta ), line=+2.5)
  ARL1r <- round(ARL1(L,delta,n),2)
  axis(3,ARL1r,cex.axis=1,las=1)
  abline(v=ARL1(L,delta,n),lty=5.5,col="blue")
  Median1 <- quantileRL1(0.5,L,delta,n)
  axis(3,Median1,cex.axis=1,las=1)
  abline(v=Median1 ,lty=5.5,col="red")

}


