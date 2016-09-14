ARL0 <- function (L) {
  a <- 1/(2-(2*pnorm(L)))
  return(a)
}

CDFRL0 <- function (t,L) {
  b<-t-1
  a<-pgeom(b,2-(2*pnorm(L)))
  return(a)
}

PDFRL0 <- function (t,L) {
  b<-t-1
  a<-dgeom(b,2-(2*pnorm(L)))
  return(a)
}

quantileRL0 <-function (p,L) {
  a<-log(1-p)/log(1-(2-(2*pnorm(L))))
  g<-ceiling(a)
  return(g)
}

plotCDFRL0 <- function (L) {
  CDFRL02 <- Vectorize(CDFRL0)
  curve(CDFRL02(x,L),1,2000 ,ylim=c(0,1),n=10000,xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
  title(main=paste("P(IC RL <= t)","for", "L=",L ), line=+2.5)
  xvalues<-c(0,200,400,600,800,1000,1200,1400,1600,1800,2000)
  yvalues<-c(0,0.2,0.4,0.6,0.8,1)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  axis(2,at=yvalues,cex.axis=1.5,las=1)
  
  ARL0r <- round(ARL0(L),2)
  CDFmeanr <-round(CDFRL0(ARL0(L),L),2)
  axis(3,ARL0r,cex.axis=1,las=1)
  axis(4,CDFmeanr,cex.axis=1,las=1)
  abline(v=ARL0(L),lty=5.5,col="blue")
  abline(h=CDFRL0(ARL0(L),L),lty=5.5,col="blue")
  
  Median0 <- quantileRL0(0.5,L)
  CDFmedianr <-round(CDFRL0(quantileRL0(0.5,L),L),2)
  axis(1,Median0,cex.axis=1,las=1, line=1)
  axis(4,CDFmedianr,cex.axis=1,las=1)
  abline(v=Median0 ,lty=5.5,col="red")
  abline(h=CDFRL0(quantileRL0(0.5,L),L),lty=5.5,col="red")
  legend(1250, 0.4, c( paste("MRL0 =", Median0) , paste("ARL0 =", ARL0r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),col=c("red","blue"))
}

plotPDFRL0 <- function (L) {
  PDF2 <- Vectorize(PDFRL0)
  t <- seq(from = 1, to = 2000, by = 10)
  PDFv <- vector(,length(t))
  for (i in 1:length(t)) {
    PDFv[i] <- PDFRL0(t[i],L)
  }
  plot(t,PDFv,xlab="t",ylab="",cex.axis=1.5,type="h",lty=1,lwd=1,yaxs="i",xaxs="i",xaxt="n")
  title(main=paste("P(IC RL = t)","for", "L=",L  ), line=+2.5)
  xvalues<-c(0,200,400,600,800,1000,1200,1400,1600,1800,2000)
  axis(1,at=xvalues,cex.axis=1.5,las=1)
  
  ARL0r <- round(ARL0(L),2)
  axis(3,ARL0r,cex.axis=1,las=1)
  abline(v=ARL0(L),lty=5.5,col="blue")
  
  Median0 <- quantileRL0(0.5,L)
  axis(1,Median0,cex.axis=1,las=1,line=1)
  abline(v=Median0 ,lty=5.5,col="red")
  legend(1250, 0.4, c( paste("MRL0 =", Median0) , paste("ARL0 =", ARL0r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),col=c("red","blue"))
}



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
  a<-log(1-p)/log(1-(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L))))
  g<-ceiling(a)
  return(g)
}

plotCDFRL1 <- function (L,delta,n) {
  CDFRL12 <- Vectorize(CDFRL1)
  curve(CDFRL12(x,L,delta,n),0,100 ,n=1000,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
  title(main=paste("P(OOC RL <= t)","for", "L=",L, "n=",n,"delta=", delta), line=+2.5)
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
  axis(1,Median1,cex.axis=1,las=1, line= 1)
  axis(4,CDFmedianr,cex.axis=1,las=1)
  abline(v=Median1 ,lty=5.5,col="red")
  abline(h=CDFRL1(quantileRL1(0.5,L,delta,n),L,delta,n),lty=5.5,col="red")
  legend(60, 0.4, c( paste("MRL1 =", Median1) , paste("ARL1 =", ARL1r)), cex=1, lty=c(5.5,5.5),lwd=c(1,1),col=c("red","blue"));
}

plotPDFRL1 <- function (L,delta,n) {
  PDF2 <- Vectorize(PDFRL1)
  curve(PDF2(x,L,delta,n),0,100,n=101,xlab="t",ylab="",cex.axis=1.5,type="h",lty=1,lwd=3,yaxs="i",xaxs="i")
  title(main=paste("P(OOC RL = t)","for", "L=",L, "n=",n,"delta=", delta ), line=+2.5)
  ARL1r <- round(ARL1(L,delta,n),2)
  axis(3,ARL1r,cex.axis=1,las=1,col="blue")
  abline(v=ARL1(L,delta,n),lty=5.5,col="blue")
  Median1 <- quantileRL1(0.5,L,delta,n)
  axis(1,Median1,cex.axis=1,las=1, line = 1,col="red")
  abline(v=Median1 ,lty=5.5,col="red")
}

plotPDFRL <- function (L,delta,n) {
  
  PDF2 <- Vectorize(PDFRL1)
  curve(PDF2(x,L,delta,n),0,500,n=501,xlab="t",ylab="",cex.axis=1.5,type="h",lty=1,lwd=1,yaxs="i",xaxs="i")
  ARL1r <- round(ARL1(L,delta,n),2)
  axis(3,ARL1r,cex.axis=1,las=1)
  abline(v=ARL1(L,delta,n),lty=5.5,col="black")
  Median1 <- quantileRL1(0.5,L,delta,n)
  axis(1,Median1,cex.axis=1,las=1, line = 1)
  abline(v=Median1 ,lty=5.5,col="black")
  
  
  t <- seq(from = 1, to = 1000, by = 5)
  PDFv <- vector(,length(t))
  for (i in 1:length(t)) {
    PDFv[i] <- PDFRL0(t[i],L)
  }
  points(t,PDFv,xlab="t",ylab="",cex.axis=1.5,type="h",lty=1,lwd=3,yaxs="i",xaxs="i",col="red")
  title(main=paste("P(RL = t)","for", "L=",L,"n=",n,"delta=",delta,"(IC in red and OOC in black)" ), line=+2.5)
  ARL0r <- round(ARL0(L),2)
  axis(3,ARL0r,cex.axis=1,las=1)
  abline(v=ARL0(L),lty=5.5,col="red")
  Median0 <- quantileRL0(0.5,L)
  axis(1,Median0,cex.axis=1,las=1,line = 1)
  abline(v=Median0 ,lty=5.5,col="red")
  
  
}