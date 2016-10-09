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

dev.new()
plotCDFRL0(3)

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

dev.new()
plotPDFRL0(3)