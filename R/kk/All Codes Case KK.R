ARL0 <- function (L) {
    a <- 1/(2-(2*pnorm(L)))
    return(a)
}

ARL0(3)

CDFRL0 <- function (t,L) {
  a<-pgeom(t,2-(2*pnorm(L)))
  return(a)
}

plotCDFRL0 <- function (L) {
  CDFRL02 <- Vectorize(CDFRL0)
  curve(CDFRL02(x,L),1,2000 ,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",main=paste("P(In-Control RL <= t)","for", "L=",L ))
}

PDFRL0 <- function (t,L) {
  a<-dgeom(t,2-(2*pnorm(L)))
  return(a)
}

plotPDFRL0 <- function (L) {
  PDF2 <- Vectorize(PDFRL0)
  curve(PDF2(x,L),1,2000,n=2000,xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",main=paste("PDF of the In-Control RL","for", "L=",L ))
}

quantileRL0 <-function (p,L) {
    a <- qgeom(p, 2-(2*pnorm(L)))
    return(a)
}
