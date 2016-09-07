library(numDeriv)
plotPDFRL0 <- function (m,n,L) {
  CDF <- function (h) {
    g <- CDFRL0(h,m,n,L)
    return(g)
  }
  PDF <- function (x) {
    f <- grad(CDF, x)
    return(f)
  }
  PDF2 <- Vectorize(PDF)
  curve(PDF2,1,2000,xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",main=paste("PDF of the In-Control RL","for", "m=", m, "n=",n, "L=",L ))
}
