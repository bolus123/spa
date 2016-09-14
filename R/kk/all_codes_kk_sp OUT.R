ARL1 <- function (L,delta,n) {
    a <- 1/(1-(pnorm((-delta*sqrt(n))+L)-pnorm((-delta*sqrt(n))-L)))
    return(a)
}

ARL1(3,0.5,20)

n<-c(5,10,15,20)
delta<-c(-1.5,1,0.5,0,0.5,1,1.5)
ARL1d<-matrix(,nrow = length(delta), ncol = length(n))

for (i in 1:length(n)){
  for (j in 1:length(delta)){   
    ARL1d[j,i]<-ARL1(3,delta[j],n[i])
  }
}





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





