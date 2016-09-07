#install.packages("numDeriv")
library(numDeriv)

secantc <- function(fun, x0, x1, tol=1e-6, niter=100000){
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

ARL0 <- function (L,m,n) {
  
  CARL <- function (U) {
    a <- 1/(2*pnorm(-1*(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1))
    return(a)
  }
  a <- integrate(CARL,0,1)$va
  return (a)
  
}

CDFRL0 <- function (a,m,n,L) {
  inside <- function (U) {
    a <- 1-((2*pnorm(1*(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1)-1)^(a-1))*dunif(U,0,1)
    return(a)
  }
  b <- integrate(inside,0,1)$va
  return (b)
}

CDFRL0 <- function (a,m,n,L) {
  inside <- function (U) {
    a <- 1 - ((pnorm((L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1) - pnorm(-1*(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1))^(a-1))*dunif(U,0,1)
    return(a)
  }
  b <- integrate(inside,0,1)$va
  return (b)
}

plotCDFRL0 <- function (m,n,L) {
  CDFRL02 <- Vectorize(CDFRL0)
  curve(CDFRL02(x,m,n,L),1,2000 ,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",main=paste("P(In-Control RL <= t)","for", "m=", m, "n=",n, "L=",L ))
}

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

quantileRL0 <-function (p,m,n,L) {
  CDFm <- function (a) {
    a <- CDFRL0(a,m,n,L) - p
    return(a)
  }
  g<-ceiling(secantc(CDFm,30,1000))
  return(g)
}

CDFCARL0 <- function (t,m,n,L) {
  v <- m*(n-1)
  zt2 <- -1*qnorm(1/(2*t))
  a <- pchisq( v*((zt2/L)^2), v)
  return(a)
}

plotCDFCARL0 <- function (m,n,L) {
  CDFCARL02 <- Vectorize(CDFCARL0)
  curve(CDFCARL02(x,m,n,L),1,2000 ,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",main=paste("P(In-Control CARL <= t)","for", "m=", m, "n=",n, "L=",L ))
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
  curve(PDF2,1,2000,xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",main=paste("PDF of the In-Control CARL","for", "m=", m, "n=",n, "L=",L ))
}