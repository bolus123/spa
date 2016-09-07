#install.packages("cubature")
#install.packages("numDeriv")
library(cubature)
library(numDeriv)

ARL0 <- function (L,m,n) {
  CARL <- function (U) {
    a <- 1/(1 - pnorm(((1/sqrt(m))*qnorm(U[1],0,1))+(L*sqrt(qchisq(U[2],m*(n-1))/(m*(n-1)))),0,1) + pnorm(((1/sqrt(m))*qnorm(U[1],0,1))-(L*sqrt(qchisq(U[2],m*(n-1))/(m*(n-1)))),0,1))
    return(a)
  }
  a <- adaptIntegrate(CARL, lowerLimit = c(0, 0), upperLimit = c(1, 1))$integral
  return (a)
}

CDFRL0 <- function (a,m,n,L) {
  inside <- function (U) {
    a <- 1 - (((pnorm(((1/sqrt(m))*qnorm(U[1],0,1))+(L*sqrt(qchisq(U[2],m*(n-1))/(m*(n-1)))),0,1) - pnorm(((1/sqrt(m))*qnorm(U[1],0,1))-(L*sqrt(qchisq(U[2],m*(n-1))/(m*(n-1)))),0,1))^a)*dunif(U[1],0,1)*dunif(U[2],0,1))
    return(a)
  }
  b <- adaptIntegrate(inside, lowerLimit = c(0, 0), upperLimit = c(1, 1))$integral
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

CDFCARL0 <- function (R,m,n,L) {
  CDFARLC <- function (x,U) {
    l <- 1/(1 - pnorm(((1/sqrt(m))*0)+(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1) + pnorm(((1/sqrt(m))*0)-(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1))
    ARL <- function (Z) {
      a <- 1/(1 - pnorm(((1/sqrt(m))*Z)+(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1) + pnorm(((1/sqrt(m))*Z)-(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1))
      return(a)
    }
    ARL0 <- function (t) {
      k <- ARL(t)-x
      return(k)
    }
    if (x>l) {
      c <- 1
    }
    if ((x<=l) & (x>=1) ) {
      b <- secantc(ARL0,-100000000000000000,0)
      c <- 2*pnorm(-1*abs(b),0,1)
    }
    if (x<1) {
      c <- 0
    }
    return(c)
  }
  x <- R
  CDFCaux <- function (Y) {
    tot <-  CDFARLC(x,Y)
    return(tot)
  }
  fy <- function(y){
    d <- dunif(y,0,1)
    return(d)
  }
  tess <- function(y){
    t <- fy(y)*CDFCaux(y)
    return(t)
  }
  tess2 <- Vectorize(tess)
  yeah <- integrate(tess2, lower = 0, upper = 1)$va
  return (yeah)
}

plotCDFCARL0 <- function (m,n,L) {
  CDFCARL02 <- Vectorize(CDFCARL0)
  curve(CDFCARL02(x,m,n,L),3,2000 ,n = 51,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",main=paste("P(In-Control CARL <= t)","for", "m=", m, "n=",n, "L=",L ))
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
  curve(PDF2,3,2000,n=51,xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",main=paste("PDF of the In-Control CARL","for", "m=", m, "n=",n, "L=",L ))
}