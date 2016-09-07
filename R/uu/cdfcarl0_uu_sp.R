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