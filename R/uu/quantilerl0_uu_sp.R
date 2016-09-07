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

quantileRL0 <-function (p,m,n,L) {
  CDFm <- function (a) {
    a <- CDFRL0(a,m,n,L) - p
    return(a)
  }
  g<-ceiling(secantc(CDFm,30,1000))
  return(g)
}
