CDFRL0 <- function (t,m,n,L) {
  inside <- function (U) {
    a <- 1-((2*pnorm(1*(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1)-1)^(t-1))*dunif(U,0,1)
    return(a)
  }
  b <- integrate(inside,0,1)$va
  return (b)
}

#dd