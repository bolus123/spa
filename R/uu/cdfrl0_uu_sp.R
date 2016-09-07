CDFRL0 <- function (a,m,n,L) {
  inside <- function (U) {
    a <- 1 - (((pnorm(((1/sqrt(m))*qnorm(U[1],0,1))+(L*sqrt(qchisq(U[2],m*(n-1))/(m*(n-1)))),0,1) - pnorm(((1/sqrt(m))*qnorm(U[1],0,1))-(L*sqrt(qchisq(U[2],m*(n-1))/(m*(n-1)))),0,1))^a)*dunif(U[1],0,1)*dunif(U[2],0,1))
    return(a)
  }
  b <- adaptIntegrate(inside, lowerLimit = c(0, 0), upperLimit = c(1, 1))$integral
  return (b)
}
##