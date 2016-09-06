ARL0 <- function (L,m,n) {
  CARL <- function (U) {
    a <- 1/(2*pnorm(-1*(L*sqrt(qchisq(U,m*(n-1))/(m*(n-1)))),0,1))
    return(a)
  }
  a <- integrate(CARL,0,1)$va
  return (a)
}