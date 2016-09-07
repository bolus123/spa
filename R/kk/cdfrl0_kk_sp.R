cdfrl0 <- function (t,L) {
  a<-pgeom(t,2-(2*pnorm(L)))
  return(a)
}

###########