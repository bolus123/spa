quantilerl0 <-function (p,L) {
  a <- qgeom(p, 2-(2*pnorm(L)))
  return(a)
}
