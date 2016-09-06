pdfrl0 <- function (t,L) {
  a<-dgeom(t,2-(2*pnorm(L)))
  return(a)
}