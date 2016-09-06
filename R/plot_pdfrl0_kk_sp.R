
plotpdfrl0 <- function (L) {
  pdf2 <- Vectorize(pdfrl0)
  curve(pdf2(x,L),1,2000,n=2000,xlab="t",ylab="",cex.axis=1.5,type="l",
        lty=1,lwd=3,yaxs="i",xaxs="i",
        main=paste("PDF of the In-Control RL","for", "L=",L ))
}
