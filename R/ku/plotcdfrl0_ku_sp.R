plotCDFRL0 <- function (m,n,L) {
  CDFRL02 <- Vectorize(CDFRL0)
  curve(CDFRL02(x,m,n,L),1,2000,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",main=paste("P(In-Control RL <= t)","for", "m=", m, "n=",n, "L=",L ))
}
