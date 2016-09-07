plotCDFCARL0 <- function (m,n,L) {
  CDFCARL02 <- Vectorize(CDFCARL0)
  curve(CDFCARL02(x,m,n,L),1,2000 ,n = 61,ylim=c(0,1),xlab="t",ylab="",cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",main=paste("P(In-Control CARL <= t)","for", "m=", m, "n=",n, "L=",L ))
}
