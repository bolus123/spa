plotcdfrl0 <- function (L) {
  cdfrl02 <- Vectorize(cdfrl0)
  curve(cdfrl02(x,L),1,2000 ,ylim=c(0,1),xlab="t",ylab="",
        cex.axis=1.5,type="l",lty=1,lwd=3,yaxs="i",xaxs="i",
        main=paste("P(In-Control RL <= t)","for", "L=",L ))
}
