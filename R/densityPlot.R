densityPlot <-
function(A, xscale=24, plot=TRUE, add=FALSE, rug=FALSE,
    n.grid=128, kmax = 3, c = 1, ...)  {

  bw <- getBandWidth(A, kmax=kmax) * c
  if(is.na(bw))
    stop("Bandwidth estimation failed.")
  xx <- seq(0, 2*pi, length=n.grid)
  densA <- densityFit(A, xx, bw)
  xsc <- if(is.na(xscale)) 1 else xscale / (2*pi)
  toPlot <- cbind(x = xx * xsc, y = densA / xsc)
  
  if(plot) {
    if(add)  {
      lines(toPlot, ...)
    } else {
      plot(toPlot, type='l', las=1, xlab="Time", ylab="Density", xaxt='n', ...)
      if(is.na(xscale)) {
        axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
          labels=c("0", expression(pi/2), expression(pi),
            expression(3*pi/2), expression(2*pi)))
      } else if(xscale == 24) {
        axis(1, at=c(0,6,12,18,24),
          labels=c("0:00", "6:00", "12:00", "18:00", "24:00"))
      } else {
        axis(1)
      }
    }
    if(rug)
      rug(A * xsc, ...)
  }
  return(invisible(toPlot))
}
