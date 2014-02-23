# Plots a kernel density for circular data

# A: a sample of times of observations in radians
# adjust: smoothing parameter (adjust = 1/c in old code)

densityPlot <-
function(A, xscale=24, add=FALSE, rug=FALSE, 
    n.grid=128, kmax = 3, adjust = 1, ylim, xlab="Time", ylab="Density", ...)  {

  bw <- getBandWidth(A, kmax=kmax) / adjust
  if(is.na(bw))
    stop("Bandwidth estimation failed.")
  # xx <- seq(0, 2*pi, length=n.grid)
  xx <- seq(-pi/4, 9*pi/4, length=n.grid)
  densA <- densityFit(A, xx, bw)
  xsc <- if(is.na(xscale)) 1 else xscale / (2*pi)
  toPlot <- cbind(x = xx * xsc, y = densA / xsc)
  if(missing(ylim))
    ylim <- c(0, max(toPlot[,'y']))
  if(!add)  {
    plot(toPlot, type='n', #las=1, 
      ylim=ylim, xlab=xlab, ylab=ylab, xaxt='n', ...)
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
    abline(h=0, col='grey')
    midnt <- c(0, 2*pi) * xsc
    edge <- par('usr')
    rect(c(edge[1], midnt[2]), rep(edge[3], 2), c(midnt[1], edge[2]), rep(edge[4],2),
      border=NA, col='lightgrey')
    box()
  }
  lines(toPlot, ...)
  if(rug)
    rug(A * xsc, ...)
  return(invisible(toPlot))
}
