# Plots a kernel density for circular data

# A: a sample of times of observations in radians
# adjust: smoothing parameter (adjust = 1/c in old code)

densityPlot <-
function(A, xscale=24, xcenter=c("noon", "midnight"), 
    add=FALSE, rug=FALSE, 
    n.grid=128, kmax = 3, adjust = 1,
    ylim, xlab="Time", ylab="Density", ...)  {

  isMidnt <- match.arg(xcenter) == "midnight"
  bw <- getBandWidth(A, kmax=kmax) / adjust
  if(is.na(bw))
    stop("Bandwidth estimation failed.")
  # xx <- seq(0, 2*pi, length=n.grid)
  xx <- seq(-pi/4, 9*pi/4, length=n.grid)
  if(isMidnt)
    xx <- xx - pi
  densA <- densityFit(A, xx, bw)
  xsc <- if(is.na(xscale)) 1 else xscale / (2*pi)
  toPlot <- cbind(x = xx * xsc, y = densA / xsc)
  if(missing(ylim))
    ylim <- c(0, max(toPlot[,'y']))
  if(!add)  {
    plot(toPlot, type='n', #las=1, 
      ylim=ylim, xlab=xlab, ylab=ylab, xaxt='n', ...)
    if(is.na(xscale)) {
      axis(1, at=c(-pi, -pi/2, 0, pi/2, pi, 3*pi/2, 2*pi),
        labels=c(expression(-pi), expression(-pi/2), "0",
          expression(pi/2), expression(pi),
          expression(3*pi/2), expression(2*pi)))
    } else if(xscale == 24) {
      axis(1, at=c(-12, -6, 0,6,12,18,24),
        labels=c("12:00", "18:00", "0:00", "6:00", "12:00", "18:00", "24:00"))
    } else if(xscale == 1) {
      axis(1, at=c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
        labels=TRUE)
    } else {
      axis(1)
    }
    abline(h=0, col='grey')
    if(isMidnt) {
      wrap <- c(-pi, pi) * xsc
    } else {
      wrap <- c(0, 2*pi) * xsc
    }
    edge <- par('usr')
    rect(c(edge[1], wrap[2]), rep(edge[3], 2), c(wrap[1], edge[2]), rep(edge[4],2),
      border=NA, col='lightgrey')
    box()
  }
  lines(toPlot, ...)
  if(rug)  {
    if(isMidnt)
      A <- ifelse(A < pi, A, A - 2*pi)
    rug(A * xsc, ...)
  }
  return(invisible(toPlot))
}
