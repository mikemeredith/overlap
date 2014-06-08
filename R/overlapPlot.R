overlapPlot <-
function(A, B, xscale=24, xcenter=c("noon", "midnight"), 
    linetype=c(1, 2), linecol=c('black', 'blue'),
    linewidth=c(1,1), olapcol='lightgrey', rug=FALSE,
    n.grid=128, 
    xlab="Time", ylab="Density", ylim, kmax = 3, adjust = 1, ...)  {
  # does a nice plot of two density curves with overlap shaded

  isMidnt <- match.arg(xcenter) == "midnight"

  bwA <- getBandWidth(A, kmax=kmax) / adjust
  bwB <- getBandWidth(B, kmax=kmax) / adjust
  if(is.na(bwA) || is.na(bwB))
    stop("Bandwidth estimation failed.")
  xsc <- if(is.na(xscale)) 1 else xscale / (2*pi)
  xxRad <- seq(0, 2*pi, length=n.grid)
  if(isMidnt)
    xxRad <- xxRad - pi
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA) / xsc
  densB <- densityFit(B, xxRad, bwB) / xsc
  densOL <- pmin(densA, densB)
  if (missing(ylim))
    ylim <- c(0, max(densA, densB))
  
  plot(0, 0, type='n', #las=1, 
    ylim=ylim, xlim=range(xx), xlab=xlab, ylab=ylab, xaxt='n', ...)
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
  polygon(c(max(xx), min(xx), xx), c(0, 0, densOL), border=NA, col=olapcol)
  if(rug)
    segments(xx[1], 0, xx[n.grid], 0, lwd=0.5)
  lines(xx, densA, lty=linetype[1], col=linecol[1], lwd=linewidth[1]) 
  lines(xx, densB, lty=linetype[2], col=linecol[2], lwd=linewidth[2]) 
  if(rug) {
    if(isMidnt) {
      A <- ifelse(A < pi, A, A - 2*pi)
      B <- ifelse(B < pi, B, B - 2*pi)
    }
    axis(1, at=A*xsc, labels=FALSE, tcl= 0.35, lwd=0, lwd.ticks=0.5, col=linecol[1])
    axis(1, at=B*xsc, labels=FALSE, tcl=-0.35, lwd=0, lwd.ticks=0.5, pos=0, col=linecol[2])
  }
  return(invisible(list(x = xx, densityA = densA, densityB = densB)))
}
