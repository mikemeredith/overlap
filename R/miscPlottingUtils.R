
# Function to plot the x (time) axis for overlapPlot and densityPlot

# Not exported

plotTimeAxis <- function(xscale) {
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
}