\name{densityPlot}
\alias{densityPlot}

\title{
Plot fitted kernel densities
}
\description{
Fits a kernel density function to a data set and plots it.
}
\usage{
densityPlot(A, xscale = 24, xcenter = c("noon", "midnight"),
   add = FALSE, rug = FALSE, extend = 'lightgrey',
   n.grid = 128, kmax = 3, adjust = 1, ...)
}

\arguments{
  \item{A}{
a vector of times of observations in \emph{radians}, ie. scaled to [\eqn{0, 2\pi}]. It must include at least 2 unique observations to fit a kernel density.
}
  \item{xscale}{
The scale for the x axis: 24 (the default) produces a curve with 0 to 24 hours. NA gives a scale in radians, labelled with \eqn{pi}.
}
  \item{xcenter}{
the center of the plot on the x axis: 'noon' (default) or 'midnight'.
}
  \item{add}{
If TRUE, the curve will be added to the existing plot. Use the same settings for xscale and xcenter as for the original plot.
}
  \item{rug}{
If TRUE, the original observations will be displayed as a rug at the bottom of the plot.
}
  \item{extend}{
If not NULL, the plot extends 3 hours before and after the main 24-hr period, and \code{extend} specifies the background colour; the plot is not extended if \code{extend = NULL}.
}
  \item{n.grid}{
Number of points at which to estimate the density for plotting; 100 is usually adequate to give a smooth-looking curve.
}
  \item{kmax}{
maximum value of k for optimal bandwidth estimation.
}
  \item{adjust}{
bandwidth adjustment (scalar).
}
  \item{\dots}{
Further arguments passed to the plotting functions, such as \code{col, lty, lwd} or \code{xlab, ylab, main}.
}
}

\value{
Returns invisibly a data frame with x and y coordinates which can be used for further plotting or calculations; see examples.
}

\author{
Mike Meredith
}

\examples{
# Get example data:
data(simulatedData)

# Do basic plot with defaults:
densityPlot(pigObs)

# Prettier plots:
densityPlot(pigObs, extend=NULL, lwd=2)
densityPlot(pigObs, rug=TRUE, main="Simulated data", extend='gold')
densityPlot(tigerObs, add=TRUE, rug=TRUE, col='red')
legend('topleft', c("Tiger", "Pig"), lty=1, col=c('black', 'red'), bg='white')
# Add vertical dotted lines to mark sunrise (say 05:30) and sunset (18:47):
# (times must be in hours if the x-axis is labelled in hours)
abline(v=c(5.5, 18+47/60), lty=3)

# A plot centered on midnight:
densityPlot(pigObs, xcenter = "m")
# Mark sunrise/sunset; values to the left of "00:00" are negative
# so subtract 24:
abline(v=c(5.5, (18+47/60) - 24), lty=3)

# Using object returned:
densityPlot(pigObs, rug=TRUE, lwd=3)
# Don't like the rug with lwd = 3?
pigDens <- densityPlot(pigObs, rug=TRUE)
lines(pigDens, lwd=3)

# Add shading below the curve:
pigDens <- densityPlot(pigObs, extend=NULL)
polygon(pigDens, col='skyblue') # works if density at midnight = 0
tigDens <- densityPlot(tigerObs, extend=NULL)
# Add vertices at (0,0) and (24, 0)
poly <- rbind(c(0,0), tigDens, c(24,0))
polygon(poly, col='pink', border=NA)
lines(tigDens, lwd=2)

# What proportion of the density lies between 9:00 and 15:00 hrs?
wanted <- pigDens$x > 9 & pigDens$x < 15
mean(pigDens$y[wanted]) * 6  # probability mass for the 6 hr period.

# Plotting time in radians:
densityPlot(pigObs, xscale=NA, rug=TRUE)
densityPlot(tigerObs, xscale=NA, add=TRUE, rug=TRUE, col='red')

}

