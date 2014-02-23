\name{densityPlot}
\alias{densityPlot}

\title{
Plot fitted kernel densities
}
\description{
Fits a kernel density function to a data set and plots it.
}
\usage{
densityPlot(A, xscale = 24, add = FALSE, rug = FALSE, n.grid = 128,
   kmax = 3, adjust = 1, ylim, xlab="Time", ylab="Density", ...)
}

\arguments{
  \item{A}{
a vector of times of observations in \emph{radians}, ie. scaled to [\eqn{0, 2\pi}].
}
  \item{xscale}{
The scale for the x axis: 24 (the default) produces a curve with 0 to 24 hours. NA gives a scale in radians, labelled with \eqn{pi}.
}
  \item{add}{
If TRUE, the curve will be added to the existing plot. 
}
  \item{rug}{
If TRUE, the original observations will be displayed as a rug at the bottom of the plot.
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
  \item{xlab, ylab}{
labels for the x and y axes.
}
  \item{ylim}{
optional y limits of the plot; the default goes from 0 to the height of the curve.
}
  \item{\dots}{
Further arguments passed to the plotting functions, such as \code{col}, \code{lty} and \code{lwd}.
}
}

\value{
Returns invisibly a two-column matrix with x and y coordinates which can be used for further plotting; see examples.
}

\author{
Mike Meredith
}

\examples{
# Get example data:
data(simulatedData)

# Do basic plots with defaults:
densityPlot(pigObs, rug=TRUE, main="Simulated data")
densityPlot(tigerObs, add=TRUE, rug=TRUE, col='blue')
legend('topleft', c("Tiger", "Pig"), lty=1, col=c('black', 'blue'), bg='white')

# Using object returned:
densityPlot(pigObs, xscale=1, rug=TRUE, lwd=3)
# Don't like the rug with lwd = 3!
pigDens <- densityPlot(pigObs, xscale=1, rug=TRUE)
lines(pigDens, lwd=3)

# Plotting time in radians:
densityPlot(pigObs, xscale=NA, rug=TRUE)
densityPlot(tigerObs, xscale=NA, add=TRUE, rug=TRUE, col='red')

}

