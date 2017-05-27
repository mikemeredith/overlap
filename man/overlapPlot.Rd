\name{overlapPlot}
\alias{overlapPlot}

\title{
Plot overlapping kernel densities
}
\description{
Fits kernel density functions to two data sets and plots them, shading the area corresponding to the coefficient of overlap.
}
\usage{
overlapPlot(A, B, xscale = 24, xcenter = c("noon", "midnight"),
   linetype = c(1, 2), linecol = c("black", "blue"), linewidth = c(1, 1),
   olapcol = "lightgrey", rug=FALSE, extend=NULL,
   n.grid = 128, kmax = 3, adjust = 1, ...)
}

\arguments{
  \item{A, B}{
vectors of times of observations for species A and species B in \emph{radians}, ie. scaled to [\eqn{0, 2\pi}]. Each must include at least 2 unique observations to fit a kernel density.
}
  \item{xscale}{
the scale for the x axis: 24 (the default) produces a curve with 0 to 24 hours. NA gives a scale in radians, labelled with \eqn{pi}.
}
  \item{xcenter}{
the center of the plot on the x axis: 'noon' (default) or 'midnight'.
}
  \item{linetype}{
a vector of length 2 giving the line type for each species. Look for \code{lty} in \code{\link{par}} for ways to specify this.
}
  \item{linecol}{
a vector of length 2 giving the line colour for each species. See the Color Specification section in \code{\link{par}} for details.
}
  \item{linewidth}{
a vector of length 2 giving the line width for each species.
}
  \item{olapcol}{
the colour to use for the shaded area. See the Color Specification section in \code{\link{par}} for details.
}
  \item{rug}{
if TRUE, the original observations will be displayed as a rug at the bottom of the plot, A below B.
}
  \item{extend}{
If not NULL, the plot extends 3 hours before and after the main 24-hr period, and \code{extend} specifies the background colour; the plot is not extended if \code{extend = NULL}.
}
  \item{n.grid}{
number of points at which to estimate the density for plotting; 100 is usually adequate to give a smooth-looking curve.
}
  \item{kmax}{
maximum value of k for optimal bandwidth estimation.
}
  \item{adjust}{
bandwidth adjustment (scalar).
}
  \item{\dots}{
Further arguments passed to the plotting functions such as \code{main, xlab, ylab, ylim}. Values for \code{col, lwd, lty} should be passed with \code{linecol, linewidth, linetype}.
}
}

\value{
Returns invisibly a data frame with columns:

  \item{x}{a vector of equally-spaced times from midnight to midnight inclusive on the scale specified by \code{xscale}.}
  \item{densA}{a vector of length \code{x} with the fitted kernel density for species A.}
  \item{densB}{a similar vector for species B.}
}
\author{
Mike Meredith
}

\seealso{
\code{\link{densityPlot}} for plotting a single density curve.
}
\examples{
# Get example data:
data(simulatedData)

# Do basic plot with defaults:
overlapPlot(pigObs, tigerObs)
# Make it prettier:
overlapPlot(tigerObs, pigObs, linet = c(1,1), linec = c("red", "blue"),
  rug=TRUE, extend="lightgreen", main="Simulated data")
legend("topleft", c("Tiger", "Pig"), lty=1, col=c("red", "blue"), bg="white")
# Add vertical dotted lines to mark sunrise (05:30) and sunset (18:47):
# (times must be in hours if the x-axis is labelled in hours)
abline(v=c(5.5, 18+47/60), lty=3)

# A plot centered on midnight:
overlapPlot(pigObs, tigerObs, xcenter = "m", rug=TRUE)
# Mark sunrise/sunset; values to the left of "00:00" are negative
# so subtract 24:
abline(v=c(5.5, (18+47/60) - 24), lty=3)

}

