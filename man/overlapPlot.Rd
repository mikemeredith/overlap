\name{overlapPlot}
\alias{overlapPlot}

\title{
Plot overlapping kernel densities
}
\description{
Fits kernel density functions to two data sets and plots them, shading the area corresponding to the coefficient of overlap.
}
\usage{
overlapPlot(A, B, xscale = 24, linetype = c(1, 2), linecol = c("black", "blue"), olapcol = "lightgrey", n.grid = 128, kmax = 3, c = 1, ...)
}

\arguments{
  \item{A, B}{
vectors of times of observations for species A and species B in \emph{radians}, ie. scaled to [\eqn{0, 2\pi}].
}
  \item{xscale}{
the scale for the x axis: 24 (the default) produces a curve with 0 to 24 hours. NA gives a scale in radians, labelled with \eqn{pi}.
}
  \item{linetype}{
a vector of length 2 giving the line type for each species. Look for \code{lty} in \code{\link{par}} for ways to specify this.
}
  \item{linecol}{
a vector of length 2 giving the line colour for each species. See the Color Specification section in \code{\link{par}} for details.
}
  \item{olapcol}{
the colour to use for the shaded area. See the Color Specification section in \code{\link{par}} for details.
}
  \item{n.grid}{
Number of points at which to estimate the density for plotting; 100 is usually adequate to give a smooth-looking curve.
}
  \item{kmax}{
maximum value of k for optimal bandwidth estimation.
}
  \item{c}{
bandwidth adjustment (scalar).
}
  \item{\dots}{
Further arguments passed to the plotting functions.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns invisibly a two-column matrix with x and y coordinates which can be used for further plotting; see examples.
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
  main="Simulated data")
legend('topleft', c("Tiger", "Pig"), lty=1, col=c("red", "blue"), bty='n')

}

