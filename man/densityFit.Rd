\name{densityFit}
\alias{densityFit}

\title{
Fits von Mises kernel density to time-of-day data.
}
\description{
Fits von Mises kernel density to time-of-day data. Intended primarily for internal use: input checking is minimal.
}
\usage{
densityFit(x, grid, bw)
}

\arguments{
  \item{x}{
a vector of times of observations in \emph{radians}, ie. scaled to [\eqn{0, 2\pi}].
}
  \item{grid}{
a vector of times in \emph{radians} for which the density is required. This could be a vector of equidistant values in [\eqn{0, 2\pi}], eg. \code{seq(0, 2*pi, length=128)}, or it could be any set of times for which a density is needed.
}
  \item{bw}{
bandwidth, the concentration parameter for the von Mises kernel: smaller values result in smoother curves.
}
}
\value{
Returns a vector of densities corresponding to the times in grid.
}

\author{
C code written by Mike Meredith.
}

\seealso{
\code{\link{getBandWidth}} for appropriate bandwidth.
}
\examples{
# Get example data:
data(simulatedData)

densityFit(tigerObs, c(0, pi/2, pi, 3*pi/2, 2*pi), 50)
# Densities at 6am and 6pm are fairly high, at midnight and midday, tiny.
# A crepuscular species!
}
