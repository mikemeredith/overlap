\name{overlapTrue}
\alias{overlapTrue}

\title{
Calculates the true coefficient of overlapping between two distributions.
}
\description{
Calculates the true coefficient of overlapping between two distributions.
}
\usage{
overlapTrue(d1, d2 = NULL)
}

\arguments{
  \item{d1}{
either a vector or a 2-column matrix of densities for equidistant points from 0 to \eqn{2\pi}; if densities for both 0 and \eqn{2\pi} are included (and are equal), one will be ignored.
}
  \item{d2}{
a vector of densities as for d1; ignored if d1 is a matrix
}
}
\details{
The coefficient of overlapping \eqn{\Delta} for two probability density functions \emph{f(x)} and \emph{g(x)} is given by:
\figure{Delta.jpg}{Formula for Delta}
If the two curves in the plot below represent activity patterns of two species, the coefficient of overlapping is the area under the lower of the two curves, shaded grey in the figure:

\figure{overlap.jpg}{Plot of 2 curves, overlap shaded}

}
\value{
The coefficient of overlap of the two distributions. The function is intended to calculate true overlap for simulated data. If the densities provided are fitted kernel densities, an estimate of overlap results.
}
\references{
Ridout & Linkie (2009) Estimating overlap of daily activity patterns from camera trap data. \emph{Journal of Agricultural, Biological, and Environmental Statistics} 14:322-337
}
\author{
Mike Meredith, based on code by Martin Ridout.
}

\seealso{
\code{\link{overlapEst}} for various estimators of overlap.
}
\examples{
data(simulatedData)

overlapTrue(tigerTrue, pigTrue)

overlapTrue(cbind(tigerTrue, pigTrue))

}
