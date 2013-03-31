\name{bootstrap functions}
\alias{bootEst}
\alias{parResample}
\alias{nonparResample}

\title{
Functions to generate bootstrap estimates of overlap
}
\description{
The strategy implemented involves three steps: 1. Create a matrix of bootstrap samples for each data set, using \code{paramResample} or \code{nonparResample}. 2. Use \code{bootEst} to calculate estimates for each bootstrap sample and create a matrix of bootstrap estimates. 3. Process the bootstrap estimates, eg. to produce confidence intervals with \code{\link{bootCI}}.
}

\usage{
parResample(x, nb, kmax = 3, c = 1, n.grid = 512)
nonparResample(x, nb)
bootEst(Amat, Bmat, kmax = 3, c = NULL, n.grid = 128)
}

\arguments{
  \item{x}{
a numeric vector of time-of-capture data in \emph{radians}, ie. on [0, \eqn{2\pi}] scale
}
  \item{nb}{
the number of bootstrap samples required
}
  \item{Amat, Bmat}{
matrices of resampled data for each species produced by \code{nonparResample} or \code{paramResample}; see Value below.
}
  \item{kmax}{
maximum value of k for optimal bandwidth estimation.
}
  \item{c}{
bandwidth adjustment: see Details.
}
  \item{n.grid}{
number of points at which to estimate density for comparison between species; smaller values give lower precision but run faster in bootstraps.
}
}

\details{
bandwidth adjustment:
\itemize{
\item for \code{paramResample}, a single value.
\item for \code{bootEst}, either a single value used for all 3 overlap estimates, or a vector of 3 different values; a NA value in c means that the corresponding estimate will not be calculated. If NULL, the values indicated by Ridout & Linkie (2009), \code{c(1.25, 1, 0.25)}, are used.
}
}

\value{
The functions \code{paramResample} and \code{nonparResample} return numeric matrices with each column corresponding to a bootstrap sample. Times are in \emph{radians}. Function \code{paramResample} may return a matrix of NAs if bandwidth estimation fails.

Function \code{bootEst} returns a numeric matrix with three columns, one for each estimator of overlap. If argument c contains NAs, the corresponding columns in the output will be NAs. If bandwidth estimation fails for a bootstrap sample, the corresponding row will contain NAs.
}

\references{
Ridout & Linkie (2009) Estimating overlap of daily activity patterns from camera trap data. \emph{Journal of Agricultural, Biological, and Environmental Statistics} 14:322-337
}
\author{
Mike Meredith, including code by Martin Ridout.
}

\seealso{
\code{\link{bootCI}}.
}
\examples{
data(simulatedData)
tigSim <- nonparResample(tigerObs, 99)
dim(tigSim)

pigSim <- nonparResample(pigObs, 99)
boots <- bootEst(tigSim, pigSim)
colMeans(boots)

}
