\name{bootstrap functions}
\alias{bootEst}
\alias{resample}

\title{
Functions to generate bootstrap estimates of overlap
}
\description{
The strategy implemented involves three steps: 1. Create a matrix of bootstrap samples for each data set, using \code{resample}. 2. Use \code{bootEst} to calculate estimates for each bootstrap sample and create a matrix of bootstrap estimates. 3. Process the bootstrap estimates, eg. to produce confidence intervals with \code{\link{bootCI}}.
}

\usage{
resample(x, nb, smooth = TRUE, kmax = 3, adjust = 1, n.grid = 512)

bootEst(Amat, Bmat, kmax = 3, adjust=c(0.8, 1, 4), n.grid = 128)
}

\arguments{
  \item{x}{
a numeric vector of time-of-capture data in \emph{radians}, ie. on [0, \eqn{2\pi}] scale
}
  \item{nb}{
the number of bootstrap samples required
}
  \item{smooth}{
if TRUE, smoothed bootstrap samples are produced.
}
  \item{Amat, Bmat}{
matrices of resampled data for each species produced by \code{resample}; see Value below.
}
  \item{kmax}{
maximum value of k for optimal bandwidth estimation.
}
  \item{adjust}{
bandwidth adjustment: see Details.
}
  \item{n.grid}{
number of points at which to estimate density for comparison between species; smaller values give lower precision but run faster in bootstraps.
}
}

\details{
bandwidth adjustment:
\itemize{
\item for \code{resample}, a single value.
\item for \code{bootEst}, either a single value used for all 3 overlap estimates, or a vector of 3 different values; a NA value in \code{adjust} means that the corresponding estimate will not be calculated. 
}
}

\value{
The function \code{resample} returns a numeric matrix with each column corresponding to a bootstrap sample. Times are in \emph{radians}. It may return a matrix of NAs if \code{smooth = TRUE} and bandwidth estimation fails.

Function \code{bootEst} returns a numeric matrix with three columns, one for each estimator of overlap. If argument \code{adjust} contains NAs, the corresponding columns in the output will be NAs. If bandwidth estimation fails for a bootstrap sample, the corresponding row will contain NAs.
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
tigSim <- resample(tigerObs, 99)
dim(tigSim)

pigSim <- resample(pigObs, 99)
boots <- bootEst(tigSim, pigSim)
colMeans(boots)

}
