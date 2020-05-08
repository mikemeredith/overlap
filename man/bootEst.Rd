\name{bootstrap functions}
\alias{bootstrap}
\alias{bootEst}
\alias{resample}

\title{
Functions to generate bootstrap estimates of overlap
}
\description{
\code{bootstrap} takes two sets of times of observations and calculates bootstrap estimates of the chosen estimator of overlap. Alternatively, bootstrap estimates can be calculated in a 2-stage process: (1) create a matrix of bootstrap samples for each data set, using \code{resample}; (2) pass these matrices to \code{bootEst} to obtain the bootstrap estimates.

A vector of bootstrap estimates can then be used to produce confidence intervals with \code{\link{bootCI}}.
}

\usage{
bootstrap(A, B, nb, smooth=TRUE, kmax=3, adjust=NA, n.grid=128,
    type=c("Dhat1", "Dhat4", "Dhat5"), cores=1)

resample(x, nb, smooth = TRUE, kmax = 3, adjust = 1, n.grid = 512)

bootEst(Amat, Bmat, kmax = 3, adjust=c(0.8, 1, 4), n.grid = 128,
      type=c("all", "Dhat1", "Dhat4", "Dhat5"), cores=1)
}

\arguments{
  \item{A, B}{
vectors of times of observations of two different species in radians, ie. scaled to [0, \eqn{2\pi}].
}
  \item{nb}{
the number of bootstrap samples required
}
  \item{smooth}{
if TRUE, smoothed bootstrap samples are produced.
}
  \item{kmax}{
maximum value of k for optimal bandwidth estimation.
}
  \item{adjust}{
bandwidth adjustment. If \code{adjust=NA} in \code{bootstrap}, adjust will be set to 0.8 for \code{type="Dhat1"}, 1 for \code{type="Dhat4"} and 4 for \code{type="Dhat5"}.
}
  \item{n.grid}{
number of points at which to estimate density for comparison between species; smaller values give lower precision but run faster in bootstraps.
}
  \item{type}{
the name of the estimator to use, or "all" to produce all three estimates. See \code{\link{overlapEst}} for recommendations on which to use.
}
  \item{cores}{
the number of cores to use for parallel processing. If NA, all but one of the available cores will used. Parallel processing may take longer than serial processing if the bootstrap runs quickly.
}
  \item{x}{
a numeric vector of time-of-capture data in \emph{radians}, ie. on [0, \eqn{2\pi}] scale
}
  \item{Amat, Bmat}{
matrices of resampled data for each species produced by \code{resample}; see Value below.
}
}

\value{
The function \code{bootstrap} returns a vector of bootstrap estimates. If estimation fails for a bootstrap sample, the corresponding value will be NA.

The function \code{resample} returns a numeric matrix with each column corresponding to a bootstrap sample. Times are in \emph{radians}. It may return a matrix of NAs if \code{smooth = TRUE} and bandwidth estimation fails.

Function \code{bootEst} with \code{type = "all"} returns a numeric matrix with three columns, one for each estimator of overlap, otherwise a vector of bootstrap estimates.
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
est <- overlapEst(tigerObs, pigObs, type="Dhat4")

boots <- bootstrap(tigerObs, pigObs, 99, type="Dhat4")
mean(boots)
hist(boots)
bootCI(est, boots)

# alternatively:
tigSim <- resample(tigerObs, 99)
dim(tigSim)
pigSim <- resample(pigObs, 99)
boots <- bootEst(tigSim, pigSim, type="Dhat4")
mean(boots)
}
