\name{overlapEst}
\alias{overlapEst}

\title{
Estimates of coefficient of overlapping
}
\description{
Calculates up to three estimates of activity pattern overlap based on times of observations for two species.
}
\usage{
overlapEst(A, B, kmax = 3, adjust=c(0.8, 1, 4), n.grid = 128,
    type=c("all", "Dhat1", "Dhat4", "Dhat5"))
}
\arguments{
  \item{A}{
a vector of times of observations of species A in radians, ie. scaled to [0, \eqn{2\pi}].
}
  \item{B}{
a vector of times of observations of species B in radians.
}
  \item{kmax}{
maximum value of k for optimal bandwidth estimation.
}
  \item{adjust}{
bandwidth adjustment; either a single value used for all 3 overlap estimates, or a vector of 3 different values. This corresponds to \emph{1/c} in Ridout & Linkie 2009.
}
  \item{n.grid}{
number of points at which to estimate density for comparison between species; smaller values give lower precision but run faster in simulations and bootstraps.
}
  \item{type}{
the name of the estimator to use: \code{Dhat4} is recommended if both samples are larger then 50, otherwise use \code{Dhat1}. See Details. The default is "all" for compatibility with older versions.
}
}
\details{
See \code{\link{overlapTrue}} for the meaning of coefficient of overlapping, \eqn{\Delta}.

These estimators of \eqn{\Delta} use kernel density estimates fitted to the data to approximate the true density functions \emph{f(t)} and \emph{g(t)}. Schmid & Schmidt (2006) propose five estimators of overlap:

Dhat1 is calculated from vectors of densities estimated at \emph{T} equally-spaced times, \emph{t}, between 0 and \eqn{2\pi}:

\figure{Dhat1.jpg}{Equation for Dhat1}

For circular distributions, Dhat2 is equivalent to Dhat1, and Dhat3 is inapplicable.

Dhat4 and Dhat5 use vectors of densities estimated at the times of the observations of the species, \emph{x} and \emph{y}:

\figure{Dhat4.jpg}{Equation for Dhat4}\cr % Need to separate these!
\cr
\figure{Dhat5.jpg}{Equation for Dhat5}

where \emph{n, m} are the sample sizes and \emph{I} is the indicator function (1 if the condition is true, 0 otherwise).

Dhat5 simply checks which curve is higher at each point; even tiny changes in the data can result in large, discontinuous changes in Dhat5, and it can take values > 1. Don't use Dhat5.

Comparing curves at times of actual observations works well if there are enough observations of each species. Simulations show that Dhat4 is best when the smallest sample has at least 50 observations. Dhat1 compares curves at \code{n.grid} equally spaced points, and is best for small samples.
}

\value{
If \code{type = all}, a named vector of three estimates of overlap, otherwise a single estimate. Will be NA if optimal bandwidth estimation failed.
}
\references{
Ridout & Linkie (2009) Estimating overlap of daily activity patterns from camera trap data. \emph{Journal of Agricultural, Biological, and Environmental Statistics} 14:322-337

Schmid & Schmidt (2006) Nonparametric estimation of the coefficient of overlapping - theory and empirical application, \emph{Computational Statistics and Data Analysis}, 50:1583-1596.
}
\author{
Mike Meredith, based on work by Martin Ridout.
}

\seealso{
\code{\link{overlapTrue}}.
}
\examples{
# Get example data:
data(simulatedData)

# Use defaults:
overlapEst(tigerObs, pigObs)
#     Dhat1     Dhat4     Dhat5 
# 0.2908618 0.2692011 0.2275000 

overlapEst(tigerObs, pigObs, type="Dhat4")
#    Dhat4
#    0.2692011
}
