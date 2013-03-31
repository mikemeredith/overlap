\name{overlapEst}
\alias{overlapEst}

\title{
Estimates of coefficient of overlapping
}
\description{
Calculates up to three estimates of activity pattern overlap based on times of observations for two species.
}
\usage{
overlapEst(A, B, kmax = 3, c = NULL, n.grid = 128)
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
  \item{c}{
bandwidth adjustment; either a single value used for all 3 overlap estimates, or a vector of 3 different values; a NA value in c means that the corresponding estimate will not be calculated. If NULL, the values indicated by Ridout & Linkie (2009), \code{c(1.25, 1, 0.25)}, are used.
}
  \item{n.grid}{
number of points at which to estimate density for comparison between species; smaller values give lower precision but run faster in simulations and bootstraps.
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
}

\value{
Returns a named vector of three estimates of overlap. Individual elements may be NA if the argument \code{c} contained NAs. All will be NA if optimal bandwidth estimation failed.
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

overlapEst(tigerObs, pigObs, c=c(NA, 1, NA))
#    Dhat1     Dhat4     Dhat5 
#       NA 0.2692011        NA 

}
