\name{overlap-package}
\alias{overlap-package}
\alias{overlap}
\docType{package}
\title{
Functions to estimate overlap of diurnal activity patterns of animals
}
\description{
The times recorded on camera trap photos provide information on the period during the day that a species is most active. Species active at the same periods may interact as predator and prey, or as competitors. The functions in this package allow the overlap to be quantified, and provide means of estimating confidence intervals with bootstraps.
}
\details{
\tabular{ll}{
Package: \tab overlap\cr
Type: \tab Package\cr
Version: \tab 0.011\cr
Date: \tab 2013-04-02\cr
License: \tab GPL >=3\cr
}

}
\author{
Mike Meredith, based on work by Martin Ridout.

Maintainer: Mike Meredith <mmeredith@wcs.org>
}

\seealso{
See \code{\link{overlapTrue}} for the definition of the coefficient of overlapping, and \code{\link{overlapEst}} for equations for the estimators. See \code{\link{kerinci}} for an example of calculation of overlap and confidence interval from real data.
}

\references{
Ridout & Linkie (2009) Estimating overlap of daily activity patterns from camera trap data. \emph{Journal of Agricultural, Biological, and Environmental Statistics} 14:322-337
}

\note{
The functions in this package were originally optimised for a simulation study. Hence, speed is important and checking of input is minimal. It is the user's responsibility to make sure that input is valid.

In particular, note that all times are measured in \bold{radians}. If your original data use 0-24 hours or 0-1 days, convert to radians: see the example in \code{\link{kerinci}}. If you need fitted densities in other units, use the matrix returned by \code{\link{overlapPlot}}.
}

\keyword{ package }

\examples{
# Get example data:
data(simulatedData)

# Use defaults:
overlapEst(tigerObs, pigObs)
#     Dhat1     Dhat4     Dhat5 
# 0.2908618 0.2692011 0.2275000  

overlapEst(tigerObs, pigObs, adjust=c(NA, 1, NA))
#    Dhat1     Dhat4     Dhat5 
#       NA 0.2692011        NA 

}
