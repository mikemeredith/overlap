\name{getBandWidth}
\alias{getBandWidth}

\title{
Optimal bandwidth calculation
}
\description{
Calculates the optimal bandwidth for von Mises kernel density estimation for a given sample. Used internally by other functions in the package.
}
\usage{
getBandWidth(A, kmax = 3)
}

\arguments{
  \item{A}{
a vector of times of observations in \emph{radians}, ie. scaled to [0, \eqn{2\pi}].
}
  \item{kmax}{
maximum moment to use for estimation; see Ridout & Linkie 2009.
}
}
\value{
Optimal bandwidth for the sample data, or NA if estimation fails.
}
\references{
Ridout & Linkie (2009) Estimating overlap of daily activity patterns from camera trap data. \emph{Journal of Agricultural, Biological, and Environmental Statistics} 14:322-337

Taylor (2008) Automatic bandwidth selection for circular density estimation, \emph{Computational Statistics and Data Analysis}, 52:3493-3500.
}
\author{
Code by Martin Ridout, error handling modified by Mike Meredith.
}

\examples{
data(simulatedData)
getBandWidth(tigerObs, kmax = 3)

}
