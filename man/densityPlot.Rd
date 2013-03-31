\name{densityPlot}
\alias{densityPlot}

\title{
Plot fitted kernel densities
}
\description{
Fits a kernel density function to a data set and plots it.
}
\usage{
densityPlot(A, xscale = 24, plot = TRUE, add = FALSE, rug = FALSE, n.grid = 128, kmax = 3, c = 1, ...)
}

\arguments{
  \item{A}{
a vector of times of observations in \emph{radians}, ie. scaled to [\eqn{0, 2\pi}].
}
  \item{xscale}{
The scale for the x axis: 24 (the default) produces a curve with 0 to 24 hours. NA gives a scale in radians, labelled with \eqn{pi}.
}
  \item{plot}{
If TRUE, a plot will be drawn
}
  \item{add}{
If TRUE, the curve will be added to the existing plot. Ignored if plot = FALSE.
}
  \item{rug}{
If TRUE, the original observations will be displayed as a rug at the bottom of the plot. Ignored if plot = FALSE.
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
Further arguments passed to the plotting functions, such as \code{col}, \code{lty} and \code{lwd}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns invisibly a two-column matrix with x and y coordinates which can be used for further plotting; see examples.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Mike Meredith
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Get example data:
data(simulatedData)

# Do basic plots with defaults:
densityPlot(pigObs, rug=TRUE)
densityPlot(tigerObs, add=TRUE, rug=TRUE, col='blue')

# Using object returned:
densityPlot(pigObs, xscale=1, rug=TRUE, lwd=3)
# Don't like the rug with lwd = 3!
pigDens <- densityPlot(pigObs, xscale=1, rug=TRUE)
lines(pigDens, lwd=3)

# Plotting time in radians:
densityPlot(pigObs, xscale=NA, rug=TRUE)
densityPlot(tigerObs, xscale=NA, plot=FALSE, add=TRUE, rug=TRUE, col='red')
# plot = FALSE means nothing plotted, even if other arguments are TRUE.

densityPlot(tigerObs, xscale=NA, add=TRUE, rug=TRUE, col='red')

}

