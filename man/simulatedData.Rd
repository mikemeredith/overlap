\name{Simulated data}
\alias{simulatedData}
\alias{tigerObs}
\alias{tigerTrue}
\alias{pigObs}
\alias{pigTrue}
\docType{data}
\title{
Simulated data for diel activity patterns
}
\description{
tigerObs and pigObs are simulated data sets with times of observation. tigerTrue and pigTrue are densities from which the simulated observations were drawn.
}
\usage{
data(simulatedData)
}
\format{

The data set consists of four vectors:

tigerObs is a vector of 100 observations of a crepuscular species in radians.

pigObs is a vector of 80 observations of a diurnal species in radians.

tigerTrue and pigTrue are vectors of densities at 128 times equidistant between 0 and \eqn{2\pi} inclusive.
}
\details{
The figures below show the true densities (solid line), the simulated data (rug at the foot of the plot) and a kernel density fitted to the simulated data (dotted line).

\figure{tigNpig.jpg}{plots of tiger and pig densities}

}

\examples{
data(simulatedData)

xx <- seq(0, 2*pi, length=128)
plot(xx, tigerTrue, type='l') # True density from which sample was drawn
rug(tigerObs)

}
\keyword{datasets}
