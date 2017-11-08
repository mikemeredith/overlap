\name{sunTime}
\alias{sunTime}

\title{
Convert clock times to sun times
}
\description{
Converts a vector of clock times to "sun times", by mapping sunrise to \eqn{\pi/2} and sunset to \eqn{3\pi/2}. Sunrise and sunset times are determined based on the dates and locations provided. See Nouvellet et al (2012) for a discussion. Requires \pkg{link{maptools}}.
}
\usage{
sunTime(clockTime, Dates, Coords)
}

\arguments{
  \item{clockTime}{
a vector of times of observations in \emph{radians}, ie. scaled to [\eqn{0, 2\pi}].
}
  \item{Dates}{
a POSIXct object with the dates of the observations; the time zone must be set to the time zone used for 'clockTime'.
}
  \item{Coords}{
a SpatialPoints object with the locations of the observations, or with a single point giving a approximate location for the study area.
}
}
\value{
Returns a vector of "sun times" in \emph{radians}, where \eqn{\pi/2} corresponds to sunrise and \eqn{3\pi/2} to sunset.
}
\references{
Nouvellet et al (2012) Noise clocks and silent sunrises: measurement methods of daily activity pattern. \emph{Journal of Zoology} 286:179-184.
}
\author{
Mike Meredith.
}
\examples{
# Get example data:
data(simCalls)
str(simCalls)

# Convert dates to a POSIXct object with the right time zone (GMT): 
Dates <- as.POSIXct(simCalls$dates, tz="GMT")

# Create a SpatialPoints object with the location
coords <- matrix(c(-3, 56), nrow=1)
Coords <- sp::SpatialPoints(coords, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

st <- sunTime(simCalls$time, Dates, Coords)

par(mfrow=2:1)
densityPlot(st, col='red', lwd=2, xaxt='n', main="Sun time")
axis(1, at=c(0, 6, 12, 18, 24),
  labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
densityPlot(simCalls$time, lwd=2, main="Clock time")
par(mfrow=c(1,1))
}
