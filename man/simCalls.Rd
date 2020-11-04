\name{Simulated call data}
\alias{simCalls}
\docType{data}
\title{
Simulated data for bird calls influenced by sunrise
}
\description{
A simulated data set of bird calling activity. 80\% occur around sunrise with a strong peak just before sunrise, the remainder occur around sunset. Changes in the times of sunrise and sunset through the year mean that both peaks appear to be broader than they should. The hypothetical location is near St Andrews, UK, longitude 3 degrees West, latitude 56 degrees North (CRS WGS84) and times are GMT throughout (not British Summer Time).
}
\usage{
data(simCalls)
}
\format{

The data set consists of a data frame with two columns:

time is a vector of 100 observations of bird calls in radians. Here \eqn{\pi/2} corresponds to 6am and \eqn{3\pi/2} to 6pm. The time zone is UTC (GMT).

dates is a character vector of dates in ISO format.
}
\source{
Simulated data.
}

\examples{
## See examples for the function 'sunTime'.
}
\keyword{datasets}
