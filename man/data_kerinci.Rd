\name{Example data}
\alias{Example data}
\alias{kerinci}

\docType{data}
\title{
Times of 'capture' of large mammals
}
\description{
Times of capture of large mammals in camera traps in Kerinci Seblat National Park, Indonesia.
}
\usage{
data(kerinci)
}
\format{
A data frame with 1098 rows and three columns:

\describe{
\item{Zone}{A number indicating which of four zones the record comes from.}
\item{Sps}{A factor indicating which species was observed: boar (wild pig), clouded leopard, golden cat, macaque, muntjac, sambar deer, tapir, or tiger.}
\item{Time}{The time of the observation on a scale of 0 to 1, where 0 and 1 both correspond to midnight}

}
}

\source{
Ridout, M.S. and Linkie, M. (2009) Estimating overlap of daily activity patterns from camera trap data. \emph{Journal of Agricultural, Biological and Environmental Statistics}, 14, 322-337.

\url{https://www.kent.ac.uk/smsas/personal/msr/overlap.html}

}

\examples{
data(kerinci)
str(kerinci)
# Time is in days, ie. 0 to 1:
range(kerinci$Time)
# Convert to radians:
timeRad <- kerinci$Time * 2*pi

# Extract data for tiger and tapir for Zone3:
spsA <- timeRad[kerinci$Zone == 3 & kerinci$Sps == 'tiger']
spsB <- timeRad[kerinci$Zone == 3 & kerinci$Sps == 'tapir']

# Plot the data:
overlapPlot(spsA, spsB)  # Tapir are mainly nocturnal
overlapPlot(spsA, spsB, xcenter="midnight")
legend('topleft', c("Tiger", "Tapir"), lty=c(1, 2), col=c("black", "blue"), bty='n')

# Check sample sizes:
length(spsA)
length(spsB)
# If the smaller sample is less than 50, Dhat1 gives the best estimates, together with
# confidence intervals from a smoothed bootstrap with norm0 or basic0 confidence interval.

# Calculate estimates of overlap:
( Dhats <- overlapEst(spsA, spsB) )  # or just get Dhat1
( Dhat1 <- overlapEst(spsA, spsB, type="Dhat1") )

# Do 999 smoothed bootstrap values:
bs <- bootstrap(spsA, spsB, 999, type="Dhat1")
mean(bs)
hist(bs)
abline(v=Dhat1, col='red', lwd=2)
abline(v=mean(bs), col='blue', lwd=2, lty=3)

# Get confidence intervals:
bootCI(Dhat1, bs)['norm0', ]
bootCI(Dhat1, bs)['basic0', ]
}

\keyword{datasets}
