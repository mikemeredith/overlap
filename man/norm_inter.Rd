\name{norm.inter}
\alias{norm.inter}

\title{
Interpolation on the normal quantile scale
}
\description{
"For a non-integer order statistic this function interpolates between the surrounding
order statistics using the normal quantile scale.  See equation 5.8 of Davison and Hinkley (1997)" (package boot source code)
}
\usage{
norm.inter(t, alpha, extreme.ok = NA)
}

\arguments{
  \item{t}{
a numeric vector, typically estimates from bootstrap sampling.
}
  \item{alpha}{
vector of quantiles to be estimated.
}
  \item{extreme.ok}{
logical, should extreme order statistics (ie. the maximum or minimum values in t) be used as quantiles? If NA (default), continue with warning; if FALSE, return NAs.
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A 2-column matrix, first column gives the order statistic, second column the value.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) \emph{Bootstrap Methods and Their Application}, Chapter 5. Cambridge University Press.

}
\author{
Code taken from the \code{boot} package function of the same name, but not exported, so not available. The argument \code{extreme.ok} added by Mike Meredith to prevent floods of warnings during simulations.
}

\examples{
# Generate some data:
foo <- runif(200)
norm.inter(foo, c(0.025, 0.975))

diddyfoo <- runif(20)
norm.inter(diddyfoo, c(0.025, 0.975), extreme.ok = TRUE)
norm.inter(diddyfoo, c(0.025, 0.975), extreme.ok = FALSE)
norm.inter(diddyfoo, c(0.025, 0.975)) # Default extreme.ok = NA
# You should see a warning after this.
}
