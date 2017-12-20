\name{Bootstrap confidence intervals}
\alias{bootCI}
\alias{bootCIlogit}

\title{
Confidence interval calculation from bootstrap samples.
}
\description{
\code{bootCI} calculates five different confidence intervals from bootstrap samples: see details: \code{bootCIlogit} calculates corrections on the logit scale and back-transforms.
}
\usage{
bootCI(t0, bt, conf = 0.95)
bootCIlogit(t0, bt, conf = 0.95)
}
\arguments{
  \item{t0}{
the statistic estimated from the original sample, usually the output from \code{\link{overlapEst}}.
}
  \item{bt}{
a vector of bootstrap statistics, usually the output from \code{\link{bootEst}}
}
  \item{conf}{
a (single!) confidence interval to estimate.
}
}
\details{
Let t = true value of the statistic,\cr
    t0 = estimate of t based on the original sample,\cr
    bt = bootstrap estimates.
    
If bootstrap sampling introduces no bias, E[mean(bt)] = t0, otherwise BS bias = mean(bt) - t0.

Assuming that the original sampling causes the same bias as the bootstrap sampling, we write: mean(bt) - t0 = t0 - t, and hence calculate a bias-corrected estimate, t1 = 2 x t0 - mean(bt).

The percentiles CI, \dQuote{perc}, gives quantiles of the bootstrap values, interpolated if necessary. However, in general, the bootstrap estimates are biased, so \dQuote{perc} should be corrected. 

\dQuote{basic} is a bias-corrected version of \dQuote{perc}, analogous to t1: 2 x t0 - \code{perc}.

\dQuote{norm} gives tail cutoffs for a normal distribution with mean = t1 and sd = sd(bt).

These three are equivalent to the confidence intervals returned by \code{boot::boot.ci}. \dQuote{basic} and \dQuote{norm} are appropriate if you are using the bias-corrected estimator, t1. If you use the uncorrected estimator, t0, you should use \dQuote{basic0} or \dQuote{norm0}:

\dQuote{basic0} is \code{perc} - mean(bt) + t0.

\dQuote{norm0} gives tail cutoffs as before, but with mean = t0 instead of t1.

The "logit" versions perform the corrections on the logit scale and then back transform. This would be appropriate for probabilities or proportions.
}

\value{
A named matrix with 2 columns for lower and upper limits and a row for each type of estimate. Values will be NA if the bootstrap sample is too small (after removing NAs) for estimation: 40 is the minimum for a 95\% confidence interval, 200 for 99\% (though for stable estimates you need at least 999 bootstrap estimates, preferably 10,000).
}

\author{
Mike Meredith
}

\seealso{
\code{boot.ci} in package \code{boot}. See \code{\link{kerinci}} for an example.
}
\examples{
# See ?kerinci
}
