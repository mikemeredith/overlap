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
the statistic estimated from the original sample
}
  \item{bt}{
a vector of bootstrap statistics.
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

Assuming that the original sampling causes the same bias as the bootstrap sampling, we write: mean(bt) - t0 = t0 - t, and hence calculate a bias-corrected estimate, t~ = 2 x t0 - mean(bt).

The percentiles CI, \dQuote{perc}, gives interpolated quantiles of the bootstrap values, so it is centered on the bootstrap mean, mean(bt). 

\dQuote{basic} is a bias-corrected version of \dQuote{perc}, centered on t~.

\dQuote{norm} gives tail cutoffs for a normal distribution with mean = t~ and sd = sd(bt)

These three are equivalent to the confidence intervals returned by \code{boot::boot.ci}.

\dQuote{basic0} is a bias-corrected version of \dQuote{perc}, centered on t0.

\dQuote{norm0} gives tail cutoffs for a normal distribution with mean = t0 and sd = sd(bt)

So: "perc" ignores BS bias completely; "basic" and "norm" assume original sampling introduces a bias equal to the BS bias. Neither may be appropriate for your analysis! "basic0" and "norm0" correct for the difference between mean(bt) and t0, but make no assumptions about any sampling bias affecting t0.

The "logit" versions perform the corrections on the logit scale and then back transform. This would be appropriate for probabilities or proportions.
}
\value{
A named matrix with 2 columns for lower and upper limits and a row for each type of estimate. Values will be NA if the bootstrap sample is too small (after removing NAs): 40 is the minimum for a 95\% confidence interval, 200 for 99\%.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Mike Meredith
}

\seealso{
\code{boot.ci} in package \code{boot}.
}
\examples{
# Generate some sample data:
set.seed(123)
dat <- runif(20)
t0 <- sd(dat)
# and 999 nonparametric bootstrap estimates:
bootmat <- matrix(sample(dat, 20*999, replace=TRUE), 20, 999)
bt <- apply(bootmat, 2, sd)
mean(bt)
mean(bt) - t0  # bootstrap bias

bootCI(t0, bt)
bootCIlogit(t0, bt)

bootCI(t0, bt, conf=0.05)

# Which of these 5% CIs include the point estimate, t0?
bootCI(t0, bt, conf=0.05) < t0  # Should be "TRUE  FALSE"
}
