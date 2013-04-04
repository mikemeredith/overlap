# Calculate confidence intervals based on a set of bootstrap estimates.
# In bootCIlogit, the corrections are perfomed on the logistic scale.

# t0 is point estimate of parameter of interest
# bt is a vector of bootstrap estimates
# conf is required confidence interval
# extreme.ok: should extreme order statistics be used as endpoints? If NA,
#   continue with warning, if FALSE, return NAs.
# Returns: matrix with estimators in rows, lower/upper limits in columns.

bootCI <-
function(t0, bt, conf=0.95, extreme.ok=NA)  {
  out <- matrix(NA, 5, 2)
  dimnames(out) <- list(c("norm", "norm0", "basic", "basic0", "perc"),
                        c("lower","upper"))
  bias <- mean(bt, na.rm=TRUE) - t0
  merr <- sd(bt, na.rm=TRUE) * qnorm((1 + conf)/2)
  out[1, ] <- c(t0 - bias - merr, t0 - bias + merr)
  out[2, ] <- c(t0 - merr, t0 + merr)
  out[5, ] <- norm.inter(bt, (1+c(-conf,conf))/2, extreme.ok)[,2]
  out[3, ] <- 2 * t0 - out[5, 2:1]
  out[4, ] <- out[5, ] - bias
  return(out)
}

bootCIlogit <-
function(t0, bt, conf=0.95, extreme.ok=NA)  
  plogis(bootCI(qlogis(t0), qlogis(bt), conf=conf, extreme.ok=extreme.ok))
