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
