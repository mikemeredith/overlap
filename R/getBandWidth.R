getBandWidth <-
function(A, kmax = 3) {
  estkappa <- numeric(kmax)
  for (k in 1:kmax) {
    trigmom <- TrigMomRad(A, k)
    # CHECK Afun value at end points: opposite signs or zero
    # Upper limit reduced from 500 to 345 in version 0.3.1.9003, 2018-05-03
    if (Afun(0.0001, trigmom, k) * Afun(345, trigmom, k) <= 0)
      estkappa[k] <- uniroot(Afun, c(0.0001,345), trigmom, k)$root
  }
  kappahat <- max(estkappa[1:kmax])
  if(kappahat > 0)  {
    return( ( 3 * length(A) * kappahat^2 * besselI(2 * kappahat, 2) / 
        (4 * sqrt(pi) * besselI(kappahat, 0)^2) )^(2/5) )
  }  else  {  # Would happen if all k gave uniroot errors.
    return(NA)
  }
}


TrigMomRad <-
function(x, p) {
    sinr <- sum(sin(x))
    cosr <- sum(cos(x))
    circmean <- atan2(sinr, cosr)
    sin.p <- mean(sin(p * (x - circmean)))
    cos.p <- mean(cos(p * (x - circmean)))
    sqrt(sin.p^2 + cos.p^2)
}

Afun <-
function(kappa, trigmom, k) {
  besselI(kappa, k) / besselI(kappa, 0) - trigmom
}
