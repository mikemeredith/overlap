norm.inter <-
function(t, alpha, extreme.ok = NA)
#
#  "Interpolation on the normal quantile scale.  For a non-integer
#  order statistic this function interpolates between the surrounding
#  order statistics using the normal quantile scale.  See equation
#  5.8 of Davison and Hinkley (1997)" (package boot source code)
#
# Args:
#   t : numeric vector
#   alpha : vector of quantiles to be estimated
#   extreme.ok : logical, should extreme order statistics be used as endpoints?
#     If NA (default), continue with warning; if FALSE, return NAs.
# Returns: a 2-column matrix, 1st column gives the order statistic, 2nd column
#     the value.

{
    t <- t[is.finite(t)]
    R <- length(t)
    rk <- (R+1)*alpha
    if (!all(rk>1 & rk<R) )  {
      if(is.na(extreme.ok))  {
        warning("extreme order statistics used as endpoints")
      } else if(!extreme.ok) {
        return(matrix(NA, length(alpha), 2))
      }
    }
    k <- trunc(rk)
    inds <- seq_along(k)
    out <- inds
    kvs <- k[k>0 & k<R]
    tstar <- sort(t, partial = sort(union(c(1, R), c(kvs, kvs+1))))
    # Take care of quantiles which do not need interpolation:
    ints <- (k == rk)
    if (any(ints))
      out[inds[ints]] <- tstar[k[inds[ints]]]
    out[k == 0] <- tstar[1L] 
    out[k == R] <- tstar[R] 
    # Do the messy bit:
    not <- function(v) xor(rep(TRUE,length(v)),v)  # Why not use "!"?
    temp <- inds[not(ints) & k != 0 & k != R] # which item(s) need fixing
    temp1 <- qnorm(alpha[temp])
    temp2 <- qnorm(k[temp]/(R+1))
    temp3 <- qnorm((k[temp]+1)/(R+1))
    tk <- tstar[k[temp]]
    tk1 <- tstar[k[temp]+1L]
    out[temp] <- tk + (temp1-temp2)/(temp3-temp2)*(tk1 - tk)  # interpolation
    cbind(round(rk, 2), out)
}
