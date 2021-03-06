overlapEst <-
function( A, B, kmax=3, adjust=c(0.8, 1, 4), n.grid=128,
    type=c("all", "Dhat1", "Dhat4", "Dhat5")) {
  ## Calculates 3 estimates of overlap: numbers 1,4 and 5 in R&L 2009:325
  # Args
  #   A, B : the observations of species A and species B, in RADIANS.
  #   kmax : maximum value of k for kappa estimation
  #   adjust : smoother adjustment; either a single value used for all 3 Deltas,
  #         or a vector of 3 different values; a NA value in adjust means that
  #         the corresponding Dhat will not be estimated. (adjust = 1/c in old code)
  #   n.grid : number of points at which to estimate density; smaller values
  #         give lower precision but run faster in simulations and bootstraps.
  # Returns:
  #   A named vector of 3 overlap estimates

  if(length(adjust) == 1)
    adjust <- rep(adjust, 3)
  adj <- adjust
  type <- match.arg(type)
  index <- switch(type,
            all = 0,
            Dhat1 = {is.na(adj) <- 2:3 ; 1},
            Dhat4 = {is.na(adj) <- c(1,3) ; 2},
            Dhat5 = {is.na(adj) <- 1:2 ; 3} )
  if(index && is.na(adj[index]))
    stop("You specified adjust = NA for the selected type: ", type)
  
  grid <- seq(0, 2*pi, length=n.grid)
  dA.current <- NA
  out <- rep(NA, 3)
  names(out) <- c("Dhat1", "Dhat4", "Dhat5")
  # Get bandwidth
  bw0.A <- getBandWidth(A, kmax=kmax)
  bw0.B <- getBandWidth(B, kmax=kmax)
  if(!is.na(bw0.A) && !is.na(bw0.B))  {  # if no Uniroot Error
    if(!is.na(adj[1]))  {  # Do Dhat1
      dA.current <- 1
      dA <- densityFit(A, grid, bw0.A / adj[1])
      dB <- densityFit(B, grid, bw0.B / adj[1])
      # Prune first value and scale to sum to 1
      dA1 <- dA[-1] / sum(dA[-1])
      dB1 <- dB[-1] / sum(dB[-1])
      out[1] <- sum(pmin(dA1, dB1))  # Dhat1
    }   
    # Get densities for Dhat4 and Dhat5
    n1 <- length(A)
    allObs <- c(A, B)
    n <- length(allObs)
    if(!is.na(adj[2]))  {   # Get kernel densities for Dhat4
      if(n > (n.grid*1.1)) {  # Use interpolation
        if(is.na(adj[1]) | !identical(all.equal(adj[1], adj[2]), TRUE))  { 
          dA.current <- 2
          dA <- densityFit(A, grid, bw0.A / adj[2])
          dB <- densityFit(B, grid, bw0.B / adj[2])
        }
        dAx <- approx(grid, dA, allObs)$y
        dBx <- approx(grid, dB, allObs)$y
      } else {
        dAx <- densityFit(A, allObs, bw0.A / adj[2])
        dBx <- densityFit(B, allObs, bw0.B / adj[2])
      }
      out[2] <- (mean(pmin(1, dBx[1:n1] / dAx[1:n1])) +   # Dhat4
                 mean(pmin(1, dAx[(n1+1):n] / dBx[(n1+1):n]))) / 2
    }
    if(!is.na(adj[3]))  {
      if(is.na(adj[2]) | !identical(all.equal(adj[2], adj[3]), TRUE))  { 
        # Do new kernel densities for Dhat5
        if(n > (n.grid*1.1)) {  # Use interpolation
          if(is.na(dA.current) | !identical(all.equal(adj[dA.current], adj[3]), TRUE))  { 
            dA <- densityFit(A, grid, bw0.A / adj[3])
            dB <- densityFit(B, grid, bw0.B / adj[3])
          }
          dAx <- approx(grid, dA, allObs)$y
          dBx <- approx(grid, dB, allObs)$y
        } else {
          dAx <- densityFit(A, allObs, bw0.A / adj[3])
          dBx <- densityFit(B, allObs, bw0.B / adj[3])
        }
      }
      out[3] <- mean(dBx[1:n1] > dAx[1:n1]) +             # Dhat5
                mean(dAx[(n1+1):n] >= dBx[(n1+1):n])
    }
  }  # end no uniroot error 
  if(type != "all" && index)
    out <- out[index]
  return(out)
}
