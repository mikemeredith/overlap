# Calculate a series of estimates of overlap based on bootstrap samples.

# Parallel processing added in v. 0.2.7.9002 (2017-05-22)

# Function to determine the number of iterations for each core
# ============================================================
workLoad <- function(iterations, cores) {
  min <- iterations %/% cores
  xtra <- iterations - min*cores
  return( c(rep(min+1, xtra), rep(min, cores - xtra)) )
}


# Function to do bootstraps on a single core
# ==========================================
#   dataList is a list with the matrices for the 2 species
bootEstSerial <- function(dataList, kmax, adjust, n.grid, type) {
  nboot <- min(sapply(dataList, ncol))
  if(type == "all") {
    bsamp <- matrix(NA, nboot, 3)
  } else {
    bsamp <- matrix(NA, nboot, 1)
  }
  
  for(i in 1:nboot)
    bsamp[i, ] <- overlapEst(dataList[[1]][, i], dataList[[2]][, i],
                kmax=kmax, adjust=adjust, n.grid=n.grid, type=type) 
  return(bsamp)
}

# Main function
# =======================================
bootEst <-
function(Amat, Bmat, kmax=3, adjust=c(0.8, 1, 4), n.grid=128,
      type=c("all", "Dhat1", "Dhat4", "Dhat5"), cores=1) {
  type <- match.arg(type)
  
  # Deal with the parallel stuff:
  if(is.na(cores))
    cores <- parallel::detectCores() - 1
  if(cores > 1) {
    cl <- makeCluster(cores) ; on.exit(stopCluster(cl))
    clusterEvalQ(cl, library(overlap))
    # split the matrices into chunks for each worker:
    nboot <- min(ncol(Amat), ncol(Bmat))
    workerID <- rep(1:cores, workLoad(nboot, cores))
    dataList <- vector("list", cores)
    for(i in 1:cores)
      dataList[[i]] <- list(Amat[, workerID==i], Bmat[, workerID==i])

    # Run the thing
    resList <- parLapply(cl, dataList, bootEstSerial,
            kmax=kmax, adjust=adjust, n.grid=n.grid, type=type)
    bsamp <- do.call(rbind, resList) 
  } else {
    bsamp <- bootEstSerial(list(Amat, Bmat),
        kmax=kmax, adjust=adjust, n.grid=n.grid, type=type)
  }
  if(ncol(bsamp) == 1 && type != "all")  # simple, fast
    return(as.vector(bsamp))
  if(ncol(bsamp) < 3 && type == "all") { # messy! Must be NAs in 'adjust'
    nonNA <- which(!is.na(adjust))
    stopifnot(ncol(bsamp) == length(nonNA))
    out <- matrix(NA, nboot, 3)
    for(i in 1:ncol(bsamp))
      out[, nonNA[i]] <- bsamp[, i]
  } else {
    out <- bsamp
  }
  colnames(out) <- c("Dhat1", "Dhat4", "Dhat5")
  return(out)
}
