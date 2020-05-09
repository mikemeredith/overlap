
# One-by-one bootstrapping

bootstrap <- function(A, B, nb, smooth=TRUE, kmax=3, adjust=NA, n.grid=128,
    type=c("Dhat1", "Dhat4", "Dhat5"), cores=1) {

  type <- match.arg(type)
  if(is.na(adjust))
    adjust <- c(0.8, 1, 4)[match(type, c("Dhat1", "Dhat4", "Dhat5"))]
  if(is.na(cores))
    cores <- parallel::detectCores() - 1
  n <- c(length(A), length(B))
  probA <- probB <- NULL
  out <- rep(NA_real_, nb)

  if(smooth) {
    bw0 <- getBandWidth(A, kmax=kmax)
    if(is.na(bw0))  # if Uniroot Error
      return(out)
    probA <- densityFit(A, seq(0, 2*pi, length=512), bw0)[-1] # first & last are both midnight
    bw0 <- getBandWidth(B, kmax=kmax)
    if(is.na(bw0))
      return(out)
    probB <- densityFit(B, seq(0, 2*pi, length=512), bw0)[-1]
    A <- B <- seq(0, 2*pi, length=512)[-1]
  }

  run1 <- function(...) {
    # resample both
    Ares <- sample(A, n[1], replace=TRUE, prob=probA)
    Bres <- sample(B, n[2], replace=TRUE, prob=probB)
    # calculate overlap
    return(overlap::overlapEst(Ares, Bres, kmax=kmax, adjust=adjust,
        n.grid=n.grid, type=type))
  }
  if(cores == 1) {
    out <- sapply(seq_len(nb), run1)
  } else {
    cl <- makeCluster(cores) ; on.exit(stopCluster(cl))
    out <- parSapply(cl, seq_len(nb), run1)
  }
  return(unname(out))
}
