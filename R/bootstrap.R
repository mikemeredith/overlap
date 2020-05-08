
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

# testing, based on vignette
if(FALSE) {
  library(overlap)
  data(kerinci)
  timeRad <- kerinci$Time * 2 * pi
  tig2 <- timeRad[kerinci$Zone == 2 & kerinci$Sps == 'tiger']
  mac2 <- timeRad[kerinci$Zone == 2 & kerinci$Sps == 'macaque']
  tigmac2est <- overlapEst(tig2, mac2, type="Dhat4")

  # the old way
  tig2boot <- resample(tig2, 10000)
  mac2boot <- resample(mac2, 10000)
  object.size(mac2boot) # 10 MB
  system.time(
  tigmac2 <- bootEst(tig2boot, mac2boot, type="Dhat4"))  # takes 66 seconds
  ( BSmean <- mean(tigmac2) )

  # the new way
  system.time(
  # tm2 <- BS1(tig2, mac2, nb=10000, type="Dhat4"))  # 64 secs
  tm2 <- BS1(tig2, mac2, nb=10000, type="Dhat4", cores=3))  # 27 secs
  mean(tm2)
  hist(tm2)

  bootCI(tigmac2est, tigmac2)
  bootCI(tigmac2est, tm2)  # small differences in 3rd decimal place, MC error.
}
