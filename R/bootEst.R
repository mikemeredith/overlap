bootEst <-
function(Amat, Bmat, kmax=3, c=NULL, n.grid=128) {
  if(is.null(c))
    c <- c(1.25, 1, 0.25)
  nboot <- min(ncol(Amat), ncol(Bmat))
  bsamp <- matrix(NA, nboot, 3)
  colnames(bsamp) <- c("Dhat1", "Dhat4", "Dhat5")
  for(i in 1:nboot)
    bsamp[i, ] <- overlapEst(Amat[, i], Bmat[,i], kmax=kmax,
                          c=c, n.grid=n.grid) 
  return(bsamp)
}
