densityFit <-
function(x, grid, bw) {
  n <- length(x)
  nxpts <- length(grid)
  dens <- .C("densRad", as.double(x), as.integer(n),
    as.double(grid), as.integer(nxpts), as.double(bw),
    result = double(length(grid)))
  dens[['result']]
}
