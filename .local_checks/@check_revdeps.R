# Restart R before testing reverse dependencies

setwd("D:/Github/overlap_package/reverse_dependencies")

# Install new version of 'overlap' first -- only if already committed to GitHub
devtools::install_github("mikemeredith/overlap")
library(overlap)
packageVersion("overlap")

# Find which packages are reverse-depends
library(devtools)
( rds <- revdep("overlap") )
# Install them to be sure we have all their dependencies, incl. Suggests
install.packages(rds, dependencies=TRUE)
# Remove old versions of rev. deps., then download current
unlink(list.files(pattern="*.tar.gz", recursive=TRUE))
download.packages(rds, destdir=".", type="source")

( totest <- dir(pattern = ".tar.gz$") )

# Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = FALSE) # not needed if Suggests were installed.
# tstcall <- paste0("R CMD check --no-build-vignettes --no-manual ", totest)
tstcall <- paste("R CMD check", totest)
isok <- rep(NA, length(tstcall))
for(i in seq_along(tstcall)) {
  cat("\n\n***** ", totest[i], "*****\n\n")
  isok[i] <- system(tstcall[i])
}
which(isok != 0)

sessionInfo()


