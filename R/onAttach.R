
# THIS FILE SHOULD ONLY BE PRESENT IN devel VERSIONS ON GitHub
# Remove when rolling up for CRAN submission.

.onAttach <- function(libname, pkgname) {
  version <- try(utils::packageVersion('overlap'), silent=TRUE)
  if(!inherits(version, "try-error"))
    packageStartupMessage("This is overlap devel version ", version,
      ".\nFor overview type ?overlap; for changes do news(p='overlap').")
}
