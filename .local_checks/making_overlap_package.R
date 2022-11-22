
#### to compile the C code, do not use NppToR to open R ####

setwd("C:/GitHub/overlap_package") # my laptop
setwd("D:/GitHub/overlap_package") # my desktop

# Individual checks
library(devtools)
sIg <- scan("spellcheckIgnore.txt", what='character', comment="#")
tmp <- spell_check("overlap", ignore=c(hunspell::en_stats, sIg), "en_GB")
length(tmp)  # number of misspellings found
tmp  # error if length == 0

# For a dev install
system("R CMD INSTALL overlap")

# Create the overlap package
# ==========================
unlink(list.files(pattern="Rplots.pdf", recursive=TRUE))
pkg <- "overlap_0.3.5.tar.gz"  # <-- fix version number here ################

## on desktop
system("R CMD build overlap")
system(paste("R CMD check ", pkg))
system(paste("R CMD check ", pkg, "--as-cran"))  # as-cran now runs donttest

## on laptop (latex packages not available)
system("R CMD build --no-build-vignettes overlap")
system(paste("R CMD check ", pkg, "--no-manual  --no-build-vignettes"))
system(paste("R CMD check ", pkg, "--as-cran --no-manual --no-build-vignettes"))

# Pick one to install
system(paste("R CMD INSTALL ", pkg))            # install only
system(paste("R CMD INSTALL ", pkg, "--build")) # install and produce the .zip binary

### for the NOTEs on o files and 'abort' calls see ###
# https://stackoverflow.com/questions/64402688/information-on-o-files-for-x64-is-not-available-note-on-r-package-checks-using

# Test it:
library(overlap)
library(testthat)
test_package("overlap")

# Try it out:
rm(list=ls())
library(overlap)
news(p='overlap')
sessionInfo()  # Check versions
?overlap


# Run the examples:
# example("overlap-package")
example("densityPlot")
example("overlapPlot")

?overlap
