
# Function to check input data, the vector of observation times.
# Used for the plotting functions, where time taken is less important.
# Throws an error or returns NULL if no issues found.

# Not exported

checkInput <- function(y) {
  if(!is.vector(y) || !is.numeric(y))
    stop("The times of observations must be in a numeric vector.")
  if(length(unique(y)) < 2)
    stop("You have ", length(unique(y)), " different observations; at least 2 are needed to fit a density.")
  if(any(is.na(y)))
    stop("Your data have missing values.")
  if(any(y < 0 | y > 2*pi))
    stop("You have times < 0 or > 2*pi; make sure you are using radians.")
  return(NULL)
}
