###### Find frequency of a given word in the ground truth file
## Input: a correction
## Output: a prior for that correction

PriorForCorrection <- function(correction){
  freq <- length(which(correction==CleanLowTrueToken))
  Prior <- (freq+0.5)/(length(CleanLowTrueToken)+length(V)/2)
  return(Prior)
}