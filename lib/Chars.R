###### Find frequency of a given char in the ground truth file
## Input: a char
## Output: the frequency (char[])

###### Find frequency of a given word in the ground truth file
## Input: a correction
## Output: a prior for that correction

CharForCorrection <- function(character){
  times <- length(grep(character,CleanLowTrueToken))
  return(times)
}