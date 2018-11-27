###### Find frequency of a given word in the ground truth file
## Input: a correction
## Output: a prior for that correection

PriorForCorrection <- function(correction){
  freq <- length(which(correction==ALLgroundtruth_vec))
  Prior <- (freq+0.5)/(length(ALLgroundtruth_vec)+length(V)/2)
  return(Prior)
}