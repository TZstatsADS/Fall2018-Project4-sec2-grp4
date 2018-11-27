##############################
## Candidate database
## Ref: C-3
## Input: a word and a database
## Output: a list of all the possible corrected candidates (typo because of deletion and substition)
##############################
library(stringr)
library(quanteda)
library(dplyr)
library(stringdist)
SearchINCandidate <- function(word,database){
  candidate <- list()
  output <- list()
  for (i in 1:length(database)){
    for (j in 1:length(database[[i]])){
      C <- database[[i]][[j]]$correction[is.element(word,database[[i]][[j]]$typo)]
      X <- database[[i]][[j]]$X[word==database[[i]][[j]]$typo]
      Y <- database[[i]][[j]]$Y[is.element(word,database[[i]][[j]]$typo)]
      candidate <- append(candidate,c(C,X,Y))
    }
  }
  for(k in 1:(length(candidate)/3)){
    output[[k]] <- list(corr=candidate[[3*k-2]],X=candidate[[3*k-1]],Y=candidate[[3*k]])
  }
  return(output)
}


