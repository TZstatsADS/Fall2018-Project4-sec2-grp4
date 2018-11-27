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
  for (i in 1:length(database)){
    for (j in 1:length(database[[i]])){
      candidate <- append(candidate,database[[i]][[j]]$correction[is.element(word,database[[i]][[j]]$typo)])
    }
  }
  return(candidate)
}


