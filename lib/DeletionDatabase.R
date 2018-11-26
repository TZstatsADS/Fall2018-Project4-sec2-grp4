##############################
## Candidate database
## Ref: C-3
## Input: all the distinct words
## Output: a deletion database (typo because of deletion)
##############################
library(stringr)
library(quanteda)
library(dplyr)
library(stringdist)
DeletionDatabasePerWord <- function(word){
  DeletionCandidateList <- list()
  possibleword <- rep(NA,nchar(word))
  for (i in 1:nchar(word)){
    possibleword[i] <- paste(c(substr(word,start=0,stop=i-1),substr(word,start=i+1,stop=nchar(word))),collapse ="")
    DeletionCandidateList[[i]] <- list(typo=possibleword[i], correction=word)
  }
  return(DeletionCandidateList)

}


