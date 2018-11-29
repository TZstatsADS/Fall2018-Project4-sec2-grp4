##############################
## Candidate database
## Ref: C-3
## Input: all the distinct words
## Output: a deletion database (typo because of deletion)
##############################
library(stringr)
library(quanteda)
library(dplyr)

DeletionDatabasePerWord <- function(word){
  DeletionCandidateList <- list()
  possibleword <- rep(NA,nchar(word))
  for (i in 1:nchar(word)){
    possibleword[i] <- paste(c(substr(word,start=0,stop=i-1),substr(word,start=i+1,stop=nchar(word))),collapse ="")
    X <- substr(word,start=i-1,stop=i-1)
    Y <- substr(word,start=i,stop=i)
    DeletionCandidateList[[i]] <- list(typo=possibleword[i], correction=word,X=X,Y=Y)
  }
  return(DeletionCandidateList)

}


