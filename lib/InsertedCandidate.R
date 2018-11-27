##############################
## Candidate database
## Ref: C-3
## Input: a token
## Output: a vector of all the possible corrected candidates (typo because of insertation)
##############################
library(stringr)
library(quanteda)
library(dplyr)
library(stringdist)
InsertedCandidate <- function(word){
  possibleword <- rep(NA,nchar(word))
  IC <- list()
  for (i in 1:nchar(word)){
    possibleword[i] <- paste(c(substr(word,start=0,stop=i-1),substr(word,start=i+1,stop=nchar(word))),collapse ="")
  }
  ind <- is.element(possibleword,V)
  words <- possibleword[ind]
  if (length(words)>0){
    for (j in 1:length(words)){
      IC <- append(IC,words[j])
    }
  }
  return(IC)
}


