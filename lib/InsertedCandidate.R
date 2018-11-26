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
  for (i in 1:nchar(word)){
    possibleword[i] <- paste(c(substr(word,start=0,stop=i-1),substr(word,start=i+1,stop=nchar(word))),collapse ="")
  }
  ind <- is.element(possibleword,V)
  IC <- possibleword[ind]
  return(IC)
  #### for every words####
  
  #####typo because of insertion####NO NEED!
  
  
  #####typo because of deletion####
  
  
  
  ####typo because of substition###
}


