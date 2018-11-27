##############################
## Candidate database
## Ref: C-3
## Input: a word
## Output: a list of all the possible corrected candidates (typo because of insertation)
##############################
library(stringr)
library(quanteda)
library(dplyr)
library(stringdist)
InsertedCandidate <- function(word){
  possibleword <- rep(NA,nchar(word))
  Y <- rep(NA,nchar(word))
  X <- rep(NA,nchar(word))
  IC <- list()
  for (i in 1:nchar(word)){
    possibleword[i] <- paste(c(substr(word,start=0,stop=i-1),substr(word,start=i+1,stop=nchar(word))),collapse ="")
    X[i] <- substr(word,start=i-1,stop=i-1)
    Y[i] <- substr(word,start=i,stop=i)
  }
  ind <- is.element(possibleword,V)
  words <- possibleword[ind]
  cX <- X[ind]
  cY <- Y[ind]
  if (length(words)>0){
    for (j in 1:length(words)){
    #  IC <- append(IC,c(words[j],cX[j],cY[j]))
      IC[[j]] <- list(corr=words[j],X=cX[j],Y=cY[j])
    }
  }
  return(IC)
}


