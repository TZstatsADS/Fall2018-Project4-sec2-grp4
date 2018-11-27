##############################
## Candidate database
## Ref: C-3
## Input: all the distinct words in a dictionary
## Output: a deletion database (typo because of substitution)
##############################
library(stringr)
library(quanteda)
library(dplyr)
library(stringdist)
SubstitionDatabasePerWord <- function(word){
  possiblewordperchar <- rep(NA,26)
  
  possibleletters <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o",
                       "p","q","r","s","t","u","v","w","x","y","z")
  SubstitionCandidateList <- list()
#  possibleword <- rep(NA,nchar(word)*25)
  for (i in 1:nchar(word)){
    CharToSub <- substr(word,start=i,stop=i)
    Index <- CharToSub !=possibleletters
    
    for (j in 1:26){

      possiblewordperchar[j] <- paste(c(substr(word,start=0,stop=i-1),
                                        sub(CharToSub,possibleletters[j],CharToSub),
                                        substr(word,start=i+1,stop=nchar(word))),collapse ="")
    }
      ResultPerChar <- possiblewordperchar[Index]
      XPerChar <- possibleletters[Index]
#      SubstitionCandidateList <- append(SubstitionCandidateList,list(typo=ResultPerChar,correction=word))
      SubstitionCandidateList[[i]] <- list(typo=ResultPerChar, correction=word,X=XPerChar,Y=substr(word,start=i,stop=i))
        }

  return(SubstitionCandidateList)
}