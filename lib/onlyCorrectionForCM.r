
InsertedCandidate1 <- function(word){
  possibleword <- rep(NA,nchar(word))
  IC <- list()
  for (i in 1:nchar(word)){
    possibleword[i] <- paste(c(substr(word,start=0,stop=i-1),substr(word,start=i+1,stop=nchar(word))),collapse ="")
  }
  ind <- is.element(possibleword,V)
  words <- possibleword[ind]
  if (length(words)>0){
    for (j in 1:length(words)){
      #  IC <- append(IC,c(words[j],cX[j],cY[j]))
      IC <- append(IC,words[j])
    }
  }
  return(IC)
}

SearchINCandidate1 <- function(word,database){
  candidate <- list()
  output <- list()
  for (i in 1:length(database)){
    for (j in 1:length(database[[i]])){
      C <- database[[i]][[j]]$correction[is.element(word,database[[i]][[j]]$typo)]
      candidate <- append(candidate,C)
    }
  }
  return(candidate)
}




AllCorrectionCandidates1 <- function(word)
{
  InsCan <- InsertedCandidate1(word)
  DelCan <- SearchINCandidate1(word,WholeDeletionDatabase)
  SubCan <- SearchINCandidate1(word,WholeSubstitionDatabase)
  AllCan <- c(InsCan,DelCan,SubCan)
  return(AllCan)
  
}