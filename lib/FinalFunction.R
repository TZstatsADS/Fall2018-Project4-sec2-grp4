
Prob <- function(word){
  
  CC <- AllCorrectionCandidates(word)
  RowScore <- NULL
  ReSult <- NULL
  
  # Insertion
  if (length(CC$Insertion)>0){
    for (i in 1:length(CC$Insertion)){
      cc <- CC$Insertion[[i]]$corr
      priorc <- PriorForCorrection(cc)
      if (nchar(CC$Insertion[[i]]$X)==0){
        CM <- ACM["@",CC$Insertion[[i]]$Y]
        char <- length(CleanLowTrueToken)
        
      }
      else{
        CM <- ACM[CC$Insertion[[i]]$X,CC$Insertion[[i]]$Y]
        char <- CharForCorrection(CC$Insertion[[i]]$X)
        
      }
      RS <- priorc*CM/char
      RowScore <- c(RowScore,RS)
      ReSult <- c(ReSult,cc)
    }
  
  }
  
  # Deletion
  if (length(CC$Deletion)>0){
    for (i in 1:length(CC$Deletion)){
      cc <- CC$Deletion[[i]]$corr
      priorc <- PriorForCorrection(cc)
      if (nchar(CC$Deletion[[i]]$X)==0){
        CM <- DCM["@",CC$Deletion[[i]]$Y]
        char <- CharForCorrection(CC$Deletion[[i]]$Y)
        
      }
      else{
        CM <- DCM[CC$Deletion[[i]]$X,CC$Deletion[[i]]$Y]
        char <- CharForCorrection(paste(c(CC$Deletion[[i]]$X,CC$Deletion[[i]]$Y),collapse=""))
        
      }
      RS <- priorc*CM/char
      RowScore <- c(RowScore,RS)
      ReSult <- c(ReSult,cc)
    }
  }
  
  
  
  # Substitution
  if (length(CC$Substitution)>0){
    for (i in 1:length(CC$Substitution)){
      cc <- CC$Substitution[[i]]$corr
      priorc <- PriorForCorrection(cc)
      CM <- SCM[CC$Substitution[[i]]$X,CC$Substitution[[i]]$Y]
      char <- CharForCorrection(CC$Substitution[[i]]$Y)
      RS <- priorc*CM/char
      RowScore <- c(RowScore,RS)
      ReSult <- c(ReSult,cc)
    }
    
  }
  
  
  
  Percent <- RowScore/sum(RowScore)*100
  return(ReSult[which.max(Percent)])
}

