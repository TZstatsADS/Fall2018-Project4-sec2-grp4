#### function to see if a char only have letters
## Only choose words
## 
SS <- function(char){
  a <- length(unlist(str_match_all(char,'[a-zA-Z]')))
  b <- nchar(char)
  
  #a <- unlist(str_match_all(c,'[a-zA-Z]'))
  #b <- paste(a[1:length(a)],collapse = "")
  #c <- tolower(b)
  return(a==b)
  #return(c)
} 
