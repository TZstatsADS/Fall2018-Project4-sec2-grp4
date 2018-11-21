##############################
## extractFeature
## Ref: D-3
## Input: a token
## Output: a vector of fourteen features
##############################
library(stringr)
library(quanteda)
library(dplyr)
extractFeature <- function(token) {
  l <- str_length(token)
  # number of vowels
  v <- length(unlist(str_match_all(token,'[aeiou]')))
  f1 <- v/l
  # number of consonants
  c <- length(unlist(str_match_all(token,'[bcdfghjklmnprstvyz]')))
  f2 <- c/l
  # vowels divided by consonants
  f3 <- ifelse(c!=0, v/c, 0)
  # number of special symbols
  s <- length(unlist(str_match_all(token,'[^a-zA-Z0-9]')))
  f4 <- s/l
  # number of lower letters
  low <- length(unlist(str_match_all(token,'[a-z]')))
  f5 <- low/l
  # number of upper letters
  upp <- length(unlist(str_match_all(token,'[A-Z]')))
  f6 <- upp/l
  # number of digits
  d <- length(unlist(str_match_all(token,'[0-9]')))
  f7 <- d/l
  # consecutive sequence of same label
  f8 <- max(ifelse(rle(unlist(strsplit(token,"")))$lengths > 2,
                   max(rle(unlist(strsplit(token,"")))$lengths)/l,0))
  # alphanumberical vs. others {1,0}
  l_a <- length(unlist(str_match_all(token,'[a-zA-Z0-9]')))
  f9 <- ifelse(l_a<(l-l_a),1,0)
  # >=6 derectly consecutive consonants {1,0}
  f10 <- ifelse(is.na(str_match(token, '[bcdfghjklmnprstvyz]{6}')),0,1)
  # remove first and last, >= 2 non-alphanumerical, {1,0}
  f11 <- ifelse(is.na(str_match(substr(token, 2, l-1), '[^a-zA-Z0-9]{2}')),0,1)
  # Bigram
  token_bigr <- char_ngrams(unlist(str_split(str_to_lower(token),"")),2,concatenator = "")
  bigr_measure <- getBigr(token_bigr)
  f12 <- ifelse(is.na(bigr_measure), 0, bigr_measure)
  # Most Frequent Symbols
  f13 <- ifelse(max(table(unlist(strsplit(token,"")))) > 2, 
                max(table(unlist(strsplit(token,""))))/l,0)
  # Non-alphabetical Symbols
  l1 <- length(unlist(str_match_all(token,'[a-zA-Z]')))
  f14 <- ifelse(l1==0, 999, (l-l1)/l1)
  # Levenshtein distance.
  #f15
  return(c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14))
}

##############################
## getBigr
## Ref: English Letter Frequency Counts: Mayzner Revisited or ETAOIN SRHLDCU 
##      by Peter Norvig is an analysis of English letter frequencies using the 
##      Google Corpus Data.
##      https://gist.github.com/lydell/c439049abac2c9226e53#file-bigrams-json
## Input: a letter bigram vector extracted a token
## Output: a measurement defined in paper (f12)
##############################
load("../output/Bigram.RData")
getBigr <- function(token_bigram, c=10000000000) {
  df <- Bigram %>% filter(bigram %in% token_bigram)
  return(sum(df$freq)/(c*length(token_bigram)))
}