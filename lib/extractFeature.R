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
  s <- length(unlist(str_match_all(token,'[!@#$%&]')))
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
  f9 <- ifelse((2*l_a)<l,1,0)
  # >=6 derectly consecutive consonants {1,0}
  f10 <- ifelse(is.na(str_match(token, '[bcdfghjklmnprstvyz]{6}')),0,1)
  # remove first and last, non-alphanumerical, {1,0}
  f11 <- ifelse(is.na(str_match(substr(token, 2, l-1), '[^a-zA-Z0-9]{2}')),0,1)
  # Bigram
  token_bigr <- char_ngrams(unlist(str_split(str_to_lower(token),"")),2,concatenator = "")
  f12 <- getBigr(token_bigr)
  # Most Frequent Symbols
  f13 <- ifelse(max(table(unlist(strsplit(token,"")))) > 2, 
                max(table(unlist(strsplit(token,""))))/l,0)
  # Non-alphabetical Symbols
  l1 <- length(unlist(str_match_all(token,'[a-zA-Z]')))
  f14 <- (l-l1)/l1
  # Levenshtein distance.
  #f15
  return(c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14))
}