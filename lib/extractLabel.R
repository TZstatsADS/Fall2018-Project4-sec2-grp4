extractLabel <- function(line_num, current_tesseract_txt, current_ground_truth_txt) {
  tesseract_line <- current_tesseract_txt[line_num]
  ground_truth_line <- current_ground_truth_txt[line_num]
  t_vec <- str_split(tesseract_line," ")[[1]]
  g_vec <- str_split(ground_truth_line," ")[[1]]
  return(getLabel(t_vec, g_vec))
}


getLabel <- function(vec1, vec2) {
  if (length(vec1) == length(vec2)) {
    return(as.numeric(vec1 == vec2))
  }
  else{
    label <- rep(0,length(vec1))
    for (i in 1:length(vec1)) {
      label[i] <- vec1[i] %in% vec2
    }
    return(as.numeric(label))
  }
}