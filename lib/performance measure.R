###################################
## input: dataframe with corrected words(first column) and corresponding truth words(second column) 
          # the order of columns only affect line 19. It should be corrected words
## output: performance measure table
###################################

measure <- function(df) {
  load("../output/data.RData")
  load("../output/current_ground_truth_txt.RData")
  load("../output/tesseract_vec.RData")
  
  # word level evaluation
  # old interaction
  ground_truth_vec <- str_split(paste(current_ground_truth_txt, collapse = " ")," ")[[1]]
  old_intersect_vec <- vecsets::vintersect(tolower(ground_truth_vec), tolower(tesseract_vec))
  # new interaction
  error_word <- as.vector(dat$error_token[dat$label == 0])
  correct_word <- setdiff(tolower(tesseract_vec), error_word)
  error_correct_word <- df[, 1] # corrected words
  tesseract_delete_error_vec <- c(correct_word, error_correct_word)
  new_intersect_vec <- vecsets::vintersect(tolower(ground_truth_vec), tolower(tesseract_delete_error_vec))
  
  OCR_performance_table <- data.frame("Tesseract" = rep(NA,4),
                                      "Tesseract_with_postprocessing" = rep(NA,4))
  row.names(OCR_performance_table) <- c("word_wise_recall","word_wise_precision",
                                        "character_wise_recall","character_wise_precision")
  OCR_performance_table["word_wise_recall","Tesseract"] <- length(old_intersect_vec)/length(ground_truth_vec)
  OCR_performance_table["word_wise_precision","Tesseract"] <- length(old_intersect_vec)/length(tesseract_vec)
  OCR_performance_table["word_wise_recall","Tesseract_with_postprocessing"] <- length(new_intersect_vec)/length(ground_truth_vec)
  OCR_performance_table["word_wise_precision","Tesseract_with_postprocessing"] <- length(new_intersect_vec)/length(tesseract_delete_error_vec)
  
  # character-level evaluation
  # old interaction
  ground_truth_vec_char <- str_split(paste(ground_truth_vec, collapse = ""),"")[[1]]
  tesseract_vec_char <- str_split(paste(tesseract_vec, collapse = ""), "")[[1]]
  old_intersect_vec_char <- vecsets::vintersect(tolower(ground_truth_vec_char), tolower(tesseract_vec_char))
  # new interaction
  # function used to compare pairs of words in character level
  fun <- function(row) {
    return(vecsets::vintersect(str_split(row[1],""), str_split(row[2],"")))
  }
  correct_char <- str_split(paste(correct_word, collapse = ""),"")[[1]]
  new_intersect_vec_char <- c(vecsets::vintersect(tolower(ground_truth_vec_char), tolower(correct_char)), apply(df, 1, fun))
  
  OCR_performance_table["character_wise_recall","Tesseract"] <- length(old_intersect_vec_char)/length(ground_truth_vec_char)
  OCR_performance_table["character_wise_precision","Tesseract"] <- length(old_intersect_vec_char)/length(tesseract_vec_char)
  OCR_performance_table["character_wise_recall","Tesseract_with_postprocessing"] <- length(new_intersect_vec_char)/length(ground_truth_vec_char)
  OCR_performance_table["character_wise_precision","Tesseract_with_postprocessing"] <- length(new_intersect_vec_char)/length(tesseract_delete_error_vec_char)
  
  return(kable(OCR_performance_table, caption="Summary of OCR performance"))
}
