# word level evaluation

load("../output/label_dat.RData")

ground_truth_vec <- str_split(paste(current_ground_truth_txt, collapse = " ")," ")[[1]]
old_intersect_vec <- vecsets::vintersect(tolower(ground_truth_vec), tolower(tesseract_vec))

orginal_correct_word <- as.vector(label_dat$word[label_dat$label==1])
error_correct_word <- error_correct_word # from correction part
tesseract_delete_error_vec <- c(orginal_correct_word, error_correct_word)
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

ground_truth_vec_char <- str_split(paste(ground_truth_vec, collapse = ""),"")[[1]]
tesseract_vec_char <- str_split(paste(tesseract_vec, collapse = ""), "")[[1]]
old_intersect_vec_char <- vecsets::vintersect(tolower(ground_truth_vec_char), tolower(tesseract_vec_char))

orginal_correct_word <- as.vector(label_dat$word[label_dat$label==1])
orginal_correct_char <- str_split(paste(orginal_correct_word, collapse = ""),"")[[1]]
error_correct_word <- error_correct_word # from correction part
error_correct_char <- str_split(paste(error_correct_word, collapse = ""),"")[[1]]
tesseract_delete_error_vec_char <- c(orginal_correct_char, error_correct_char)
new_intersect_vec_char <- vecsets::vintersect(tolower(ground_truth_vec_char), tolower(tesseract_delete_error_vec_char))

OCR_performance_table["character_wise_recall","Tesseract"] <- length(old_intersect_vec_char)/length(ground_truth_vec_char)
OCR_performance_table["character_wise_precision","Tesseract"] <- length(old_intersect_vec_char)/length(tesseract_vec_char)
OCR_performance_table["character_wise_recall","Tesseract_with_postprocessing"] <- length(new_intersect_vec_char)/length(ground_truth_vec_char)
OCR_performance_table["character_wise_precision","Tesseract_with_postprocessing"] <- length(new_intersect_vec_char)/length(tesseract_delete_error_vec_char)

kable(OCR_performance_table, caption="Summary of OCR performance")