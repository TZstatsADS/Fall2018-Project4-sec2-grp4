library(caret)

#Defining the groupname in the data column
dat <- dat %>% mutate(group_num = case_when(file_num %in% c(1:10)~"A",
                                            file_num %in% c(11:38)~"B",
                                            file_num %in% c(39:41)~"C",
                                            file_num %in% c(42:68)~"D",
                                            file_num %in% c(69:100)~"E"
))

# Changing the levels of the labels
dat <- dat %>% mutate(label = case_when(label == 1 ~"Correct",
                                        label == 0 ~"Error"))

# Converting labels to factors
dat$label <- factor(dat$label)


#SVM Functions (Groups 1-4)
svm.func <- function(dat){
  library(caret)
  
  # defining the radial grid with the two hyperparameters to be tuned
  grid_radial <- expand.grid(sigma = c(0.25, 1), C = c(1,10))
  grid2 <- expand.grid(sigma = c(0.25), C = c(2))
  
  # setting the cross validation criteria
  trctrl <- trainControl(method = "cv", number = 2, savePredictions = 'all', classProbs = TRUE)
  
  # splitting data into training & test
  set.seed(101)
  #sample <- sample.int(n = nrow(dat), size = floor(.75*nrow(dat)), replace = F)
  #train <- dat[sample, ]
  #test  <- dat[-sample, ]
  
  # training the model
  svm_model <- train(apply(dat[,c(1:14)],2,as.numeric),dat$label, method = "svmRadial",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneGrid = grid2)
  
  # the model results
  pred <- predict(svm_model, newdata = apply(dat[,c(1:14)],2,as.numeric))
  
  # confusion matrix of the results
  conf <- confusionMatrix(pred, dat$label)
  
  return(list(conf = conf, pred = pred))
  
}

#SVM Functions (Group 5)
svm.func.2 <- function(dat){
  library(caret)
  
  # defining the radial grid with the two hyperparameters to be tuned
  grid2 <- expand.grid(sigma = c(0.25), C = c(2))
  
  # setting the cross validation criteria
  trctrl <- trainControl(method = "cv", number = 2, savePredictions = 'all', classProbs = TRUE)
  
  # splitting data into training & test
  set.seed(101)
  #sample <- sample.int(n = nrow(dat), size = floor(.75*nrow(dat)), replace = F)
  #train <- dat[sample, ]
  #test  <- dat[-sample, ]
  
  # training the model
  svm_model <- train(apply(dat[,c(1:14)],2,as.numeric),dat$label, method = "svmRadial",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneGrid = grid2)
  
  # the model results
  pred <- predict(svm_model, newdata = apply(dat[,c(1:14)],2,as.numeric))
  
  # confusion matrix of the results
  conf <- confusionMatrix(pred, dat$label)
  
  return(list(conf = conf, pred = pred))
  
}

# Splitting the dataset by groups
groupsplit <- split(dat,dat$group_num)

# Results
a <- svm.func(groupsplit$A)
b <- svm.func(groupsplit$B)
c <- svm.func(groupsplit$C)
d <- svm.func(groupsplit$D)
e <- svm.func.2(groupsplit$E)

#Saving the outputs
abcd <- rbind(matrix(a$pred),matrix(b$pred),matrix(c$pred),matrix(d$pred),matrix(e$pred))
abcd.list <- list(abcd)

# Saving the results
save(abcd, file = "../output/tempSvmLabels.RData")
save(abcd.list, file = "../output/listtempSvmLabels.RData")

confs <- cbind(a$conf$overall[1],b$conf$overall[1],c$conf$overall[1],d$conf$overall[1],e$conf$overall[1])
colnames(confs) <- c("Group 1","Group 2","Group 3","Group 4","Group 5")

save(confs, file = "../output/accuracy.RData")


detected_tokens <- cbind(rbind(matrix(groupsplit$A$error_token),matrix(groupsplit$B$error_token),
                               matrix(groupsplit$C$error_token),matrix(groupsplit$D$error_token),
                               matrix(groupsplit$E$error_token)), abcd)
colnames(detected_tokens) <- c("Tesseract Output","SVM Label")

save(detected_tokens, file = "./output/detected_tokens.RData")
