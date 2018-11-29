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