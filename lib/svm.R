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


#SVM Functions
svm.func <- function(dat){
  library(caret)
  
  # defining the radial grid with the two hyperparameters to be tuned
  grid_radial <- expand.grid(sigma = c(0.25, 1), C = c(1,10))
  
  # setting the cross validation criteria
  trctrl <- trainControl(method = "cv", number = 3, savePredictions = 'all', classProbs = TRUE)
  
  # splitting data into training & test
  set.seed(101)
  #sample <- sample.int(n = nrow(dat), size = floor(.75*nrow(dat)), replace = F)
  #train <- dat[sample, ]
  #test  <- dat[-sample, ]
  
  # training the model
  svm_model <- train(apply(dat[,c(1:14)],2,as.numeric),dat$label, method = "svmRadial",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneGrid = grid_radial)
  
  # the model results
  pred <- predict(svm_model, newdata = apply(dat[,c(1:14)],2,as.numeric))
  
  # probabilities
  predictedProbs <- predict(svm_model, apply(dat[,c(1:14)],2,as.numeric), type = "prob")
  
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
e <- svm.func(groupsplit$E)




