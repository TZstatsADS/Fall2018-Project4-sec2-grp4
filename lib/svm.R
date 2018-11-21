library(caret)
library(doMC)
registerDoMC()

# defining the radial grid with the two hyperparameters to be tuned
grid_radial <- expand.grid(sigma = c(0, 0.01, 0.1, 0.25, 1),
                           C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))
# setting the cross validation criteria
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

# combining for one dataset
data <- cbind(feature_dat,as.factor(label_dat$label))

# splitting data into training & test
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

# training the model
svm_model <- train(train[,-15],train[,15], method = "svmRadial",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid_radial,
                           tuneLength = 5)

# the model results
svm_model

# confusion matrix of the results
confusionMatrix(svm_model)



