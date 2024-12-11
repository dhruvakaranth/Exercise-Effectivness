library(caret)
library(readxl)

##loading data
acc_data_train <- read.csv("C:\\Users\\dhruv\\Downloads\\pml-training.csv")
acc_data_test <- read.csv("C:\\Users\\dhruv\\Downloads\\pml-testing.csv")



## removing values with zero  variance
NZ <- nearZeroVar(acc_data)
NZ1 <- nearZeroVar(acc_data_test)
acc_data_train <- acc_data[,-NZ]
acc_data_test <- acc_data_test[,-NZ]


## Removing columns with NA

label <- apply(acc_data_train, 2, function(x) mean(is.na(x))) > 0.95
label1 <- apply(acc_data_test, 2, function(x) mean(is.na(x))) > 0.95
acc_data_train <- acc_data_train[, -which(label, label == FALSE)]
acc_data_test <- acc_data_test[, -which(label1, label == FALSE)]


## creating validation set
inTrain <- createDataPartition(y= acc_data_train$classe ,p=0.7, list=FALSE)
validate <- acc_data_train[-inTrain,]



## training dataset
modFit <- train(classe ~ roll_belt, data = acc_data_train , method = "rf")
modFit$finalModel

##validating dataset
pred <- predict(modFit,validate)
validate$rightpred <- pred == validate$classe

##plots
qplot(roll_belt,yaw_belt,colour=rightpred,data=validate,main="Validation Predictions")

##predicting
pred_test <- predict(modFit,acc_data_test)

