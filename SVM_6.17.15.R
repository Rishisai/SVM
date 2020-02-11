rm(list = ls())
library(caret)
set.seed(2018)
url<-c("http://dataaspirant.com/wp-content/uploads/2017/01/heart_tidy.csv")

Mydata<-read.csv(url,sep=',', header = FALSE)
# There is a pdf file on the Blackboard portal that contains the description of variables
# V14 is the heart Heart Disease Flag 

#Creating test and train datasets
intrain <- createDataPartition(y = Mydata$V14, p= 0.7, list = FALSE)
Mydata$V14<-as.factor(Mydata$V14)
training <- Mydata[intrain,]
testing <- Mydata[-intrain,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(2018)
svm_Linear <- train(V14 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

test_pred <- predict(svm_Linear, newdata = testing)
confusionMatrix(test_pred, testing$V14 )

set.seed(2018)
grid <- expand.grid(C = c(0.005, 0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 2,5))
svm_Linear_Grid <- train(V14 ~., data = training, method = "svmLinear",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)

svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred <- predict(svm_Linear_Grid, newdata = testing)
confusionMatrix(test_pred, testing$V14)

set.seed(2018)
svm_Radial <- train(V14 ~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Radial
plot(svm_Radial)

test_pred_Radial <- predict(svm_Radial, newdata = testing)
confusionMatrix(test_pred_Radial, testing$V14 )
###################################################################
#Let us try decision trees 
library(rpart)
set.seed(2018)
rpart_model <- train(V14 ~., data = training, method = "rpart",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
rpart_model

test_pred_rpart <- predict(rpart_model, newdata = testing)
confusionMatrix(test_pred_rpart, testing$V14)

set.seed(2018)
rf_model <- train(V14 ~., data = training, method = "rf",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10)

test_pred_rf <- predict(rf_model, newdata = testing)
confusionMatrix(test_pred_rf, testing$V14)

