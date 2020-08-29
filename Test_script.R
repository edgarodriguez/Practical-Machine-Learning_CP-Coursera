training_data_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing_data_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training_data <- read.csv(url(training_data_url ))
testing_data <- read.csv(url(testing_data_url ))

head(training_data)
dim(training_data)
dim(testing_data)

nzv <- nearZeroVar(training_data)
train_data <- training_data[,-nzv]
test_data <- testing_data[,-nzv]
#omitting na values columns
na_value_column <- sapply(train_data, function(x) mean(is.na(x))) > 0.95
train_data <- train_data[,na_value_column == FALSE]
test_data <- test_data[,na_value_column == FALSE]

set.seed(123)
inTrain <- createDataPartition(y=train_data$classe, p=0.6,list=FALSE)
Training <- train_data[inTrain,]
Testing <- train_data[-inTrain,]
fitC <- trainControl(method = "cv",number = 10)


###
gbm_model<- train(classe ~. , data=Training, method= "gbm",
                  n.trees = 10,interaction.depth = 6,
                  n.minobsinnode = 3,verbose=FALSE)

rf_model<- train(classe ~. , data=Training, method= "rf",trControl=fitC,verbose=FALSE)
gbm_prediction<- predict(gbm_model, Testing)
rf_prediction<- predict(rf_model, Testing)
gbm_cm<-confusionMatrix(table(gbm_prediction, Testing$classe))$overall[1]
rf_cm<-confusionMatrix(table(rf_prediction, Testing$classe))$overall[1]

acc <- c(gbm_cm,rf_cm)
names(acc) <- c("Gradient Boosting","Random Forest")
acc

predDF <- data.frame(gbm_prediction, rf_prediction, classe = Testing$classe)
combMod_model <- train(classe ~ ., method = "rf", data = predDF)
comb_prediction <- predict(combMod_model, predDF)
cM_cm<-confusionMatrix(table(comb_prediction, Testing$classe))$overall[1]
resume <- c(gbm_cm,rf_cm,cM_cm)

acc <- c(gbm_cm,rf_cm)
names(acc) <- c("Gradient Boosting","Random Forest")
