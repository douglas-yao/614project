dbt <- read.csv('diabetic_data.csv')

names(dbt)
head(dbt)
summary(dbt)

library(caret)
library(naivebayes)
library(e1071)
library(randomForest)

set.seed(1234)

dbt[dbt=='?'] <- NA

# Delete unrelated / cols with significant NAs
dbt[c('encounter_id', 'patient_nbr', 'weight', 'payer_code', 'examide', 'citoglipton')] <- list(NULL)

#dbt$readmitted <- as.character(dbt$readmitted)

#dbt$readmitted[dbt$readmitted=='<30'] <- 'YES'
#dbt$readmitted[dbt$readmitted=='>30'] <- 'YES'

# Omit rows with unknown gender.
dbt <- dbt[- 30507,]
dbt <- dbt[- 75552,]
dbt <- dbt[- 82574,]

dbt <- na.omit(dbt)

trainIndex <- createDataPartition(dbt$readmitted, times=1, p=.8, list=FALSE)

dbt_train <- dbt[trainIndex,]
dbt_test <- dbt[-trainIndex,]

###--------------------------------------------------------------------------------------------###

fitcontrol <- trainControl(method='repeatedcv', number=5, repeats=3)

# Random Forest
seed <- 123
metric <- 'Accuracy'
set.seed(seed)
mtry <- 7
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(readmitted~., 
                    data=dbt_train, 
                    method='rf', 
                    metric=metric,
                    tuneGrid=tunegrid,
                    trControl=fitcontrol,
                    na.action=na.omit)
print(rf_default)

#Logistic Regression

gbm_fit <- train(new_readmit ~ ., data = dbt_train,
                 method='gbm',
                 trControl=fitControl,  
                 metric = 'ROC',
                 tuneLength=10,
                 preProc = c('center', 'scale'))
summary(gbm_fit)
print(gbm_fit)
plot(gbm_fit)

gbm_pred <- predict(gbm_fit, newdata = dbt_test, type='raw')
confusionMatrix(gbm_pred, dbt_test$readmitted)    ###Accuracy : 0.6255

#probabilites 
gbm_pred2 <- predict(gbm_fit, newdata = dbt_test, type='prob')
plot(roc(dbt_test$readmitted, gbm_pred2[,2]), print.auc = TRUE, col = 'blue')

#SVM

svm_control <- trainControl(method='repeatedcv', number=10, repeats=3)
model <- train(readmitted~., data=dbt_train, method='linear')

dbt_svm <- train(readmitted~., data = dbt_train, trControl = rf_control, method = 'svmLinear', preProcess = c("center", "scale"), tuneLength = 10)
SVM_grid <-expand.grid(method = c('svmLinear', 'svmPoly','svmRadial'))
SVM_model <- train(readmitted~., data = dbt_train, trControl = rf_control, tuneGrid = SVM_grid, preProcess = c("center", "scale"), tuneLength = 10)
SVM_pred <- predict(SVM_model, newdata = dbt_test)
confusionMatrix(SVM_pred, dbt_train$readmitted)
