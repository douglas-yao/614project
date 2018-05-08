dbt <- read.csv('diabetic_data.csv')

names(dbt)
head(dbt)
summary(dbt)

library(caret) 
library(naivebayes)
library(e1071)
library(randomForest)
library(car)
library(stringr)

set.seed(1234)

dbt[dbt=='?'] <- NA

dbt.readm <- subset(dbt, readmitted=='<30' | readmitted=='NO')
summary(dbt.readm)

dbt.readm$admission_type_id[dbt.readm$admission_type_id==1] <- 'Emergency'
dbt.readm$admission_type_id[dbt.readm$admission_type_id==2] <- 'Urgent'
dbt.readm$admission_type_id[dbt.readm$admission_type_id==3] <- 'Elective'
dbt.readm$admission_type_id[dbt.readm$admission_type_id==4] <- 'Newborn'
dbt.readm$admission_type_id[dbt.readm$admission_type_id==5] <- NA
dbt.readm$admission_type_id[dbt.readm$admission_type_id==6] <- NA
dbt.readm$admission_type_id[dbt.readm$admission_type_id==7] <- 'Trauma Center'
dbt.readm$admission_type_id[dbt.readm$admission_type_id==8] <- NA

dbt.readm$discharge_disposition_id <- as.character(dbt.readm$discharge_disposition_id)

dbt.readm$discharge_disposition_id[dbt.readm$discharge_disposition_id==1] <- 'Home'
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id, "c('2','5','28')='Inpatient' ")
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id, "c('3','4','16','17','22','23','24')='Outpatient' ")
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id, "c('6','8')='Home Health' ")
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id, "c('7')='AMA' ")
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id, "c('11','19','20','21')='Expired' ")
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id, "c('12','15')='Same Institute' ")
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id, "c('13','14')='Hospice' ")
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id, "c('27','29','30')='' ")
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id, "c('9','10','18','25','26')=NA ")

dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id, "c('2','5','28')='Inpatient' ")

table(dbt.readm$discharge_disposition_id)

for (item in dbt.readm$discharge_disposition_id){
  if (item == c(2,5,28)){
    item <- 'Inpatient Facility'
  } else {
  NULL
  }
}
                                    
table(dbt.readm$discharge_disposition_id)
                                    
# Delete unrelated / cols with significant NAs
dbt.readm[c('encounter_id', 'patient_nbr', 'weight', 'payer_code', 'examide', 'citoglipton')] <- list(NULL)

#dbt$readmitted <- as.character(dbt$readmitted)

#dbt$readmitted[dbt$readmitted=='<30'] <- 'YES'
#dbt$readmitted[dbt$readmitted=='>30'] <- 'YES'

# Omit rows with unknown gender.
dbt.readm <- dbt.readm[- 30507,]
dbt.readm <- dbt.readm[- 75552,]
dbt.readm <- dbt.readm[- 82574,]

dbt.readm <- na.omit(dbt.readm)

# Clustering

hc.complete = hclust(dist(dbt.readm), method="complete")

par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=.9)

cutree(hc.complete, 2)

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
confusionMatrix(gbm_pred, dbt_test$readmitted)

#probabilites 
gbm_pred2 <- predict(gbm_fit, newdata = dbt_test, type='prob')
plot(roc(dbt_test$readmitted, gbm_pred2[,2]), print.auc = TRUE, col = 'blue')

#SVM

svm_control <- trainControl(method='repeatedcv', number=10, repeats=3)
model <- train(readmitted~., data=dbt_train, method='svmLinear')

dbt_svm <- train(readmitted~., data = dbt_train, trControl = rf_control, method = 'svmLinear', preProcess = c("center", "scale"), tuneLength = 10)
SVM_grid <-expand.grid(method = c('svmLinear', 'svmPoly','svmRadial'))
SVM_model <- train(readmitted~., data = dbt_train, trControl = rf_control, tuneGrid = SVM_grid, preProcess = c("center", "scale"), tuneLength = 10)
SVM_pred <- predict(SVM_model, newdata = dbt_test)
confusionMatrix(SVM_pred, dbt_train$readmitted)