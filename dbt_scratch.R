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

dbt.readm <- dbt

#dbt.readm <- subset(dbt, readmitted=='<30' | readmitted=='NO')
summary(dbt.readm)

table(dbt$age)

# Delete unrelated / cols with significant NAs
dbt.readm[c('encounter_id', 'patient_nbr', 'weight', 'payer_code', 'examide', 'citoglipton', 'acetohexamide')] <- list(NULL)

dbt.readm$admission_type_id[dbt.readm$admission_type_id==1] <- 'Emergency'
dbt.readm$admission_type_id[dbt.readm$admission_type_id==2] <- 'Urgent'
dbt.readm$admission_type_id[dbt.readm$admission_type_id==3] <- 'Elective'
dbt.readm$admission_type_id[dbt.readm$admission_type_id==4] <- 'Newborn'
dbt.readm$admission_type_id[dbt.readm$admission_type_id==5] <- NA
dbt.readm$admission_type_id[dbt.readm$admission_type_id==6] <- NA
dbt.readm$admission_type_id[dbt.readm$admission_type_id==7] <- 'Trauma Center'
dbt.readm$admission_type_id[dbt.readm$admission_type_id==8] <- NA
dbt.readm$admission_type_id <- as.factor(dbt.readm$admission_type_id)
plot(dbt.readm$admission_type_id)
table(dbt.readm$admission_type_id)

dbt.readm$admission_source_id <- as.character(dbt.readm$admission_source_id)
dbt.readm$admission_source_id <- recode(dbt.readm$admission_source_id, "c(1,2,3) = 'Refer'")
dbt.readm$admission_source_id <- recode(dbt.readm$admission_source_id, "c(4,5,6,10,18,19,22,25,26) = 'Transfer'")
dbt.readm$admission_source_id[dbt.readm$admission_source_id== '7'] <- 'ER'
dbt.readm$admission_source_id[dbt.readm$admission_source_id== '8'] <- 'Court'
dbt.readm$admission_source_id <- recode(dbt.readm$admission_source_id, "c(9,15,17,20,21) = 'Unknown'")
dbt.readm$admission_source_id <- recode(dbt.readm$admission_source_id, "c(11,12,13,14,23,24) = 'Pediatric'")
dbt.readm$admission_source_id <- as.factor(dbt.readm$admission_source_id)
plot(dbt.readm$admission_source_id)
table(dbt.readm$admission_source_id)


######
dbt.readm$discharge_disposition_id <- as.character(dbt.readm$discharge_disposition_id)
dbt.readm$discharge_disposition_id[dbt.readm$discharge_disposition_id==1] <- 'Home'
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id,
                                             " c('2','3','4','5','6','8',
                                             '9','10','12','15','16','17',
                                             '22','23','24','25','30','27',
                                             '28','29') = 'Other Facility' ")
dbt.readm$discharge_disposition_id <- recode(dbt.readm$discharge_disposition_id,
                                             " c('7','11','13','14','18','19',
                                             '20','21','26') = NA ")

dbt.readm$discharge_disposition_id <- as.factor(dbt.readm$discharge_disposition_id)
plot(dbt.readm$discharge_disposition_id)
table(dbt.readm$discharge_disposition_id)

### Re-grouping medical specialty ###
mydata$medical_specialty <- as.character(mydata$medical_specialty)
mydata$medical_specialty[mydata$medical_specialty == "AllergyandImmunology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Cardiology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Dermatology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Endocrinology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Endocrinology-Metabolism"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Gastroenterology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Hematology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Hematology/Oncology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "InfectiousDiseases"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "InternalMedicine"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Nephrology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Neurology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Oncology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Pulmonology"] <- "Medicine"
mydata$medical_specialty[mydata$medical_specialty == "Rheumatology"] <- "Medicine"

mydata$medical_specialty[mydata$medical_specialty == "Orthopedics"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Orthopedics-Reconstructive"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgeon"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-Cardiovascular"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-Cardiovascular/Thoracic"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-Colon&Rectal"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-General"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-Maxillofacial"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-Neuro"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-Pediatric"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-Plastic"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-PlasticwithinHeadandNeck"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-Thoracic"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Surgery-Vascular"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "SurgicalSpecialty"] <- "Surgery"
mydata$medical_specialty[mydata$medical_specialty == "Urology"] <- "Surgery"

mydata$medical_specialty[mydata$medical_specialty == "Anesthesiology-Pediatric"] <- "Pediatrics"
mydata$medical_specialty[mydata$medical_specialty == "Cardiology-Pediatric"] <- "Pediatrics"
mydata$medical_specialty[mydata$medical_specialty == "Pediatrics-AllergyandImmunology"] <- "Pediatrics"
mydata$medical_specialty[mydata$medical_specialty == "Pediatrics-CriticalCare"] <- "Pediatrics"
mydata$medical_specialty[mydata$medical_specialty == "Pediatrics-EmergencyMedicine"] <- "Pediatrics"
mydata$medical_specialty[mydata$medical_specialty == "Pediatrics-Endocrinology"] <- "Pediatrics"
mydata$medical_specialty[mydata$medical_specialty == "Pediatrics-Hematology-Oncology "] <- "Pediatrics"
mydata$medical_specialty[mydata$medical_specialty == "Pediatrics-InfectiousDiseases"] <- "Pediatrics"
mydata$medical_specialty[mydata$medical_specialty == "Pediatrics-Neurology"] <- "Pediatrics"
mydata$medical_specialty[mydata$medical_specialty == "Pediatrics-Pulmonology"] <- "Pediatrics"
mydata$medical_specialty[mydata$medical_specialty == "Pediatrics-Hematology-Oncology"] <- "Pediatrics"

mydata$medical_specialty[mydata$medical_specialty == "Gynecology"] <- "OB-GYN"
mydata$medical_specialty[mydata$medical_specialty == "Obsterics&Gynecology-GynecologicOnco"] <- "OB-GYN"
mydata$medical_specialty[mydata$medical_specialty == "Obstetrics"] <- "OB-GYN"              
mydata$medical_specialty[mydata$medical_specialty == "ObstetricsandGynecology"] <- "OB-GYN"  

mydata$medical_specialty[mydata$medical_specialty == "Anesthesiology"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "Dentistry"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "DCPTEAM"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "Hospitalist"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "Neurophysiology"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "Ophthalmology"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "Osteopath"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "Otolaryngology"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "OutreachServices"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "Pathology"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "Perinatology"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "PhysicalMedicineandRehabilitation"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "PhysicianNotFound"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "Podiatry"] <- "Other"
mydata$medical_specialty[mydata$medical_specialty == "Proctology"] <- "Other"                       
mydata$medical_specialty[mydata$medical_specialty == "Psychiatry"] <- "Other"                              
mydata$medical_specialty[mydata$medical_specialty == "Psychiatry-Addictive"] <- "Other" 
mydata$medical_specialty[mydata$medical_specialty == "Psychiatry-Child/Adolescent"] <- "Other" 
mydata$medical_specialty[mydata$medical_specialty == "Psychology"] <- "Other"  
mydata$medical_specialty[mydata$medical_specialty == "Radiologist"] <- "Other"  
mydata$medical_specialty[mydata$medical_specialty == "Radiology"] <- "Other"  
mydata$medical_specialty[mydata$medical_specialty == "Resident"] <- "Other"  
mydata$medical_specialty[mydata$medical_specialty == "Speech"] <- "Other"  
mydata$medical_specialty[mydata$medical_specialty == "SportsMedicine"] <- "Other"  
                                    

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

#Split Data 80/20
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