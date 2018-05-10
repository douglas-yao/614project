

choose.dir()

dbt <- read.csv('diabetic_data.csv')

names(dbt)
head(dbt)
summary(dbt)

library(caret) 
library(e1071)
library(ranger)
library(klaR)
library(pROC)
library(ROCR)
library(factoextra)
library(car)
library(stringr)
library(ggplot2)

set.seed(1234)

#Replace all '?' values as NA
dbt[dbt=='?'] <- NA

#Subset for pts readmitted under 30 days or not readmitted at all.
dbt.readm <- subset(dbt, readmitted=='<30' | readmitted=='NO')
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
dbt.readm$medical_specialty <- as.character(mydata$medical_specialty)
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "AllergyandImmunology"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Dermatology"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Endocrinology"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Endocrinology-Metabolism"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Gastroenterology"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Hematology"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Hematology/Oncology"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "InfectiousDiseases"] <- "Medicine"
dbt.readm$medical_specialty[mdbt.readm$medical_specialty == "InternalMedicine"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Nephrology"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Neurology"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Oncology"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pulmonology"] <- "Medicine"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Rheumatology"] <- "Medicine"

dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Orthopedics"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Orthopedics-Reconstructive"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgeon"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-Cardiovascular"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-Cardiovascular/Thoracic"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-Colon&Rectal"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-General"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-Maxillofacial"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-Neuro"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-Pediatric"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-Plastic"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-PlasticwithinHeadandNeck"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-Thoracic"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Surgery-Vascular"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "SurgicalSpecialty"] <- "Surgery"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Urology"] <- "Surgery"

dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Anesthesiology-Pediatric"] <- "Pediatrics"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Cardiology-Pediatric"] <- "Pediatrics"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pediatrics-AllergyandImmunology"] <- "Pediatrics"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pediatrics-CriticalCare"] <- "Pediatrics"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pediatrics-EmergencyMedicine"] <- "Pediatrics"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pediatrics-Endocrinology"] <- "Pediatrics"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pediatrics-Hematology-Oncology "] <- "Pediatrics"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pediatrics-InfectiousDiseases"] <- "Pediatrics"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pediatrics-Neurology"] <- "Pediatrics"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pediatrics-Pulmonology"] <- "Pediatrics"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pediatrics-Hematology-Oncology"] <- "Pediatrics"

dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Gynecology"] <- "OB-GYN"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Obsterics&Gynecology-GynecologicOnco"] <- "OB-GYN"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Obstetrics"] <- "OB-GYN"              
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "ObstetricsandGynecology"] <- "OB-GYN"  

dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Anesthesiology"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Dentistry"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "DCPTEAM"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Hospitalist"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Neurophysiology"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Ophthalmology"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Osteopath"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Otolaryngology"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "OutreachServices"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Pathology"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Perinatology"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "PhysicalMedicineandRehabilitation"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "PhysicianNotFound"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Podiatry"] <- "Other"
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Proctology"] <- "Other"                       
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Psychiatry"] <- "Other"                              
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Psychiatry-Addictive"] <- "Other" 
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Psychiatry-Child/Adolescent"] <- "Other" 
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Psychology"] <- "Other"  
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Radiologist"] <- "Other"  
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Radiology"] <- "Other"  
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Resident"] <- "Other"  
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "Speech"] <- "Other"  
dbt.readm$medical_specialty[dbt.readm$medical_specialty == "SportsMedicine"] <- "Other"  
                                    

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