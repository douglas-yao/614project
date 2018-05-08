mydata <- read.csv("Desktop/HS614/diabetic_data.csv", na.strings = c("?", NA))
set.seed(825)

require(caret)
require(ranger)
require(ROCR)
require(pROC)
require(stringr)
require(rebus)
require(klaR)
require(ggplot2)
require(Rmisc)
require(plyr)
require(RColorBrewer)
require(factoextra)
require(car)

### Drop level ###
remove <- which(mydata$gender == "Unknown/Invalid") 
mydata <- mydata[-remove,] 
mydata$gender <- factor(mydata$gender)
summary(mydata$gender)

### Age group 50 ###
mydata$new_age50 <- mydata$age
mydata$new_age50 <- as.character(mydata$age)
mydata$new_age50[mydata$new_age50=="[0-10)"] <- "<50"
mydata$new_age50[mydata$new_age50=="[10-20)"] <- "<50"
mydata$new_age50[mydata$new_age50=="[20-30)"] <- "<50"
mydata$new_age50[mydata$new_age50=="[30-40)"] <- "<50"
mydata$new_age50[mydata$new_age50=="[40-50)"] <- "<50"
mydata$new_age50[mydata$new_age50=="[50-60)"] <- ">50"
mydata$new_age50[mydata$new_age50=="[60-70)"] <- ">50"
mydata$new_age50[mydata$new_age50=="[70-80)"] <- ">50"
mydata$new_age50[mydata$new_age50=="[80-90)"] <- ">50"
mydata$new_age50[mydata$new_age50=="[90-100)"] <- ">50"
mydata$new_age50 <- as.factor(mydata$new_age50)
summary(mydata$new_age50)

### Change readmitted class ###
mydata$new_readmit <- mydata$readmitted
mydata$new_readmit <- as.character(mydata$new_readmit)
mydata$new_readmit[mydata$new_readmit == "NO"] <- "No"
mydata$new_readmit[mydata$new_readmit == "<30"] <- "Yes"
mydata$new_readmit[mydata$new_readmit == ">30"] <- "Yes"
mydata$new_readmit <- as.factor(mydata$new_readmit)
summary(mydata$new_readmit)

### Re-grouping diagnosis 1 ###
mydata$diag_1 <- as.character(mydata$diag_1)
mydata$diag_1[str_detect(mydata$diag_1, pattern = START %R% or("39","40","41","42","43","44","45","785")) == T] <- "Circulatory"
mydata$diag_1[str_detect(mydata$diag_1, pattern = START %R% or("52","53","54","55","56","57","787")) == T] <- "Digestive"
mydata$diag_1[str_detect(mydata$diag_1, pattern = START %R% or("58","59","60","61","62","788")) == T] <- "Genitourinary"
mydata$diag_1[str_detect(mydata$diag_1, pattern = START %R% "25") == T] <- "Diabetes"
mydata$diag_1[str_detect(mydata$diag_1, pattern = START %R% or("80","81","82","83","84","85",
                                                               "86","87","88","89","90","91",
                                                               "92","93","94","95","96","97",
                                                               "98","99")) == T] <- "Injury"
mydata$diag_1[str_detect(mydata$diag_1, pattern = START %R% or("71","72","73")) == T] <- "Musculoskeletal"
mydata$diag_1[str_detect(mydata$diag_1, pattern = START %R% or("14","15","16","17","18","19",
                                                               "20","21","22","23")) == T] <- "Neoplasms"
mydata$diag_1[str_detect(mydata$diag_1, pattern = START %R% or("46","47","48","49","50","51","786")) == T] <- "Respiratory"
mydata$diag_1[str_detect(mydata$diag_1, pattern = "[[:digit:]]") == T] <- "Other"
mydata$diag_1 <- as.factor(mydata$diag_1)
summary(mydata$diag_1)

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

mydata$medical_specialty[is.na(mydata$medical_specialty)] <- "Missing/Unknown"  

mydata$medical_specialty <- as.factor(mydata$medical_specialty)
summary(mydata$medical_specialty)

##Re-grouping admission type
mydata$admission_type_id <- as.character(mydata$admission_type_id)
mydata$admission_type_id[mydata$admission_type_id==1] <- 'ER'
mydata$admission_type_id[mydata$admission_type_id==2] <- 'ER'
mydata$admission_type_id[mydata$admission_type_id==3] <- 'Elective'
mydata$admission_type_id[mydata$admission_type_id==4] <- 'Newborn'
mydata$admission_type_id[mydata$admission_type_id==5] <- 'Unknown'
mydata$admission_type_id[mydata$admission_type_id==6] <- 'Unknown'
mydata$admission_type_id[mydata$admission_type_id==7] <- 'Trauma Center'
mydata$admission_type_id[mydata$admission_type_id==8] <- 'Unknown'
mydata$admission_type_id <- as.factor(mydata$admission_type_id)

##Re-grouping admission type
mydata$admission_source_id <- as.character(mydata$admission_source_id)
mydata$admission_source_id <- recode(mydata$admission_source_id, "c(1,2,3) = 'Refer'")
mydata$admission_source_id <- recode(mydata$admission_source_id, "c(4,5,6,10,18,19,22,25,26) = 'Transfer'")
mydata$admission_source_id[mydata$admission_source_id== "7"] <- "ER"
mydata$admission_source_id[mydata$admission_source_id== "8"] <- "Court"
mydata$admission_source_id <- recode(mydata$admission_source_id, "c(9,15,17,20,21) = 'Unknown'")
mydata$admission_source_id <- recode(mydata$admission_source_id, "c(11,12,13,14,23,24) = 'Pediatric'")
mydata$admission_source_id <- as.factor(mydata$admission_source_id)

##Re-grouping discharge id
mydata$discharge_disposition_id <- as.character(mydata$discharge_disposition_id)
mydata$discharge_disposition_id[mydata$discharge_disposition_id==1] <- 'Home'
mydata$discharge_disposition_id <- recode(mydata$discharge_disposition_id, "c('2','5','28')='Inpatient' ")
mydata$discharge_disposition_id <- recode(mydata$discharge_disposition_id, "c('3','4','16','17','22','23','24')='Outpatient' ")
mydata$discharge_disposition_id <- recode(mydata$discharge_disposition_id, "c('6','8')='Home Health' ")
mydata$discharge_disposition_id <- recode(mydata$discharge_disposition_id, "c('7')='AMA' ")
mydata$discharge_disposition_id <- recode(mydata$discharge_disposition_id, "c('11','19','20','21')='Expired' ")
mydata$discharge_disposition_id <- recode(mydata$discharge_disposition_id, "c('12','15')='Same Institute' ")
mydata$discharge_disposition_id <- recode(mydata$discharge_disposition_id, "c('13','14')='Hospice' ")
mydata$discharge_disposition_id <- recode(mydata$discharge_disposition_id, "c('27','29','30')='' ")
mydata$discharge_disposition_id <- recode(mydata$discharge_disposition_id, "c('9','10','18','25','26')= 'Unknown'")
mydata$discharge_disposition_id <- recode(mydata$discharge_disposition_id, "c('2','5','28')='Inpatient' ")
mydata$discharge_disposition_id <- as.factor(mydata$discharge_disposition_id)

### Clean data ###
delete = c("encounter_id", "patient_nbr", "weight", "payer_code", "diag_2",
           "diag_3","readmitted", "age", "examide", "citoglipton", "metformin.rosiglitazone") 
#Last three have single level
mydata <- mydata[, !(names(mydata) %in% delete)] 
names(mydata)

### Set level ###
levels(mydata$max_glu_serum) <- c("None", "Norm", ">200", ">300")
summary(mydata$max_glu_serum)
levels(mydata$A1Cresult) <- c("None", "Norm", ">7", ">8")
summary(mydata$A1Cresult)
levels(mydata$change) <- c("No", "Ch")
summary(mydata$change)

### Data visualization ###
plot(mydata$new_readmit)
plot(mydata$new_age50)

g <- ggplot(data=mydata)
g + geom_boxplot(data=mydata, aes(y=time_in_hospital, x=new_readmit, 
                                  fill=new_readmit), alpha =.5) +
  xlab("Readmitted") +
  ylab("Time in hospital (days)") +
  ggtitle("Box plot")

g + geom_boxplot(data=mydata, aes(y=number_diagnoses, x=new_readmit, 
                                  fill=new_readmit), alpha =.5) +
  xlab("Readmitted") +
  ylab("Number of diagnoses") +
  ggtitle("Box plot")

par(mfrow=c(1, 1), mar=c(5, 5, 4, 4))
coul = brewer.pal(8, "Pastel2")
counts <- 100*prop.table(table(mydata$new_readmit, mydata$medical_specialty),2)
barplot(counts, col=coul, main = "Medical specialty vs readmitted",
        xlab = "Medical specialty", ylab = "Percentage of readmitted", space = 1,
        legend.text=TRUE,
        args.legend=list(x = "topright", bty = "n", inset = c(-0.15,0)))

### Split the data ###
index <- createDataPartition(y = mydata$new_readmit, p=0.7, list = FALSE)
train.data <- mydata[index,] # 70% data goes in here
write.csv(train.data, file = "Desktop/HS614/TrainDataDM.csv")

test.data<- mydata[-index,] # 30% data goes in here
write.csv(test.data, file = "Desktop/HS614/TestDataDM.csv")

### Normalization ###
preProcValues <- preProcess(train.data, method = c("center","scale"))
trainTransformed <- predict(preProcValues, train.data)
testTransformed <- predict(preProcValues, test.data)

### Remove NA ###
trainTransformed  <- na.omit(trainTransformed)
testTransformed <- na.omit(testTransformed)

### Clustering ###
dat <- subset(trainTransformed, select = -new_readmit)
number_col <- names(dat) %in% c("time_in_hospital", "num_lab_procedures", "num_medications", "num_procedures", "number_diagnoses", "number_emergency", "number_inpatient", "number_outpatient")
cat_dat <- dat[!number_col]
num_dat <- dat[number_col]

##K-means Clustering
fit.km_num <- kmeans(num_dat, 2, nstart=20)
table(fit.km_num$cluster, trainTransformed$new_readmit)
fviz_cluster(fit.km_num, data = num_dat,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot")

#Hierarchical Clustering
Sample = sample(nrow(num_dat), 100) 
d <- dist(as.matrix(num_dat[Sample,]))
hc <- hclust(d, method="ward.D2")
plot(hc)

res <- hcut(d, k = 2, stand = TRUE)
fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF"))

### Plot a histogram ###
not_include <- names(trainTransformed) %in% c("time_in_hospital", "num_lab_procedures",
                                              "num_medications", "num_procedures", "number_diagnoses",
                                              "number_emergency", "number_inpatient", "number_outpatient") 
histo <- trainTransformed[!not_include]
readmit <- histo[which(histo$new_readmit=='Yes'),]
no_readmit <- histo[which(histo$new_readmit=='No'),]

for (i in 1:(ncol(readmit)-1))
  local({
    i <- i
    g1 <- ggplot(data=readmit)
    g2 <- ggplot(data=no_readmit)
    p1 <- g1 + geom_bar(aes(readmit[,i], fill=readmit[,i])) +
      xlab(colnames(readmit[i])) +
      ylab("Number of patients") +
      ggtitle("Readmitted") +
      scale_fill_discrete(name=colnames(readmit[i]))
    p2 <- g2 + geom_bar(aes(no_readmit[,i], fill=no_readmit[,i])) +
      xlab(colnames(no_readmit[i])) +
      ylab("Number of patients") +
      ggtitle("Non-readmitted") +
      scale_fill_discrete(name=colnames(no_readmit[i]))
    multiplot(p1, p2, cols=2)
  })

### Random Forest ###
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats=3, 
                           savePredictions="final", classProbs=TRUE, search="grid")
tunegrid <- expand.grid(.mtry=c(1:15),
                        .splitrule = "gini",
                        .min.node.size = c(10, 20))
rf_fit <- train(new_readmit ~ ., data = trainTransformed,
                method = "ranger", metric= "Accuracy", tuneGrid=tunegrid,
                trControl = fitControl)
print(rf_fit)
plot(rf_fit)

rf_pred <- predict(rf_fit, testTransformed, type='raw') ###Accuracy : 
confusionMatrix(rf_pred,testTransformed$new_readmit)  

rf_pred2 <- predict(rf_fit, testTransformed, type='prob')
plot(roc(testTransformed$new_readmit, rf_pred2[,2]), print.auc = TRUE, col = "blue", asp = NA, legacy.axes = TRUE)

F1 <- result$byClass[7]
print(F1)
