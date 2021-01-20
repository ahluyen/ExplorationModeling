install.packages("rpart.plot")
library(ggplot2)
library (dplyr)
library(caret)
library(rpart.plot)
library(e1071)
library(randomForest)
library(ISLR)
library(pscl)

setwd("/Users/Andy/Documents/USFSpring2019/HS614/FinalProject")

mydf_original <- read.csv("resp_renal_final.csv", header =T, stringsAsFactors = F)

mydf <- mydf_original

summary(mydf)
colnames(mydf)

##### PART 1: EDA #####

#age_yrs

summary(mydf$age_yrs)
class(mydf$age_yrs)
anyNA(mydf$age_yrs)
hist(mydf$age_yrs)

a <- which(mydf$age_yrs >= 90)
length(a)
mydf$age_yrs[mydf$age_yrs >= 90] <- 90
summary(mydf$age_yrs)

#creating a new variable new: age_level for visualization (all patient with age greater than 90 will be 90 and +)

summary(mydf$age_yrs)

lev <- c('18-20', '20-30',  '30-40',  '40-50', '50-60',  '60-70',  '70-80', '80-90', '90.and.+')

mydf$age_level <- factor(rep(NA, nrow(mydf)), ordered = T, levels = lev)

mydf$age_level[mydf$age_yrs < 20] <- "18-20" 
mydf$age_level[mydf$age_yrs >= 20 & mydf$age_yrs < 30] <- "20-30" 
mydf$age_level[mydf$age_yrs >= 30 & mydf$age_yrs < 40] <- "30-40" 
mydf$age_level[mydf$age_yrs >= 40 & mydf$age_yrs < 50] <- "40-50"
mydf$age_level[mydf$age_yrs >= 50 & mydf$age_yrs < 60] <- "50-60" 
mydf$age_level[mydf$age_yrs >= 60 & mydf$age_yrs < 70] <- "60-70" 
mydf$age_level[mydf$age_yrs >= 70 & mydf$age_yrs < 80] <- "70-80" 
mydf$age_level[mydf$age_yrs >= 80 & mydf$age_yrs < 90] <- "80-90" 
mydf$age_level[mydf$age_yrs >= 90] <- "90.and.+" 

mydf$age_level <- as.factor(mydf$age_level)

summary(mydf$age_level)
class(mydf$age_level)
plot(mydf$age_level)

#create new variable: total meds (???)

#gender: female = 0, male = 1

summary(mydf$gender)
class(mydf$gender)

#mydf$gender[mydf$gender == 'F'] <- 'Female'
#mydf$gender[mydf$gender == 'M'] <- 'Male'

mydf$gender <- as.factor(mydf$gender)
summary(mydf$gender)

#hos_stay:

summary(mydf$hosp_stay)
class(mydf$hosp_stay)
anyNA(mydf$hosp_stay)
hist(mydf$hosp_stay)

#change class to binary variable: 0 - resp, 1 - renal

summary(mydf$class)
class(mydf$class)

mydf$class[mydf$class == 'RESP'] <- '0'
mydf$class[mydf$class == 'Renal'] <- '1'

mydf$class <- as.factor(mydf$class)
summary(mydf$class)

#typecast all prescription to binary

summary(mydf$diuretic)
mydf$diuretic <- as.factor(mydf$diuretic)

summary(mydf$albut)
mydf$albut <- as.factor(mydf$albut)

summary(mydf$advair)
mydf$advair <- as.factor(mydf$advair)

summary(mydf$budesonide)
mydf$budesonide <- as.factor(mydf$budesonide)

summary(mydf$losartan)
mydf$losartan <- as.factor(mydf$losartan)

summary(mydf$montelukast)
mydf$montelukast <- as.factor(mydf$montelukast)

#o2: consider not using o2 due to high missing values 

summary(mydf$o2)
mydf$o2 <- as.numeric(mydf$o2)
class(mydf$o2)
anyNA(mydf$o2)

#min_plat: 

summary(mydf$min_plat)
class(mydf$min_plat)
anyNA(mydf$min_plat)

#creati2

summary(mydf$creati2)
mydf$creati2 <- as.numeric(mydf$creati2)
b <- which(mydf$creati2 >= 99)
length(b)
mydf$creati2[mydf$creati2 >= 99] <- NA
summary(mydf$creati2)

#insurance

mydf$insurance <- as.factor(mydf$insurance)
summary(mydf$insurance)

#collapse some ethnicity into some ethnicities group: Black, Asian, White, Hispanic, and Other

summary(mydf$ethnicity)
class(mydf$ethnicity)

ethnicity_collapse <- which(mydf$ethnicity=="AMERICAN INDIAN/ALASKA NATIVE" 
                     | mydf$ethnicity=="AMERICAN INDIAN/ALASKA NATIVE FEDERALLY RECOGNIZED TRIBE")
                             
mydf$ethnicity[ethnicity_collapse] <- "American Indian & Alaska Native" 
 
ethnicity_collapse2 <- which(mydf$ethnicity=="ASIAN" | mydf$ethnicity =="ASIAN - ASIAN INDIAN"
                             | mydf$ethnicity == "ASIAN - CAMBODIAN" | mydf$ethnicity == "ASIAN - CHINESE"
                             | mydf$ethnicity == "ASIAN - FILIPINO" | mydf$ethnicity == "ASIAN - JAPANESE"
                             | mydf$ethnicity == "ASIAN - KOREAN" | mydf$ethnicity == "ASIAN - OTHER"
                             | mydf$ethnicity == "ASIAN - THAI" | mydf$ethnicity == "ASIAN - VIETNAMESE")
 
mydf$ethnicity[ethnicity_collapse2] <- "Asian" 

ethnicity_collapse3 <- which(mydf$ethnicity == "BLACK/AFRICAN" | mydf$ethnicity == "BLACK/AFRICAN AMERICAN"
                            | mydf$ethnicity == "BLACK/CAPE VERDEAN" | mydf$ethnicity == "BLACK/HAITIAN")

mydf$ethnicity[ethnicity_collapse3] <- "Black or African American" 

ethnicity_collapse4 <- which(mydf$ethnicity == "HISPANIC OR LATINO" | mydf$ethnicity == "HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)"
                             | mydf$ethnicity == "HISPANIC/LATINO - COLOMBIAN" | mydf$ethnicity == "HISPANIC/LATINO - CUBAN"
                             | mydf$ethnicity == "HISPANIC/LATINO - DOMINICAN" | mydf$ethnicity == "HISPANIC/LATINO - GUATEMALAN"
                             | mydf$ethnicity == "HISPANIC/LATINO - HONDURAN" | mydf$ethnicity == "HISPANIC/LATINO - MEXICAN"
                             | mydf$ethnicity == "HISPANIC/LATINO - PUERTO RICAN" | mydf$ethnicity == "HISPANIC/LATINO - SALVADORAN")

mydf$ethnicity[ethnicity_collapse4] <- "Hispanic or Latino"

ethnicity_collapse5 <- which(mydf$ethnicity == "WHITE" | mydf$ethnicity == "WHITE - BRAZILIAN"
                             | mydf$ethnicity == "WHITE - EASTERN EUROPEAN" | mydf$ethnicity == "WHITE - OTHER EUROPEAN"
                             | mydf$ethnicity == "WHITE - RUSSIAN")

mydf$ethnicity[ethnicity_collapse5] <- "White" 

keep_ind <- which(mydf$ethnicity == "Asian" | mydf$ethnicity == "White" |
                    mydf$ethnicity == "Black or African American" | mydf$ethnicity == "Hispanic or Latino" 
                    | mydf$ethnicity == "American Indian & Alaska Native"
                    | mydf$ethnicity == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER")  
 
mydf$ethnicity[-keep_ind] <- NA 
 
mydf$ethnicity <- as.factor(mydf$ethnicity)

summary(mydf$ethnicity)

#Diagnosis

mydf$diagnosis <- as.factor(mydf$diagnosis)
summary(mydf$diagnosis)

#Hospital_expire_flag

mydf$hospital_expire_flag <- as.factor(mydf$hospital_expire_flag)
summary(mydf$hospital_expire_flag)

#### Visualization ####

# Important variable

# LOS by gender: bar

g_LOS_gender <- ggplot(data = na.omit(mydf)) 
g_LOS_gender + geom_histogram(aes(x=hosp_stay, fill=factor(gender)), binwidth=5, position = "dodge") +
  ggtitle("Relationship between Length of Stay and Gender") + 
  xlab("Length of Stay") + ylab("Count")

# LOS and gender: boxplot

g_LOS_gender2 <- ggplot(data = na.omit(mydf)) 
g_LOS_gender2 + geom_boxplot(aes(x=gender, y=hosp_stay)) +
  ggtitle("Relationship between Length of Stay and Gender") + 
  xlab("Gender") + ylab("Hospital Length of stay")

# LOS and class: boxplot

g_LOS_class <- ggplot(data = na.omit(mydf)) 
g_LOS_class + geom_boxplot(aes(x=class, y=hosp_stay)) +
  ggtitle("Relationship between Length of Stay and Class") + 
  xlab("Class") + ylab("Length of stay")

# Age Level and Length of Stay: boxplot

g_agelv_LOS <- ggplot(na.omit(mydf), aes(x=age_level, y = hosp_stay))
g_agelv_LOS + geom_boxplot() + ggtitle("Relationship between Age and Length of Stay") + xlab("Age Level")+ ylab("Length of Stay")

# min_plat and class

g_class_MP <- ggplot(data = na.omit(mydf)) 
g_class_MP + geom_boxplot(aes(x=class, y=min_plat)) +
  ggtitle("Relationship between Length of class and min platelet") + 
  xlab("Class") + ylab("Min Platelet")

# Class and Gender

g_class_gender <- ggplot(data = na.omit(mydf)) 
g_class_gender + geom_bar(aes(x = class, fill = gender), width = 0.5, position = "dodge") + ggtitle("Relationship between Class and Gender") + xlab("Class") + ylab("Gender")

# Age level and class

g_agelv_class <- ggplot(data = na.omit(mydf))
g_agelv_class + geom_bar(aes(x = age_level, fill = class), position = "dodge") + ggtitle("Relationship between Age Level and Class")  + xlab("Age Level") + ylab("Class")


##### PART 2: Modeling #####


#Spliting data into test set and train set

summary(mydf)
anyNA(mydf)

mydf2 <- mydf
summary(mydf2)
mydf2 <- na.omit(mydf2)
anyNA(mydf2)

mydf3 <- mydf2[-c(1:3, 5:6, 9, 14, 24)] 
#take out: subjcect_id, hadm_id, icd9, admit, dishtime, diagnosis, charttime.
summary(mydf3)

#split data into train/ test set: p = .75

intrain <- createDataPartition(y = mydf3$class, p= 0.75, list = FALSE)
training <- mydf3[intrain,]
testing <- mydf3[-intrain,]
dim(intrain); dim(training); dim(testing)


#logistic regression: 

fit1 <- glm(class~ insurance + ethnicity + hospital_expire_flag + gender
            + age_yrs + hosp_stay + creati2 + min_plat + o2 + diuretic
            + albut + advair + budesonide + losartan + montelukast, family = 'binomial', data = training)
summary(fit1)

summary(mydf3$class)

#take out insignificant variables: budesonide, gender, insurance, ethnicity and creati2

fit2 <- glm(class ~ hospital_expire_flag + hosp_stay + age_yrs + min_plat + o2 + diuretic 
            + albut + advair + losartan + montelukast, family = 'binomial', data = training)
summary(fit2)

test_prediction <- predict(fit2, newdata = testing, type = "response")
test_prediction2 <- ifelse(test_prediction > 0.5, 1, 0)

confusionMatrix(factor(test_prediction2), testing$class)
#str(test_prediction2)
#str(testing$class)
#identical(levels(test_prediction2),levels(testing$class))

exp(coef(fit2)) # to get the odds
vif(fit2)


# pseudoR2 <- function(LogModel) {
#   dev <- LogModel$deviance
#   nullDev <- LogModel$null.deviance
#   modelN <-  length(LogModel$fitted.values)
#   R.l <-  1 -  dev / nullDev
#   R.cs <- 1- exp ( -(nullDev - dev) / modelN)
#   R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
#   cat("Pseudo R^2 for logistic regression\n")
#   cat("Hosmer and Lemeshow R^2  ", round(R.l, 4), "\n")
#   cat("Cox and Snell R^2        ", round(R.cs, 4), "\n")
#   cat("Nagelkerke R^2           ", round(R.n, 4),    "\n")
# }

#use pR2 function in pscl package:

round(pR2(fit2), 2)

install.packages("ROCR")
library(ROCR)



#SVM (linear, rbf, poly)

#linear

summary(training$class)
summary(testing$class)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(class ~., data = training, method = "svmLinear",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10)
 
svm_Linear

svm_Linear2 <- train(class ~., data = training, method = "svmLinear",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tunegrid = expand.grid(C = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
svm_Linear2
svm_Linear2$results
svm_Linear2$bestTune


test_pred_svm_lin <- predict(svm_Linear2, newdata = testing)
test_pred_svm_lin

cm <- confusionMatrix(test_pred_svm_lin, testing$class)

sensitivity <- cm$byClass[[1]]
specitivity <- cm$byClass[[2]]



##rbf##


svm_rbf2 <- train(class ~., data = training, method = "svmRadial", 
                  trControl=trctrl, preProcess = c("center", "scale"), 
                  tunegrid = expand.grid(C = c(0.001, 0.01, 0.1, 1, 5, 10, 100), 
                                         sigma = c(0.1,0.5,1,2,3,4)))
svm_rbf2
summary(svm_rbf2)
svm_rbf2$results
svm_rbf2$bestTune

test_pred_svm_rbf <- predict(svm_rbf2, newdata = testing)
test_pred_svm_rbf

confusionMatrix(test_pred_svm_rbf, testing$class)

#poly: To avoid long runtime, default degree = 1 (Reference: Meelad's)

CV_folds <- createMultiFolds(training, k = 5, times = 3)

svm_poly <- train(class ~., data = training, method = "svmPoly", tuneLength = 3, 
                  trControl=trainControl(method='repeatedCV',index=CV_folds), scale =FALSE)

svm_poly

# svm_poly2 <- train(class ~ ., data = training, method = "svmPoly",
#                      trControl=trctrl,
#                      preProcess = c("center", "scale"),
#                      tunegrid = expand.grid(C = c(0.001, 0.01, 0.1, 1, 5, 10, 100), degree = c(1,2)))
# summary(svm_poly2)

svm_poly$results
svm_poly$bestTune

test_pred_svm_poly <- predict(svm_poly, newdata = testing)
test_pred_svm_poly

confusionMatrix(test_pred_svm_poly, testing$class)


#decision tree

dtree_fit <- train(class ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

dtree_fit

#plot(dtree_fit$finalModel)
#text(dtree_fit$finalModel) 

prp(dtree_fit$finalModel, box.palette = "Blues")

test_pred_dtree <- predict(dtree_fit, newdata = testing)
test_pred_dtree

confusionMatrix(test_pred_dtree, testing$class)

#dtree <- rpart(class~., data = training)
#rpart.plot(dtree, box.palette = "auto", shadow.col = 0)

#random forest

rf_fit <- randomForest(class ~ ., data = training, importance = TRUE)

rf_fit

round(importance(rf_fit), 2)

test_pred_rf <- predict(rf_fit, newdata = testing)
test_pred_rf

confusionMatrix(test_pred_rf, testing$class)

