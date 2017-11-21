
## KNN for Data-set 1

gc()
rm(list=ls())
#Set the library
setwd("C:/AML - BUAN 6341")

install.packages ("gmodels"); install.packages("MASS")
install.packages("caret");install.packages("ggplot2")
install.packages("plyr")
install.packages("class")
install.packages("corrplot")
install.packages("dummies")

setwd("D:/Fall 17/Machine Learning/Project 2")

#Read data 

data = read.csv("data.csv", header=T)

#Print Summary

summary(data)
str(data)

#attach(data)

#Delete the variable created at the end.
data=subset(data,select = -X)

#Checking for missing values
mv_data=sapply(data, function(x){sum(is.na(x))/length(x)}*100)
mv_data
#The dataset has no missing values

# Simple Bar Plot 
counts <- table(data$diagnosis)
counts
barplot(counts, main="Cancer type distribution", 
        xlab="Type of cancer")

#checking the proportion of the target variable-"diagnosis"
round(prop.table(table(data$diagnosis))*100)
#B = 63 and M=37

#transforming the target variable(diagnosis) as M==1 and B==0
data$diagnosis=ifelse(data$diagnosis == 'M', 1, 0)
#data$diagnosis=as.factor(data$diagnosis)

#installing packages to plot the correlation matrix
library("ggplot2")
#library("dplyr")
library("plyr")

#Apply cor function to display pair-wise correlation
library(corrplot)
require(lattice)

data <- subset(data, select = -id)
data_cor <- cor(data[sapply(data, function(x) !is.factor(x))])

levelplot(data_cor)
data_cor

library(caret)
#setting cutoff value of correlation coeff=0.70
Highly_correlated=findCorrelation(data_cor, cutoff = 0.70)
#displaying highly correlated variables
Highly_correlated_var=colnames(data[Highly_correlated])
Highly_correlated_var


#removing the highly correlated variables
data_req=data[,-Highly_correlated]
str(data_req)

#Add target variable to data
data_req=cbind(data_req,data$diagnosis)

#renaming the target variable
data_req=rename(data_req, c("data$diagnosis"="diagnosis"))
str(data_req)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data_normalize <- as.data.frame(lapply(data_req[1:10], normalize))

summary(data_normalize$area_mean)

train_n <- data_normalize[1:469,]
test_n <- data_normalize[470:569,]

train_labels <- data_req[1:469,11]
test_labels <- data_req[470:569,11]


library(class)
library(caret)

set.seed(133)
pred_test <- knn(train = train_n,test = test_n,cl = train_labels,k=20)
Conf_matrix <- table(test_labels,pred_test)
Conf_matrix

confusionMatrix(pred_test,test_labels)

train_labels <- factor(train_labels)


library (gmodels); library (MASS)

CrossTable(x=test_labels,y=pred_test,prop.chisq = FALSE)


library("caret")



data_normalize_cv <- as.data.frame(lapply(data_req[1:11], normalize))
training <- data_normalize_cv[1:469,]
testing <- data_normalize_cv[470:569,]

training$diagnosis <- as.factor(training$diagnosis)

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(4433)
knn_fit <- train(diagnosis ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 15)
knn_fit
plot(knn_fit)

##train dataset accuracy

attach(training)
trainData_x <- subset(training, select = -diagnosis)
train_pred <- predict(knn_fit, newdata = trainData_x)
training$diagnosis <- as.factor(training$diagnosis)
confusionMatrix(train_pred, training$diagnosis)


testData_x <- subset(testing, select = -diagnosis)
test_pred <- predict(knn_fit, newdata = testData_x)
testing$diagnosis <- as.factor(testing$diagnosis)
confusionMatrix(test_pred, testing$diagnosis)
##***********************************************************************************************************************************************************
## KNN for Data-set 2

gc()
rm(list=ls())
#Set the library


setwd("C:/AML - BUAN 6341")

#Read data 

Data = read.csv("churn.csv", header=T)
head(Data)
names(Data)

## Remove/Impute NA values
data <- na.omit(Data)

##Remove ID Field
data <- subset(data, select = -customerID)

##Check if there are any missing values.
mv_data=sapply(data, function(x){sum(is.na(x))/length(x)}*100)
mv_data


library(dummies)
data <- dummy.data.frame(data, names=c("MultipleLines","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract"), sep="_")
names(data)

counts <- table(data$Churn)
counts
barplot(counts, main="distribution", 
        xlab="churn")

data$gender=ifelse(data$gender == 'Female', 1, 0)
data$Churn=ifelse(data$Churn == 'Yes', 1, 0)
data$Partner=ifelse(data$Partner == 'Yes', 1, 0)
data$Dependents=ifelse(data$Dependents == 'Yes', 1, 0)
data$PhoneService=ifelse(data$PhoneService == 'Yes', 1, 0)
data$PaperlessBilling=ifelse(data$PaperlessBilling == 'Yes', 1, 0)


library("ggplot2")
library("plyr")

#Apply cor function to display pair-wise correlation

library(corrplot)
require(lattice)
require(ggplot2)

data_cor <- cor(data[sapply(data, function(x) !is.factor(x))])

levelplot(data_cor)
data_cor

library(caret)
#setting cutoff value of correlation coeff=0.70
Highly_correlated=findCorrelation(data_cor, cutoff = 0.70)
#displaying highly correlated variables
Highly_correlated_var=colnames(data[Highly_correlated])
Highly_correlated_var

#removing the highly correlated variables
data_req=data[,-Highly_correlated]
str(data_req)

#data_req=rename(data_req, c("data$Churn"="Churn"))
str(data_req)

#setting seed for reproducibility
set.seed(3344)

#Splitting the data into train(70%) and test(30%)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data_normalize <- as.data.frame(lapply(data_req[1:26], normalize))
set.seed(2344)
train_n <- data_normalize[1:4923,]
test_n <- data_normalize[4924:7032,]

train_labels <- data_req[1:4923,27]
test_labels <- data_req[4924:7032,27]

library(class)
library(caret)

set.seed(133)
pred_test <- knn(train = train_n,test = test_n,cl = train_labels,k=70)
Conf_matrix <- table(test_labels,pred_test)
Conf_matrix
confusionMatrix(pred_test,test_labels)

library (gmodels); library (MASS)

CrossTable(x=test_labels,y=pred_test,prop.chisq = FALSE)

library(caret)

train_labels <- factor(train_labels)

data_normalize_cv <- as.data.frame(lapply(data_req[1:27], normalize))
training <- data_normalize_cv[1:4923,]
testing <- data_normalize_cv[4924:7032,]

training$Churn <- as.factor(training$Churn)

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(333)
knn_fit <- train(Churn ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
plot(knn_fit)

##train dataset accuracy

trainData_x <- subset(training, select = -Churn)
train_pred <- predict(knn_fit, newdata = trainData_x)
training$Churn <- as.factor(training$Churn)
confusionMatrix(train_pred, training$Churn)

##test dataset accuracy

testData_x <- subset(testing, select = -Churn)
test_pred <- predict(knn_fit, newdata = testData_x)
testing$Churn <- as.factor(testing$Churn)
confusionMatrix(test_pred, testing$Churn)


##***********************************************************************************************************************************************

## ANN for Data-set 1
gc()
rm(list=ls())

rm(list = ls())
install.packages(c("plyr", "dplyr", "rpart", "rpart.plot","pacman","h2o","data.table","ggplot2","caret","functional","dummies","corrplot","lattice"))
library(pacman)
pacman::p_load(dplyr, rpart,plyr,ggplot2,functional,caret,dummies,h2o,data.table,corrplot,lattice) 

#Set the library
setwd("C:/AML - BUAN 6341")

#Read data 

Data = read.csv("churn.csv", header=T)
head(Data)
names(Data)

## Remove/Impute NA values
data <- na.omit(Data)

##Remove ID Field
data <- subset(data, select = -customerID)

##Check if there are any missing values.
mv_data=sapply(data, function(x){sum(is.na(x))/length(x)}*100)
mv_data

#Apply cor function to display pair-wise correlation

data_cor <- cor(data[sapply(data, function(x) !is.factor(x))])

levelplot(data_cor)
data_cor

#setting cutoff value of correlation coeff=0.70

Highly_correlated=findCorrelation(data_cor, cutoff = 0.70)
#displaying highly correlated variables
Highly_correlated_var=colnames(data[Highly_correlated])
Highly_correlated_var

#removing the highly correlated variables
data_req=data[,-Highly_correlated]
str(data_req)
dim(data_req)


#data_req=rename(data_req, c("data$Churn"="Churn"))
str(data_req)

#setting seed for reproducibility
set.seed(1234)


#------------ H2O package --------------------------------------------------------------------------
install.packages("h2o")
library(h2o)
h2o.init()
data <- h2o.importFile(path = normalizePath("C:/AML - BUAN 6341/Churn_h2o.csv"))
dim(data)

## 70% training data, 15% validation, 15% test

splits1 <- h2o.splitFrame(data, c(0.7,0.15), seed=1234)
train1  <- h2o.assign(splits[[1]], "train1.hex") # 70%
valid1  <- h2o.assign(splits[[2]], "valid1.hex") # 15%
test1   <- h2o.assign(splits[[3]], "test1.hex") 

train1 <- train1[,-1]
valid1 <- valid1[,-1]
test1 <- test1[,-1]

## 60% training data, 20% validation, 20% test
splits2 <- h2o.splitFrame(data, c(0.6,0.20), seed=1234)
train2  <- h2o.assign(splits[[1]], "train2.hex") # 70%
valid2  <- h2o.assign(splits[[2]], "valid2.hex") # 15%
test2   <- h2o.assign(splits[[3]], "test2.hex") 

train2 <- train2[,-1]
valid2 <- valid2[,-1]
test2 <- test2[,-1]

## 50% training data, 25% validation, 25% test
splits3 <- h2o.splitFrame(data, c(0.5,0.25), seed=1234)
train3  <- h2o.assign(splits[[1]], "train3.hex") # 70%
valid3  <- h2o.assign(splits[[2]], "valid3.hex") # 15%
test3   <- h2o.assign(splits[[3]], "test3.hex") 

train3 <- train3[,-1]
valid3 <- valid3[,-1]
test3 <- test3[,-1]

names(test)
dim(train)

#args(h2o.deeplearning)
#help(h2o.deeplearning)
#example(h2o.deeplearning)

## Varying the proportion of train data-set

m1 <- h2o.deeplearning(
  model_id="ANN_tuned1", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=1:18, 
  y=19, 
  input_dropout_ratio = 0.2,
  hidden=c(5,5),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01) 

m2 <- h2o.deeplearning(
  model_id="ANN_tuned2", 
  training_frame=train2, 
  validation_frame=valid2, 
  x=1:18, 
  y=19, 
  input_dropout_ratio = 0.2,
  hidden=c(5,5),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

m3 <- h2o.deeplearning(
  model_id="ANN_tuned3", 
  training_frame=train3, 
  validation_frame=valid3, 
  x=1:18, 
  y=19, 
  input_dropout_ratio = 0.2,
  hidden=c(5,5),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

summary(m1)
summary(m2)
summary(m3)

RMSE_range <- c(0.3686381,0.3647361,0.3701969)
Train_data_proportion <- c(70,60,50)

plot(Train_data_proportion,RMSE_range,type = "l")

h2o.performance(m3, train=T)          ## sampled training data (from model building)
h2o.performance(m3, valid=T)          ## sampled validation data (from model building)
h2o.performance(m3, newdata=train)    ## full training data
h2o.performance(m3, newdata=valid)    ## full validation data
h2o.performance(m3, newdata=test)     ## full test data

pred <- h2o.predict(m3, test)
pred

## Varying the number of hidden layers and the number of neurons in each layer
m1 <- h2o.deeplearning(
  model_id="ANN_tuned1", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=1:18, 
  y=19, 
  input_dropout_ratio = 0.2,
  hidden=c(50,50),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01) 

m2 <- h2o.deeplearning(
  model_id="ANN_tuned2", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=1:18, 
  y=19, 
  input_dropout_ratio = 0.2,
  hidden=c(200,200),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

m3 <- h2o.deeplearning(
  model_id="ANN_tuned3", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=1:18, 
  y=19, 
  input_dropout_ratio = 0.2,
  hidden=c(100,50,50),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

m4 <- h2o.deeplearning(
  model_id="ANN_tuned4", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=1:18, 
  y=19, 
  input_dropout_ratio = 0.2,
  hidden=c(50,50,50,50),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

RMSE_range <- c(0.3769635,0.3708997,0.3734565,0.372566)
Model_number <- c(1,2,3,4)
plot(Model_number,RMSE_range,type = "l")

m5 <- h2o.deeplearning(
  model_id="ANN_tuned5", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=1:18, 
  y=19, 
  input_dropout_ratio = 0.2,
  hidden=c(200,200),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

plot(h2o.performance(m5),valid=T,type='roc')


plot(m5,timestep="samples",metric="logloss")


auc <- h2o.auc(m5, valid = TRUE)
fpr <- h2o.fpr( h2o.performance(m5, valid = TRUE) )[['fpr']]
tpr <- h2o.tpr( h2o.performance(m5, valid = TRUE) )[['tpr']]

ggplot( data.table(fpr = fpr, tpr = tpr), aes(fpr, tpr) ) + 
  geom_line(color = "green", linetype = 1) + ggtitle( sprintf('AUC: %f', auc) ) + theme(plot.subtitle = element_text(vjust = 1), 
                                                                                        plot.caption = element_text(vjust = 1)) +labs(x = "False Positive Rate", y = "True Positive Rate")


h2o.performance(m5, train=T)          ## sampled training data (from model building)
h2o.performance(m5, valid=T)          ## sampled validation data (from model building)
h2o.performance(m5, newdata=train1)    ## full training data
h2o.performance(m5, newdata=valid1)    ## full validation data
h2o.performance(m5, newdata=test1)     ## full test data

pred <- h2o.predict(m5, test)
pred

#*******************************************************************************************************************************************************************************
## ANN for Data-set 2

#data = read.csv("data.csv", header=T)

#Print Summary


h2o.init()
tempPath = "C:/AML - BUAN 6341/data.csv"
data_raw.hex <- h2o.importFile(path = tempPath, destination_frame = "data_raw.hex")
# data_raw.hex <- na.omit(data_raw.hex)
# data_raw.R <- as.data.frame(data_raw.hex)


## 70% training data, 15% validation, 15% test

splits1 <- h2o.splitFrame(data_raw.hex, c(0.7,0.15), seed=1234)
train1  <- h2o.assign(splits1[[1]], "train1.hex") # 70%
valid1  <- h2o.assign(splits1[[2]], "valid1.hex") # 15%
test1   <- h2o.assign(splits1[[3]], "test1.hex") 

train1 <- train1[,-1]
valid1 <- valid1[,-1]
test1 <- test1[,-1]

dim(train1)
str(train1)

## 60% training data, 20% validation, 20% test
splits2 <- h2o.splitFrame(data_raw.hex, c(0.6,0.20), seed=1234)
train2  <- h2o.assign(splits2[[1]], "train2.hex") # 70%
valid2  <- h2o.assign(splits2[[2]], "valid2.hex") # 15%
test2   <- h2o.assign(splits2[[3]], "test2.hex") 

train2 <- train2[,-1]
valid2 <- valid2[,-1]
test2 <- test2[,-1]

## 50% training data, 25% validation, 25% test
splits3 <- h2o.splitFrame(data_raw.hex, c(0.5,0.25), seed=1234)
train3  <- h2o.assign(splits3[[1]], "train3.hex") # 70%
valid3  <- h2o.assign(splits3[[2]], "valid3.hex") # 15%
test3   <- h2o.assign(splits3[[3]], "test3.hex") 

train3 <- train3[,-1]
valid3 <- valid3[,-1]
test3 <- test3[,-1]

names(test)
dim(train)

#args(h2o.deeplearning)
#help(h2o.deeplearning)
#example(h2o.deeplearning)

## Varying the proportion of train data-set

m1 <- h2o.deeplearning(
  model_id="ANN_tuned1", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=2:32, 
  y=1, 
  input_dropout_ratio = 0.2,
  hidden=c(5,5),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01) 

m2 <- h2o.deeplearning(
  model_id="ANN_tuned2", 
  training_frame=train2, 
  validation_frame=valid2, 
  x=2:32, 
  y=1, 
  input_dropout_ratio = 0.2,
  hidden=c(5,5),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

m3 <- h2o.deeplearning(
  model_id="ANN_tuned3", 
  training_frame=train3, 
  validation_frame=valid3, 
  x=2:32, 
  y=1, 
  input_dropout_ratio = 0.2,
  hidden=c(5,5),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

summary(m1)
summary(m2)
summary(m3)

RMSE_range <- c(0.1352999,0.1090284,0.1393722)
Train_data_proportion <- c(70,60,50)

plot(Train_data_proportion,RMSE_range,type = "l")


## Varying the number of hidden layers and the number of neurons in each layer
m1 <- h2o.deeplearning(
  model_id="ANN_tuned1", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=2:32, 
  y=1, 
  input_dropout_ratio = 0.2,
  hidden=c(100,100),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01) 

m2 <- h2o.deeplearning(
  model_id="ANN_tuned2", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=2:32, 
  y=1, 
  input_dropout_ratio = 0.2,
  hidden=c(200,200),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

m3 <- h2o.deeplearning(
  model_id="ANN_tuned3", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=2:32, 
  y=1, 
  input_dropout_ratio = 0.2,
  hidden=c(200,100,50),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

m4 <- h2o.deeplearning(
  model_id="ANN_tuned4", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=2:32, 
  y=1, 
  input_dropout_ratio = 0.2,
  hidden=c(200,200,200),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

RMSE_range <- c(0.1780487,0.1400323,0.1632938,0.1428685)
Model_number <- c(1,2,3,4)
plot(Model_number,RMSE_range,type = "l")

m5 <- h2o.deeplearning(
  model_id="ANN_tuned5", 
  training_frame=train1, 
  validation_frame=valid1, 
  x=2:32, 
  y=1, 
  input_dropout_ratio = 0.2,
  hidden=c(200,200),          ## more hidden layers -> more complex interactions
  epochs=10,
  adaptive_rate=F,## to keep it short enough
  nfolds=5,
  fold_assignment="Modulo",
  activation=c("Tanh"),
  rate=0.01)

plot(h2o.performance(m5),valid=T,type='roc')


plot(m5,timestep="samples",metric="logloss")


auc <- h2o.auc(m5, valid = TRUE)
fpr <- h2o.fpr( h2o.performance(m5, valid = TRUE) )[['fpr']]
tpr <- h2o.tpr( h2o.performance(m5, valid = TRUE) )[['tpr']]

ggplot( data.table(fpr = fpr, tpr = tpr), aes(fpr, tpr) ) + 
  geom_line(color = "green", linetype = 1) + ggtitle( sprintf('AUC: %f', auc) ) + theme(plot.subtitle = element_text(vjust = 1), 
                                                                                        plot.caption = element_text(vjust = 1)) +labs(x = "False Positive Rate", y = "True Positive Rate")


h2o.performance(m5, train=T)          ## sampled training data (from model building)
h2o.performance(m5, valid=T)          ## sampled validation data (from model building)
h2o.performance(m5, newdata=train1)    ## full training data
h2o.performance(m5, newdata=valid1)    ## full validation data
h2o.performance(m5, newdata=test1)     ## full test data

pred <- h2o.predict(m5, test)
pred







