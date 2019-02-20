rm(list = ls())
setwd("~/Documents/MES Data Science/Statistical Data Mining/Assignment 1/Zipcode")
library("ggplot2")
library("ElemStatLearn")
data("zip.test")
#rm(zip.test)
#test = subset(zip.test)
#write.table(test, file = "zipcode_test.txt", sep = "\t", row.names = FALSE)
ziptraindata = zip.train
ziptestdata = zip.test

#write.table(ziptraindata, file = "zipcode_train.tsv", sep = "\t", row.names = FALSE)
ziptraindata = read.delim("zipcode_train.tsv", sep = '\t', header = TRUE)

#write.table(ziptestdata, file = "zipcode_test.tsv", sep = "\t", row.names = FALSE)
ziptestdata = read.delim("zipcode_test.tsv", sep = '\t', header = TRUE)

#######################################
## Filtering according to 2's and 3's
#######################################
zipfilteredtestdata = subset(ziptestdata, ziptestdata[,1] == 2 | ziptestdata[,1] == 3)
zipfilteredtraindata = subset(ziptraindata, ziptraindata[,1] == 2 | ziptraindata[,1] == 3)

##################################################################
## Generatimg linear model and prediction for train and test set
#################################################################
trainmodel = lm(V1 ~ . , data = zipfilteredtraindata)
testmodel = predict.lm(trainmodel, newdata = zipfilteredtestdata)
trainmodel = predict.lm(trainmodel, newdata = zipfilteredtraindata)

##############################################################
## Mean square error
##############################################################
errorpredictedinlm = sqrt(mean((zipfilteredtestdata$V1 - testmodel)^2))
errorpredictedinlm_train = sqrt(mean((zipfilteredtraindata$V1 - trainmodel)^2))


##################
## kNN 
##################
require(class)


#####################################
## Test set
#####################################

for (i in c(1,3,5,7,9,11,13,15))
{
  Knn_test <- knn(zipfilteredtraindata[,-1], zipfilteredtestdata[,-1], zipfilteredtraindata$V1, k = i)
  temp<-paste("knn_test_list",i,sep="_")
  assign(temp,as.numeric(as.character(Knn_test)))
}

#####################################
## Train set
#####################################

for (i in c(1,3,5,7,9,11,13,15))
{
  Knn_train <- knn(zipfilteredtraindata[,-1], zipfilteredtraindata[,-1], zipfilteredtraindata$V1, k = i)
  temp<-paste("knn_train_list",i,sep="_")
  assign(temp,as.numeric(as.character(Knn_train)))
}
#######################################################
#calculating error for knn

#function for calculation of error

#test
knnErrorCalculation <- function(x)
{
  a = zipfilteredtestdata$V1
  b = as.numeric(as.character(x))
  errorinknn = mean((a-b)^2)
  return(errorinknn)
}

#train
knnErrorCalculation <- function(x)
{
  a = zipfilteredtraindata$V1
  b = as.numeric(as.character(x))
  errorinknn = mean((a-b)^2)
  return(errorinknn)
}


# for printing the knn error values we call the function

#test
knn_1 = knnErrorCalculation(knn_test_list_1)
knn_3 = knnErrorCalculation(knn_test_list_3)
knn_5 = knnErrorCalculation(knn_test_list_5)
knn_7 = knnErrorCalculation(knn_test_list_7)
knn_9 = knnErrorCalculation(knn_test_list_9)
knn_11 = knnErrorCalculation(knn_test_list_11)
knn_13 = knnErrorCalculation(knn_test_list_13)
knn_15 = knnErrorCalculation(knn_test_list_15)
knn_1
knn_3
knn_5
knn_7
knn_9
knn_11
knn_13
knn_15

#train
knnt__1 = knnErrorCalculation(knn_train_list_1)
knnt__3 = knnErrorCalculation(knn_train_list_3)
knnt__5 = knnErrorCalculation(knn_train_list_5)
knnt__7 = knnErrorCalculation(knn_train_list_7)
knnt__9 = knnErrorCalculation(knn_train_list_9)
knnt__11 = knnErrorCalculation(knn_train_list_11)
knnt__13 = knnErrorCalculation(knn_train_list_13)
knnt__15 = knnErrorCalculation(knn_train_list_15)
knnt__1
knnt__3
knnt__5
knnt__7
knnt__9
knnt__11
knnt__13
knnt__15
