rm(list = ls())

#Install Packages#############
#install.packages ("ElemStatLearn")
#install.packages("randomForest")
# load library
library(ElemStatLearn)
library(DataExplorer)
library(glmnet)
library(pls)
library(leaps)
library("randomForest")

############################
# Load prostate Data set
############################
df = spam
## raname column name######################
colnames(df)[58] = "A.58"

#########################################
# Create a training and test set
#########################################
set.seed(123)
#read data
train = sample(nrow(df), nrow(df)*0.80)
test = -train
df_train = df[train, ]
df_test = df[test, ]

df_train$A.58 = as.character(df_train$A.58)
df_train$A.58 = as.factor(df_train$A.58)


### apply random forest for multiple mtry
# get m
m = round(sqrt(dim(df_train)[2]-1))
for (i in c(500,1000,2000)){ 
  for (j in c(m-1, m, m+1)){ 
    set.seed(123)
    rf.spam=randomForest(A.58 ~., data=df_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=df_train$A.58
    error_rate = mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) 
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) 
    }
  }
}
oob_err = as.data.frame(oob_err) 
names(oob_err) = c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry = as.factor(oob_err$mtry) 
library(ggplot2)

ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 


# get parameters for best tree
num_tree = oob_err$ntree[which.min(oob_err$oob_error_rate)]
num_tree

## [1] 1000
mtry = as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry


min(oob_err$oob_error_rate) 


## apply prediction on Test Data Set

rf.spam=randomForest(A.58 ~., data=df_train,  importance=TRUE, mtry=mtry, ntree=num_tree)

yhat = predict(rf.spam, df_test, type = 'class')
y_train = df_test$A.58
table(y_train,yhat) 

mean(y_train != yhat)

