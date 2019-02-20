############################
## Assignment 3: Question 1
############################
rm(list = ls())

library("MASS")
library(caret)

ds = Boston
#?Boston
head(ds)
str(ds)
summary(ds)
ds$crim_class = ifelse(ds$crim > median(ds$crim),1,0)
length(which(ds$crim_class == 0))

##########################################
## Logistic Model
##########################################
ds1 = ds[,-1]
str(ds1)
set.seed(123)
split = sample(nrow(ds1), nrow(ds1)*0.75)

ds_train = ds1[split,]
ds_test = ds1[-split,]

logistic_model = glm(crim_class ~., data=ds_train, family = "binomial")
summary(logistic_model)

logistic_predict_train = predict(logistic_model, ds_train, type = "response")
logistic_predict_test = predict(logistic_model, ds_test, type="response")

summary(logistic_predict_train)
y_hat_train = round(logistic_predict_train)
y_hat_test = round(logistic_predict_test)
y_true_test = as.numeric(ds_test$crim_class)
err_test_logistic = sum(abs(y_hat_test - y_true_test))/length(y_true_test)
err_test_logistic

#length(ds_train$crim_class)

cm_logistic_train = confusionMatrix(as.factor(y_hat_train),as.factor(ds_train$crim_class))
cm_logistic_train

cm_logistic = confusionMatrix(as.factor(y_hat_test),as.factor(ds_test$crim_class))
cm_logistic

##########################################
## Create model on selected predictors
##########################################
ds2 = ds1[,c("nox","dis","rad","medv","crim_class")]
ds2_train = ds2[split,]  
ds2_test = ds2[-split,]  
  
logistic_model_2 = glm(crim_class ~.,ds2_train, family = "binomial")
logistic_predict_train_2 = predict(logistic_model_2, ds2_train, type = "response")
logistic_predict_test_2 = predict(logistic_model_2, ds2_test, type="response")

y_hat_test_2 = round(logistic_predict_test_2)
y_true_test_2 = as.numeric(ds2_test$crim_class)
err_test_logistic = sum(abs(y_hat_test_2 - y_true_test_2))/length(y_true_test_2)
err_test_logistic
#class(y_hat_test_2)

cm_logistic_2 = confusionMatrix(as.factor(y_hat_test_2),as.factor(ds2_test$crim_class))
cm_logistic_2

##########################################
## LDA: Linear Discriminant Analysis
##########################################
cor(ds[,-15])
lda_model = lda(crim_class ~., ds_train) 
summary(lda_model)
lda_predict_train = predict(lda_model, ds_train)
lda_predict_test = predict(lda_model, ds_test)
  
cm_lda = confusionMatrix(lda_predict_test$class, as.factor(ds_test$crim_class))
cm_lda

lda_y_hat_test = as.numeric(lda_predict_test$class)-1
lda_y_true_test = as.numeric(ds_test$crim_class)-1
err_test_lda = sum(abs(lda_y_hat_test - lda_y_true_test))/length(lda_y_true_test)
err_test_lda
#cbind(da_y_hat_test, lda_y_true_test)

lda_model_2 = lda(crim_class ~., ds2_train)
lda_predict_train_2 = predict(lda_model_2, ds2_train)
lda_predict_test_2 = predict(lda_model_2, ds2_test)

cm_lda_2 = confusionMatrix(lda_predict_test_2$class, as.factor(ds2_test$crim_class))
cm_lda_2

lda_y_hat_test_2 = as.numeric(lda_predict_test_2$class)-1
lda_y_true_test_2 = as.numeric(ds_test$crim_class)-1
err_test_lda_2 = sum(abs(lda_y_hat_test_2 - lda_y_true_test_2))/length(lda_y_true_test_2)
err_test_lda_2

##########################################
## KNN: K-Nearest Neighbour
##########################################
library(class)
knn_predict = knn(ds_train[,-14], ds_test[,-14], as.factor(ds_train$crim_class), 3 )
knn_predict
#knn_predict_numeric = c(0, 1)[sapply(knn_predict, as.numeric)]
err_3knn = 1-mean(as.numeric(knn_predict) - as.numeric(ds_test$crim_class))
err_3knn

class(ds_test$crim_class)
cm_knn = confusionMatrix(knn_predict, as.factor(ds_test$crim_class))
cm_knn

knn_predict_2 = knn(ds_train[,-14], ds_test[,-14], as.factor(ds_train$crim_class), 5 )
knn_predict_2
#knn_predict_numeric = c(0, 1)[sapply(knn_predict, as.numeric)]
err_5knn = 1-mean(as.numeric(knn_predict_2) - as.numeric(ds_test$crim_class))
err_5knn

class(ds_test$crim_class)
cm_knn_2 = confusionMatrix(knn_predict_2, as.factor(ds_test$crim_class))
cm_knn_2

knn_predict_3 = knn(ds_train[,-14], ds_test[,-14], as.factor(ds_train$crim_class), 10 )
knn_predict_3
#knn_predict_numeric = c(0, 1)[sapply(knn_predict, as.numeric)]
err_10knn = 1-mean(as.numeric(knn_predict_3) - as.numeric(ds_test$crim_class))
err_10knn

class(ds_test$crim_class)
cm_knn_3 = confusionMatrix(knn_predict_3, as.factor(ds_test$crim_class))
cm_knn_3
#############################
## KNN on significant subset
#############################
sknn_predict = knn(ds2_train[,-5], ds2_test[,-5], as.factor(ds2_train$crim_class), 3 )
sknn_predict
#knn_predict_numeric = c(0, 1)[sapply(knn_predict, as.numeric)]
serr_3knn = 1-mean(as.numeric(sknn_predict) - as.numeric(ds2_test$crim_class))
serr_3knn

class(ds2_test$crim_class)
scm_knn = confusionMatrix(sknn_predict, as.factor(ds2_test$crim_class))
scm_knn

sknn_predict_2 = knn(ds2_train[-5], ds2_test[,-5], as.factor(ds2_train$crim_class), 5 )
sknn_predict_2
#knn_predict_numeric = c(0, 1)[sapply(knn_predict, as.numeric)]
serr_5knn = 1-mean(as.numeric(sknn_predict_2) - as.numeric(ds2_test$crim_class))
serr_5knn

scm_knn_2 = confusionMatrix(sknn_predict_2, as.factor(ds2_test$crim_class))
scm_knn_2

sknn_predict_3 = knn(ds2_train[,-5], ds2_test[,-5], as.factor(ds2_train$crim_class), 10 )
sknn_predict_3
#knn_predict_numeric = c(0, 1)[sapply(knn_predict, as.numeric)]
serr_10knn = 1-mean(as.numeric(sknn_predict_3) - as.numeric(ds2_test$crim_class))
serr_10knn

scm_knn_3 = confusionMatrix(sknn_predict_3, as.factor(ds2_test$crim_class))
scm_knn_3
