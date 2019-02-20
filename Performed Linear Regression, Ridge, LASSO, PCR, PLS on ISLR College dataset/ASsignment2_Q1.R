##############################
## Question 1
##############################
## PART A: Linear Regression
##############################

rm(list=ls())
library(ISLR)
ds <- College
str(ds)
ds$Private=ifelse(ds$Private=='Yes',1,0)

summary(ds)

##############################
## Split Data
##############################
set.seed(9)
split=sample(nrow(ds),nrow(ds)*.75)
training_set=ds[split,]
test_set=ds[-split,]
#pairs(ds)
##############################
## Fit Model
##############################
linear_regression = lm(Apps~.,training_set)
summary(linear_regression)

##############################
## Prediction
##############################
pred_apps = predict(linear_regression, test_set)
rmse_lm = sqrt(mean((pred_apps - test_set$Apps)^2))
rmse_lm

##############################
## PART B: Ridge Regression
##############################
library(ElemStatLearn)
library(glmnet)
library(leaps)
ridge_train_set = na.omit(training_set)
ridge_test_set = na.omit(test_set)

#levels(ridge_train_set[,1])
#ridge_train_set$Private=ifelse(ridge_train_set$Private=='Yes',1,0)
#ridge_test_set$Private=ifelse(ridge_test_set$Private=='Yes',1,0)

# Function glmnet reqquires dataset to be in matrix form
ridge_train_x = as.matrix(ridge_train_set[,-2])
ridge_train_y = (ridge_train_set[,2])

ridge_test_x = as.matrix(ridge_train_set[,-2])
ridge_test_y = (ridge_train_set[,2])


# Fit ridge model
ridge_model = glmnet(ridge_train_x, ridge_train_y, alpha = 0)
ridge_model_cv = cv.glmnet(ridge_train_x, ridge_train_y, alpha = 0)
bestlam1 = ridge_model_cv$lambda.min
ridge_pred = predict(ridge_model, newx = ridge_test_x, s= bestlam1, type='response')

rmse_ridge = sqrt(mean((ridge_pred - ridge_test_y)^2))
rmse_ridge

##############################
## PART D: Lasso Regression
##############################
lasso_model = glmnet(ridge_train_x, ridge_train_y, alpha = 1)
lasso_model_cv = cv.glmnet(ridge_train_x, ridge_train_y, alpha = 1)
bestlam2 = lasso_model_cv$lambda.min
lasso_pred_coef = predict(lasso_model, s = bestlam2, newx = ridge_test_x, type = "coefficients")
lasso_pred = predict(lasso_model, s = bestlam2, newx = ridge_test_x, type = "response")
length(lasso_pred_coef)
rmse_lasso = sqrt(mean((lasso_pred - ridge_test_y)^2))
rmse_lasso

##############################
## PART E : Principal Components Regression
##############################
library(pls)
pcr_train = training_set
pcr_test = test_set

#pcr_train$Private = ifelse(training_set$Private == 'Yes',1,0)
#pcr_test$Private = ifelse(test_set$Private == 'Yes',1,0)

pcr_model = pcr(Apps~., data = pcr_train, scale = TRUE, validation = "none")
summary(pcr_model)
validationplot(pcr_model, val.type = "MSEP")

training_error_store = c()
test_error_store = c()
for (i in 1:17){
  pcr_pred_train = predict(pcr_model, pcr_train, ncomp = i)
  pcr_pred_test = predict(pcr_model, pcr_test, ncomp = i)
  train_error = sqrt(mean((pcr_pred_train-pcr_train[,2])^2))
  test_error = sqrt(mean((pcr_pred_test-pcr_test[,2])^2))
  training_error_store = c(training_error_store, train_error)
  test_error_store = c(test_error_store, test_error)
}

plot(test_error_store, main='Test Errors')
min(test_error_store)
which.min(test_error_store)

#########################################
## PART F: Partial Least Squares 
#########################################
pls_model = plsr(Apps ~., data = pcr_train, scale = TRUE, validation = "none")
summary(pls_model)
validationplot(pls_model, val.type = "MSEP")

training_error_store_pls = c()
test_error_store_pls = c()
for (i in 1:17){
  pls_pred_train = predict(pls_model, pcr_train, ncomp = i)
  pls_pred_test = predict(pls_model, pcr_test, ncomp = i)
  train_error = sqrt(mean((pls_pred_train-pcr_train[,2])^2))
  test_error = sqrt(mean((pls_pred_test-pcr_test[,2])^2))
  training_error_store_pls = c(training_error_store_pls, train_error)
  test_error_store_pls = c(test_error_store_pls, test_error)
}

plot(test_error_store_pls)
min(test_error_store_pls)
which.min(test_error_store_pls)

min(test_error_store)
min(test_error_store_pls)
rmse_lasso
rmse_ridge
rmse_lm

