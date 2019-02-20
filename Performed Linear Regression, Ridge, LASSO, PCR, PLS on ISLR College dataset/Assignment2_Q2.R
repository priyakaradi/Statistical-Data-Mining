##############################
## Question 2
##############################
rm(list=ls())
library(leaps)
train_set = read.table('ticdata2000.txt')
test_set = read.table('ticeval2000.txt')
length(which(train_set$V86==1))
length(which(train_set$V86==0))

split = sample(nrow(train_set), nrow(train_set)*0.75)
train_data = train_set[split,]
test_data = train_set[-split,]

lm_model = lm(train_data$V86 ~., data = train_data)
lm_pred = predict(lm_model, test_data)
summary(lm_model)

rmse_lm = sqrt(mean((lm_pred - test_data)^2))
rmse_lm

fwd_selection = regsubsets(train_data$V86 ~., data = train_data, nvmax = 85, method = 'forward')
bwd_selection = regsubsets(train_data$V86 ~., data = train_data, nvmax = 85, method = 'backward')

fs = summary(fwd_selection)
plot(fs$rsq)

test_data_matrix = model.matrix(test_data$V86 ~., data = test_data)
fwd_test_result = c()
bwd_test_result = c()
test_error_fwd = c()
test_error_bwd = c()

for (i in 1:84) {
  coef_fwd = coef(fwd_selection, id = i)
  coef_bwd = coef(bwd_selection, id = i)
  pred_fwd = test_data_matrix[,names(coef_fwd)]%*%coef_fwd
  pred_bwd = test_data_matrix[,names(coef_bwd)]%*%coef_bwd
  test_error_fwd[i] = sqrt(mean((test_data$V86 - pred_fwd)^2))
  test_error_bwd[i] = sqrt(mean((test_data$V86 - pred_bwd)^2))
  pred_fwd_bool = ifelse(pred_fwd>0.2,1,0)
  pred_bwd_bool = ifelse(pred_bwd>0.2,1,0)
  fwd_test_result[i] = length(which(pred_fwd_bool == test_data$V86 & pred_fwd_bool == 1))
  bwd_test_result[i] = length(which(pred_bwd_bool == test_data$V86 & pred_bwd_bool == 1))
}

which.min(test_error_fwd)
min(test_error_fwd)
which.min(test_error_bwd)
min(test_error_bwd)

library(glmnet)
ridge_train_data = as.matrix(train_data[,-86])
ridge_test_data = as.matrix(test_data[,-86])
ridge_model = glmnet(ridge_train_data, train_data$V86, alpha = 0)
ridge_model_cv = cv.glmnet(ridge_train_data, train_data$V86, alpha = 0)
bestlam1 = ridge_model_cv$lambda.min
ridge_pred = predict(ridge_model, newx = ridge_test_data, s= bestlam1, type='response')
rmse_rd = sqrt(mean((ridge_pred - test_data[86])^2))
rmse_rd

lasso_model = glmnet(ridge_train_data, train_data$V86, alpha = 1)
lasso_model_cv = cv.glmnet(ridge_train_data, train_data$V86, alpha = 1)
bestlam2 = lasso_model_cv$lambda.min
lasso_pred = predict(lasso_model, newx = ridge_test_data, s= bestlam2, type='response')
rmse_ls = sqrt(mean((lasso_pred - test_data[86])^2))
rmse_ls

graphics.off()
quartz()
plot(ridge_model, main = 'Ridge Shrinkage')
plot(lasso_model, main = 'Lasso Shrinkage')

quartz()
par(mfrow=c(2,2))
plot(pred_fwd, main = 'Forward Selection')
plot(pred_bwd, main = 'Backward Selection')
plot(ridge_pred, main = 'Ridge Regression')
plot(lasso_pred, main = 'Lasso Regression')

pred_ridge_bool = ifelse(ridge_pred>0.2,1,0)
pred_lasso_bool = ifelse(lasso_pred>0.2,1,0)

quartz()
par(mfrow=c(2,2))
plot(pred_fwd_bool, main = 'Forward Selection')
plot(pred_bwd_bool, main = 'Backward Selection')
plot(pred_ridge_bool, main = 'Ridge Regression')
plot(pred_lasso_bool, main = 'Lasso Regression')

cm=table(lm_pred,test_data$V86)
cm
