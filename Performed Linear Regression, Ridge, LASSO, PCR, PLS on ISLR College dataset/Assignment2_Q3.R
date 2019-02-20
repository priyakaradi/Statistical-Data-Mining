##############################
## Question 3
##############################
rm(list=ls())
library(leaps)
x = matrix(rnorm(1000*20),1000)
beta = matrix(rnorm(20*1),20)
beta[1:7]=0
epsilon = matrix((500),1000)
y=x %*% beta + epsilon

set.seed(123)
split = sample(nrow(x),nrow(x)*0.10)
x_train = x[split,]
x_test = x[-split,]
y_train = y[split,]
y_test = y[-split,]

train_set = data.frame(x_train,y_train)
test_set = data.frame(x_test,y_test)

lm_model = lm(y_train ~., data =train_set)
predict_lm = predict(lm_model, test_set)
rmse = sqrt(mean((y_test - predict_lm)^2))
rmse

best_subset = regsubsets(y_train ~., data =train_set, nvmax = 20, method = 'exhaustive' )
pred_train_mat = model.matrix(y_train ~., data = train_set)
pred_test_mat = model.matrix(y_test~., data=test_set)

train_error_store = c()
test_error_store = c()

for (i in 1:20){
  coef_id = coef(best_subset, id=i)
  pred_train = pred_train_mat[,names(coef_id)] %*% coef_id
  train_error_store[i] = sqrt(mean((y_train - pred_train)^2))
  pred_test = pred_test_mat[,names(coef_id)] %*% coef_id
  test_error_store[i] = sqrt(mean((y_test - pred_test)^2))
}

graphics.off()
quartz()
par(mfrow=c(2,1))
plot(train_error_store,main='Train Set Error', ylab='Error Value', xlab='Model Size')
which.min(train_error_store)

plot(test_error_store, main='Test Set Error', ylab='Error Value', xlab='Model Size')
which.min(test_error_store)
coef(best_subset, which.min(test_error_store))
summary(lm_model)
