#install.packages ("e1071")
#install.packages("class")
library(ISLR)
library(e1071)
rm(list=ls())
df = OJ
set.seed(123)
#read data
train = sample(nrow(df), nrow(df)*0.80)
test = -train
df_train = df[train, ]
df_test = df[test, ]


linear_test_error = c()
linear_train_error = c()


#######Fitting SVM with Linear Kernel########
for (i in c(0.01,0.1, 1, 5, 10)){
  linear_svm = tune(svm, Purchase ~ .,data = df_train, kernel = "linear",
                        ranges = list(cost = i))
  
  
  best_model = linear_svm$best.model
  best_model
  
  #predict the test data
  y_hat = predict(best_model, newdata = df_test)
  y_true = df_test$Purchase
  
  test_err = length(which(y_true != y_hat))/length(y_true)
  
  linear_test_error=c(linear_test_error,test_err)
  
  y_hat = predict(best_model, newdata = df_train)
  y_true = df_train$Purchase
  
  train_err = length(which(y_true != y_hat))/length(y_true)
  
  linear_train_error=c(linear_train_error,train_err)
}
linear_test_error
linear_train_error

### Plot the results
upper_lr = max(linear_test_error, linear_train_error)
lower_lr = min(linear_test_error, linear_train_error)

plot(linear_train_error, type = "o", lty = 2, col = "red", ylim = c(lower_lr -1, upper_lr +1) , xlab = "cost", ylab = "error", main = "Test and Training Errors")
lines(linear_test_error, type = "o", lty = 1, col = "orange")
legend("topright", c("training", "test"), lty = c(2,1), col = c("red","orange"))

###4(b)
### Radial Kernel
radial_train_error = c()
radial_test_error= c()
for (i in c(0.01,0.1, 1, 5, 10)){
  radial_svm = tune(svm, Purchase ~ .,data = df_train, kernel = "radial",
                        ranges = list(cost = i))
  
  
  best_model = radial_svm$best.model
  best_model
  
  #predict the test data
  y_hat = predict(best_model, newdata = df_test)
  y_true = df_test$Purchase
  
  test_err = length(which(y_true != y_hat))/length(y_true)
  
  radial_test_error=c(radial_test_error,test_err)
  
  y_hat = predict(best_model, newdata = df_train)
  y_true = df_train$Purchase
  
  train_err = length(which(y_true != y_hat))/length(y_true)
  
  radial_train_error=c(radial_train_error,train_err)
}
radial_test_error
radial_train_error

### Plot the results
upper_lr = max(radial_test_error, radial_train_error)
lower_lr = min(radial_test_error, radial_train_error)

plot(radial_train_error, type = "o", lty = 2, col = "blue", ylim = c(lower_lr -1, upper_lr +1) , xlab = "cost", ylab = "error", main = "Test and Training Errors for Radial Kernel")
lines(radial_test_error, type = "o", lty = 1, col = "black")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue","black"))

table(predict = y_hat, truth = y_true)




###########Polynimal Kernel#########
poly_train_error = c()
poly_test_error= c()
for (i in c(0.01,0.1, 1, 5, 10)){
  poly_svm = tune(svm, Purchase ~ .,data = df_train, degree = 2, kernel = "polynomial",
                      ranges = list(cost = i))
  
  
  best_model = poly_svm$best.model
  best_model
  
  #predict the test data
  y_hat = predict(best_model, newdata = df_test)
  y_true = df_test$Purchase
  
  test_err = length(which(y_true != y_hat))/length(y_true)
  
  poly_test_error=c(poly_test_error,test_err)
  
  y_hat = predict(best_model, newdata = df_train)
  y_true = df_train$Purchase
  
  train_err = length(which(y_true != y_hat))/length(y_true)
  
  poly_train_error=c(poly_train_error,train_err)
}
poly_test_error
poly_train_error

### Plot the results
upper_lr = max(poly_test_error, poly_train_error)
lower_lr = min(poly_test_error, poly_train_error)

plot(poly_train_error, type = "o", lty = 2, col = "blue", ylim = c(lower_lr -1, upper_lr +1) , xlab = "cost", ylab = "error", main = "Test and Training Errors for Polynomial Kernel")
lines(poly_test_error, type = "o", lty = 1, col = "orange")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue","orange"))

