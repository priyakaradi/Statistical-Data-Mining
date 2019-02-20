library(ISLR)
library(MASS)
library(class)
library(neuralnet)
library(lasso2)
library(neuralnet)  
library(plyr) 
library(ElemStatLearn)
rm(list = ls())
data(spam)

# Cross validating function

crossvalidate = function(data,hidden_l=c(5))
{ # Scaling the data (min-max scaling)
  max = apply(data, 2, max) 
  min = apply(data, 2, min)
  scaled = as.data.frame(scale(data, center = mins, scale = max - min))
  
  # Initialize cv_error vector
  cv_error = NULL
  
  # Number of train-test splits
  k = 10
  
  # Cross validating
  for(j in 1:k)
  {
    # Train-test split
    index = sample(1:nrow(data),round(0.90*nrow(data)))
    train_cv = scaled[index,]
    test_cv = scaled[-index,]
    
    # NN fitting
    nn = neuralnet(f,data=train_cv,hidden=hidden_l,linear.output=T)
    
    # Predicting
    pr_nn = compute(nn,test_cv[,1:57])
    
    # Scaling back the predicted results
    pr_nn = pr_nn$net.result*(max(data$spam)-min(data$spam))+min(data$spam)
    
    # Real results
    test_cv_r = (test_cv$spam)*(max(data$spam)-min(data$spam))+min(data$spam)
    
    # Calculating MSE test error
    cv_error[j] = sum((test_cv_r - pr_nn)^2)/nrow(test_cv)
  }
  
  # Return average MSE
  return(mean(cv_error))
}


# Data
data_s = spam
data_s$spam=ifelse(data_s$spam=="spam", 1, 0)

# Initializing test and train error vectors
test_error = NULL
train_error = NULL

# Scaling for NN
maxs = apply(data_s, 2, max) 
mins = apply(data_s, 2, min)
scaled = as.data.frame(scale(data_s, center = mins, scale = maxs - mins))

n = names(scaled)
f = as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))

# Generate progress bar
pbar = create_progress_bar('text')
pbar$init(57)

set.seed(100)
# Testing and Cross validating (may take a while to compute)
for(i in 1:57){
  # Fit the net and calculate training error (point estimate)
  nn = neuralnet(f,data=scaled,hidden=c(i),linear.output=T)
  train_error[i] = sum(((as.data.frame(nn$net.result)*(50-5)+5) - (scaled$spam*(50-5)+5))^2)/nrow(scaled)
  
  # Calculate test error through cross validation
  test_error[i] = crossvalidate(data_s,hidden_l=c(i))
  
  # Step bar
  pbar$step()
}

# Print out test and train error vectors
test_error
train_error

# Plot train error
plot(train_error,main='MSE vs hidden neurons',xlab="Hidden neurons",ylab='Train error MSE',type='l',col='red',lwd=2)
# Plot test error
plot(test_error,main='MSE vs hidden neurons',xlab="Hidden neurons",ylab='Test error MSE',type='l',col='blue',lwd=2)

# Number of neurons (index) that minimizes test/train error
which(min(test_error) == test_error)
which(min(train_error) == train_error)

#######Additive Model
#install.packages("gam")
library(gam)

train = sample(1:nrow(spam), .90*nrow(spam))
spam_train = spam[train,]
spam_test = spam[-train,]

spam_additive=gam(spam ~ ., data=spam,family=binomial)
plot(spam_additive,se=T,color = "green")

pr=predict(spam_additive,newdata=spam_test)
conf_gam= table(pr>.5,spam_test$spam) 
error_rate = 1-sum(diag(conf_gam))/sum(conf_gam)
error_rate
