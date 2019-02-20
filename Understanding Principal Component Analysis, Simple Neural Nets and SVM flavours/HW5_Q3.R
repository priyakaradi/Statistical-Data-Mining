#install.packages("neuralnet")
library(neuralnet)
rm(list=ls())
data = read.csv("cereals.csv", header=T)

str(data)
summary(data)

# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample(nrow(data),nrow(data)*.60)

datatrain = data[index,]
datatest = data[-index,]
## Scale data for neural network
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
#set.seed(2)
for (i in seq(1,5)){
  NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = i , linear.output = T )
  
  #Predict
  predict_testNN = compute(NN, testNN[,c(1:5)])
  predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)
  # Calculate Root Mean Square Error (RMSE)
  RMSE.NN = (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5
  
  # plot neural network
  plot(NN)
  plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating", main = paste("Neurons: ",i,"RMSE: ",RMSE.NN))
  abline(0,1)
  
  print(i)
  print(RMSE.NN)
}
predictors = names(data)[-6]
#names(NN)
#NN$result.matrix
out <- cbind(as.matrix(NN$covariate), NN$net.result[[1]])
colnames(out) <- c(predictors, "nn-output")

############################
# Creating outlier
data_outlier = data
data_outlier[69,]$calories = 1400
boxplot(data_outlier)
max = apply(data_outlier , 2 , max)
min = apply(data_outlier, 2 , min)
scaled_outlier = as.data.frame(scale(data_outlier, center = min, scale = max - min))

boxplot(scaled_outlier)

trainNN_out = scaled_outlier[index , ]
testNN_out = scaled_outlier[-index , ]

# fit neural network
#set.seed(2)
for (i in seq(1,5)){
  NN_out = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN_out, hidden = i , linear.output = T )
  
  #Predict
  predict_testNN_out = compute(NN_out, testNN_out[,c(1:5)])
  predict_testNN_out = (predict_testNN_out$net.result * (max(data_outlier$rating) - min(data_outlier$rating))) + min(data_outlier$rating)
  
  # Calculate Root Mean Square Error (RMSE)
  RMSE.NN_out = (sum((datatest$rating - predict_testNN_out)^2) / nrow(datatest)) ^ 0.5
  
  # plot neural network
  plot(NN_out)
  plot(datatest$rating, predict_testNN_out, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating", main=paste("Outlier Value: ",data_outlier[69,]$calories,", Neurons: ",i,", RMSE: ",RMSE.NN_out))
  abline(0,1)
  print(i)
  print(RMSE.NN_out)
}

out2 <- cbind(as.matrix(NN_out$covariate), NN_out$net.result[[1]])
colnames(out2) <- c(predictors, "nn-output")

###### Running on Outlier Neural Net
ds = data_outlier
quartz()
for (i in seq(from=1400, to=140, by=-150)){
  ds[69,]$calories = i
  max = apply(ds , 2 , max)
  min = apply(ds, 2 , min)
  scaled_ds = as.data.frame(scale(ds, center = min, scale = max - min))
  trainNN_ds = scaled_ds[index , ]
  testNN_ds = scaled_ds[-index , ]
  
  #NN_ds = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN_ds, hidden = 1 , linear.output = T )
  predict_testNN_ds = compute(NN_out, testNN_ds[,c(1:5)])
  predict_testNN_ds = (predict_testNN_ds$net.result * (max(ds$rating) - min(ds$rating))) + min(ds$rating)
  RMSE.NN_ds = (sum((datatest$rating - predict_testNN_ds)^2) / nrow(datatest)) ^ 0.5
  print(i)
  print(RMSE.NN_ds)
  plot(datatest$rating, predict_testNN_ds, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating", main= paste("Outlier Correction Value:", i, "RMSE: ",RMSE.NN_ds))
  abline(0,1)
  
}

###### Running on original Neural Net
ds = data_outlier
for (i in seq(from=1400, to=140, by=-150)){
  ds[69,]$calories = i
  max = apply(ds , 2 , max)
  min = apply(ds, 2 , min)
  scaled_ds = as.data.frame(scale(ds, center = min, scale = max - min))
  trainNN_ds = scaled_ds[index , ]
  testNN_ds = scaled_ds[-index , ]
  
  #NN_ds = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN_ds, hidden = 1 , linear.output = T )
  predict_testNN_ds = compute(NN, testNN_ds[,c(1:5)])
  predict_testNN_ds = (predict_testNN_ds$net.result * (max(ds$rating) - min(ds$rating))) + min(ds$rating)
  RMSE.NN_ds = (sum((datatest$rating - predict_testNN_ds)^2) / nrow(datatest)) ^ 0.5
  print(i)
  print(RMSE.NN_ds)
  plot(datatest$rating, predict_testNN_ds, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating", main= paste("Outlier Correction Value:", i, "RMSE: ",RMSE.NN_ds))
  abline(0,1)
  
}


out <- cbind(as.matrix(NN$covariate), NN$net.result[[1]])
colnames(out) <- c(predictors, "nn-output")
