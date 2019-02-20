library(randomForest)
library(gbm)
library(class)
library(glmnet)
library(caret)

rm(list=ls())
df = iris
plot(df)
str(df)

set.seed(123)
split = sample(nrow(df),nrow(df)*.8)
df_train = df[split,]
df_test = df[-split,]

## Bagging
model_bagging = randomForest(Species~.,data=df_train[,],mtry=4,importance =TRUE)
varImpPlot(model_bagging, main = 'Importance of Features, Bagging')
importance(model_bagging) ## Petal.Length and Petal.Width is the most important feature
pred_bag = predict(model_bagging, df_test)

cm_bag = confusionMatrix(pred_bag,df_test$Species)
cm_bag ## 96.67% accuracy
err_bag = sqrt(mean((as.numeric(pred_bag)-as.numeric(df_test$Species))^2))
err_bag

## Random Forest
model_rf = randomForest(Species~.,data=df_train[,],importance =TRUE)
varImpPlot(model_rf, main = 'Importance of Features, RandomForest')
importance(model_rf) ## Petal.Length  and Petal.Width is the most important feature
pred_rf = predict(model_rf, df_test)

cm_rf = confusionMatrix(pred_rf,df_test$Species)
cm_rf ## 96.67% accuracy
err_rf = sqrt(mean((as.numeric(pred_rf)-as.numeric(df_test$Species))^2))
err_rf

## Boosting
model_boost = gbm(Species~.,data=df_train,distribution="multinomial",n.trees=500, interaction.depth=4)
summary(model_boost) ## Petal.Length and Petal.Width are the most important features
pred_boost = predict(model_boost,df_test,n.trees=100)
err_boost = sqrt(mean((pred_boost-as.numeric(df_test$Species))^2))
err_boost

## KNN
df_knn = df
df_knn$Species = as.numeric(df_knn$Species)
predict_knn = knn(df_knn[split,], df_knn[-split,], df_knn[split,]$Species, k=2) 

summary(predict_knn)
table(predict_knn, df_knn[-split,]$Species)
mean((predict_knn == df_knn[-split,]$Species))
err_knn = mean(predict_knn!=df_knn[-split,]$Species)
err_knn
## Cannot use Logistic Regression because dataset has more than 2 classes in response variable.
