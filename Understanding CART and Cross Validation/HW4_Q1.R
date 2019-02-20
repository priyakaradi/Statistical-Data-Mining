install.packages("fitdistrplus")
library(ElemStatLearn)
library(glmnet)
library(pls)
library(leaps) #for best-subset selection
library(fitdistrplus) #for normal distribution


rm(list=ls())
set.seed(123)
df = prostate[,-10] #last colummn indicates trainig data, so remove
#df = prostate
#?prostate
str(df) #column 9 is response variable
sum(is.na(df))

descdist(df$lpsa, discrete = FALSE)

chk_diststribution = fitdist(df$lpsa, "norm")
plot(chk_diststribution) # Distribution is normal

best_subset = regsubsets(lpsa~.,data = df,method="exhaustive") #By default it creates nvmax=8 models
res_regsubset = summary(best_subset)
res_regsubset$cp
res_regsubset$bic

quartz()
par(mfrow = c(2,2))
plot(res_regsubset$rss, xlab="Number of variables", ylab = "RSS",  type = "l")
plot(res_regsubset$adjr2, xlab="Number of variables", ylab = "Adjusted R^2", type = "l")
which.max (res_regsubset$adjr2) #7
points (7, res_regsubset$adjr2[7], col ="red",cex =2, pch =20)
plot(res_regsubset$cp, xlab="Number of variables", ylab = "Cp", type = "l")
which.min (res_regsubset$cp)   ##### cp=5 variable model is best
points (5, res_regsubset$cp[5], col ="red",cex =2, pch =20)
min (res_regsubset$cp)  ####### min Cp 5.816804

plot(res_regsubset$bic, xlab="Number of variables", ylab = "BIC", type = "l")
which.min (res_regsubset$bic)   ##### bic=3 variable model is best
points (3, res_regsubset$bic[3], col ="red",cex =2, pch =20)
min (res_regsubset$bic) ####### min bic -79.71614
graphics.off()

## Creating Function
predict_regsubsets()
predict_regsubsets =function (object ,newdata ,id ,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

## 5-fold CV
###############################
## Split into Train and Test
###############################
split = sample(nrow(df),nrow(df)*.8)
df_train = df[split,]
df_test = df[-split,]

best_subset_5cv= regsubsets(lpsa~.,data = df_train,method="exhaustive") #By default max=8 best subsets will be returned
test_mat = model.matrix(lpsa~.,df_test)
test_err = rep(NA,8) #Total 8 predictors and 1 response.
#y_train = df_train[9] #Response variable
y_test = df_test$lpsa
y_pred=model.matrix(lpsa~.,df_test)
for (i in 1:8){
  coefi = coef(best_subset_5cv, id = i)
  y_pred = test_mat[,names(coefi)]%*%coefi  #Matrix Multiplication
  test_err[i] = (mean((y_test - y_pred)^2))
}

test_err
which.min(test_err)

## 10-fold CV ### Will still create max 8 subsets
split = sample(nrow(df),nrow(df)*.9)
df_train = df[split,]
df_test = df[-split,]
best_subset_10cv= regsubsets(lpsa~.,data = df_train,method="exhaustive") #By default max=8 best subsets will be returned
test_mat10 = model.matrix(lpsa~.,df_test)
test_err10 = rep(NA,8) #Total 8 predictors and 1 response.
#y_train = df_train[9] #Response variable
y_test10 = df_test$lpsa
y_pred10 = model.matrix(lpsa~.,df_test)
for (i in 1:8){
  coefi10 = coef(best_subset_10cv, id = i)
  y_pred10 = test_mat[,names(coefi10)]%*%coefi10  #Matrix Multiplication
  test_err10[i] = (mean((y_test10 - y_pred10)^2))
}

test_err
which.min(test_err)
test_err10
which.min(test_err10)



# Bootstrap
library(bootstrap)
best_subset_boot = regsubsets(lpsa~.,df, method = 'exhaustive')
boot_summary_outmat = summary(best_subset_boot)$outmat

theta_fit = function(X,Y){
  lsfit(X,Y)	
}

theta_predict = function(fit, X){
  cbind(1,X)%*%fit$coef
}

sqr_error = function(Y,Yhat){
  (Y-Yhat)^2
}

err_list = c()
for (i in 1:8){
  temp = which(boot_summary_outmat[i,] == "*") # Pulling out the model
  result = bootpred(df[,temp], df$lpsa, nboot = 50, theta.fit = theta_fit, theta.predict = theta_predict, err.meas = sqr_error) 
  err_list = c(err_list, result[[3]])
  
}
err_list
which.min(err_list)
