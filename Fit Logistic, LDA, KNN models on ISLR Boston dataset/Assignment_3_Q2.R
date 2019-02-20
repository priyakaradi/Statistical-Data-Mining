########################
## Question 2
#######################
rm(list = ls())
library("MASS")
library("caret")
ds = read.table('Diabetes.txt')
ds1 = ds[,5:10]
str(ds1)
summary(ds1)

split = sample(nrow(ds1), nrow(ds1)*0.75)
#names(ds1$V5)='glucose.area'
#names(ds1$V6)='insulin.area'
#names(ds1$V7)='SSPG'
#names(ds1$V8)='relative.weight'
#names(ds1$V9)='fasting.plasma.glucose'

#colnames(data)[colnames(data)=="old_name"] <- "new_name"

pairs(ds1[1:5])

#colours = character(nrow(ds1))
colours[]="black"
colours[ds1$V10==3] = 'pink'
colours[ds1$V10==2] = 'orange'
pairs(ds1[1:5], col=colours, main='Correlation Matrix')

cor(ds1[1:5])
train_set = ds1[split,]
test_set = ds1[-split,]

model_lda = lda(train_set$V10 ~.,data = train_set)
summary(model_lda)
predict_train_lda = predict(model_lda, train_set)
predict_test_lda = predict(model_lda, test_set)

str(ds1)
cm_lda = confusionMatrix(as.factor(train_set$V10),as.factor(predict_train_lda$class))
cm_lda

##############################################
## QDA
##############################################
model_qda = qda(train_set$V10 ~.,data = train_set)
summary(model_qda)
predict_train_qda = predict(model_qda, train_set)
predict_test_qda = predict(model_qda, test_set)

cm_qda = confusionMatrix(as.factor(train_set$V10),as.factor(predict_train_qda$class))
cm_qda

##############################################
##Test new point
##############################################
ds2 = data.frame(0.98,122,544,186,184)
colnames(ds2)=c('V5','V6','V7','V8','V9')
ds2

tes_point_lda = predict(model_lda, ds2)
tes_point_qda = predict(model_qda, ds2)

tes_point_lda$class
tes_point_qda$class
