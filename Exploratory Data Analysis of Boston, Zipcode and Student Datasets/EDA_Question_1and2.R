##########################
## EDA
##########################
rm(list = ls())
ds1=read.csv("new_student.csv",header=TRUE)
str(ds1)

summary(ds1)

ds2=na.omit(ds1)
quartz()
hist(ds2$G1, probability = T,xlab="G1 Values", main="Histogram for G1", ylim=c(0,0.15))
lines(density(ds2$G1))

boxplot(ds1$G2, main='Boxplot for G1 Values')

hist(ds2$absences, probability = T,xlab="Absences", main="Histogram for Absences", ylim=c(0,0.16))
lines(density(ds2$absences))

plot(ds2$age, ds2$G1)      
boxplot(ds1$age, main="Boxplot for Age")

#Removing 2 outliers from Age
ds3=ds2[-c(897,280),]

#install.packages("caret")
#library(caret)

str(ds3)
graphics.off()
quartz()
par(mfrow=c(1,2))
plot(ds3$G1~ds3$absences, ds3, xlab='Number of Absences', ylab='Grade 1 Score', main="Absences vs Grades")
#with(ds3,lines(lowess(ds3$absences,ds3$G1)))

plot(log(ds3$G1)~log(ds3$absences), ds3, xlab='Log(Number of Absences)', ylab='Log(Grade 1 Score)',main="Absences vs Grades")
#with(ds3,lines(lowess(ds3$absences,ds3$G1)))


names(ds3)
library(caret)
str(ds3)
graphics.off()
quartz()
par(mfrow=c(2,3))

plot(ds3$G1~Medu+Fedu + traveltime +studytime +
       failures+
       famrel+
       freetime+
       goout+
       Dalc+
       Walc+
       health,
       data=ds3, ylab="Grades")


plot(ds3$G1~Mjob+Fjob+reason+guardian+schoolsup+famsup+paid+activities+nursery+higher+internet,
     data=ds3, ylab="Grades")

graphics.off()
quartz(title = "Relationship between Grades")
par(mfrow=c(1,3))
plot(G1~G2,data=ds3, xlab="G2 Value", ylab="G1 Value")
with(ds3,lines(lowess(ds3$G2,ds3$G1)))

plot(G3~G1,data=ds3, xlab="G1 Value", ylab="G3 Value")
with(ds3,lines(lowess(ds3$G1,ds3$G3)))

plot(G3~G2,data=ds3, xlab="G2 Value", ylab="G3 Value")
with(ds3,lines(lowess(ds3$G2,ds3$G3)))

write.table(ds3, file = "student_data.tsv", sep = "\t", row.names = FALSE)
##########################################
##Linear Regression
##########################################
library(caTools)
#set.seed(123)
ds4=ds3[,1:31]
str(ds4)
nrow(ds4)

split=sample(nrow(ds4),nrow(ds4)*.75)
training_set=ds4[split,]
test_set=ds4[-split,]

reg1=lm(formula=G1~.,data=ds4)

summary(reg1)

reg2=lm(G1~ studytime+failures+schoolsup+famsup+paid+higher+Fjob+Mjob+famsize+school+goout+health+absences,ds4 )
summary(reg2)

confint(reg2)
pred = predict(reg2, test_set)

rmse=sqrt(mean((pred - test_set$G1)^2))
rmse

reg3=lm(G1~studytime+failures+schoolsup+famsup+paid+health
          +Fjob+famsize+school+goout+absences+studytime:schoolsup+Mjob*higher,ds4)
summary(reg3)

pred3=predict(reg3,test_set)
rmse3=sqrt(mean((pred3 - test_set$G1)^2))
rmse3
