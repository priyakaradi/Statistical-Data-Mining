library(corrplot)
library(dplyr)
library(MASS)
library(lattice)
data(Boston)
?Boston
#Boston_data = subset(Boston)
write.table(Boston, file = "Boston_data", sep = "\t", row.names = FALSE, col.names = names(Boston))
summary(Boston)
str(Boston)


######################
## Part A
######################
pairs(Boston)
graphics.off()
quartz()

pairs(Boston)
pairs(~ medv + lstat, data = Boston)

xyplot(medv ~ sqrt(crim), data=Boston, groups = chas, auto.key = list(columns=2))
xyplot(medv ~ sqrt(indus), data=Boston, groups = chas, auto.key = list(columns=2))

xyplot(medv ~ lstat, data = Boston)



######################
## Part B
######################
cor_boston <- select(Boston,crim,zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,black,lstat,medv)
M <- cor(cor_boston)
quartz()
corrplot(M, method="number")
plot_cor <- cor(cor_Boston)

####Subdivision C####
graphics.off()
quartz()
xyplot(crim ~ chas, data = Boston, group = chas , type = c("p","smooth"), auto.key=list(columns=2))
xyplot(ptratio ~ chas, data = Boston, group = chas , type = c("p","smooth"), auto.key=list(columns=2))
xyplot(tax ~ chas, data = Boston, group = chas , type = c("p","smooth"), auto.key=list(columns=2))

?Boston()
subset_one <- subset(Boston, chas==1)
summary(subset_one)
subset_zero <- subset(Boston, chas==0)
summary(subset_zero)

############Subdiviion  D ##############
subset_rm7<-subset(Boston, rm>7) #d
nrow(subset_rm7)
subset_rm8<-subset(Boston, rm>8)
nrow(subset_rm8)
summary(subset_rm8)
plot(subset_rm8)
