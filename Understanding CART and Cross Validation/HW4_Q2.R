library(rpart)
library(rpart.plot)
library(MASS)
#Load data, rename columns and classes
rm(list=ls())
df_wine = read.csv('wine.data.csv',header = FALSE)
colnames(df_wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                         'Alcalinity', 'Magnesium', 'Phenols', 
                         'Flavanoids', 'Nonflavanoids',
                         'Proanthocyanins', 'Color', 'Hue', 
                         'Dilution', 'Proline')
df_wine$Type[which(df_wine$Type==1)] <- "Barbera"
df_wine$Type[which(df_wine$Type==2)] <- "Barolo"
df_wine$Type[which(df_wine$Type==3)] <- "Grignolino"

#Split data
set.seed(123)
split = sample(1:nrow(df_wine), nrow(df_wine)*0.80)
df_train = df_wine[split,]
df_test = df_wine[-split,]

#Create tree model
tree_model<-rpart.control(minsplit=10,xval=10)

#Create tree for train samples
model_tree_train<-rpart(Type~.,data=df_train,control=tree_model)
rpart.plot(model_tree_train, nn=TRUE)
summary(model_tree_train)

#Create tree for test samples
model_tree_train<-rpart(Type~.,data=df_test,control=tree_model)
rpart.plot(model_tree_train, nn=TRUE)
summary(model_tree_train)