#-----------------------Predicting survival in titanic-----------------------

train <- read.csv("F:/data/Titanic/train.csv")
a<-NULL
for(i in 1:nrow(train)){
  
  a[i]=title_fun(train[i,4])
  
}

head(a)

train$Title=as.factor(a)

train$family_size<-train$SibSp + train$Parch
train$family_size<-as.factor(train$family_size)



train.new<-data.frame(train$Survived,train$Pclass,train$Age,train$family_size,train$Fare,train$Title)
names(train.new)=c("Survived","Pclass","Age","family_size","Fare","Title")
train.new$Survived<-as.factor(train.new$Survived)
train.new$family_size<-as.factor(train.new$family_size)
train.new$Pclass<-as.factor(train.new$Pclass)



#train.new$Title<-as.factor(train.new$Title)
#train.new$Pclass<-as.factor(train.new$Pclass)

attach(train.new)

# Amelia -> For Dealing with missing data in our dataset
library(Amelia)
library(caret)
library(data.table)

AmeliaView()

amelia(x = getAmelia("amelia.data"), m = 5, idvars = c("Name", "Sex", "Ticket", "Cabin", "Embarked"), ts = NULL, cs = NULL, 
       priors = NULL, lags = NULL, empri = 0, intercs = FALSE, leads = NULL, 
       splinetime = NULL, logs = NULL, sqrts = NULL, lgstc = NULL, 
       ords = NULL, noms = NULL, bounds = NULL, max.resample = 1000, 
       tolerance = 1e-04)

#train.imp1 is a imputed form of train dataset
train.imp1 <- read.csv("F:/Projects/R Projects/Titanic survival prediction/train/Train_title-imp1.csv")
train.imp1$Age <- round(train.imp1$Age)
train.imp1$family_size<-train.imp1$SibSp+train.imp1$Parch

train.imp1$Title=train$Title
train.new$Survived<-as.factor(train.new$Survived)
train.imp1$Pclass=as.factor(train.imp1$Pclass)
train.imp1$Title=as.factor(train.imp1$Title)

sapply(train.imp1,class)

train.new<-data.frame(train.imp1$Survived,train.imp1$Pclass,train.imp1$Sex,train.imp1$Age,train.imp1$family_size,train.imp1$Fare,train.imp1$Title)
names(train.new)=c("Survived","Pclass","Sex","Age","family_size","Fare","Title")
train.new$Survived<-as.factor(train.new$Survived)
train.new$family_size<-as.factor(train.new$family_size)
sapply(train.new,class)

# Creating partitions
t<-createDataPartition(y=train.new$Survived,p=0.75,list=F)
training<-train.new[t,]
testing<-train.new[-t,]
training<-data.table(training)
testing<-data.table(testing)
sapply(training,class)
sapply(testing,class)

#XXXXXXXXXXXXXXXXXXXXXXXX---Let's use Logistic regression first----XXXXXXXXXXXXXXXXXXXXXXX

model1<-glm(Survived~.,data=training,family = binomial)

summary(model1)


q<-predict(model1,testing,type="response")
q<-round(q)
confusionMatrix(q,testing$Survived)
accuracy<-(1-mean(q!=testing$Survived))*100
accuracy

#Accuracy = 82.43 % on cross validation


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Support Vector Machine---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

require(e1071)

tuned<-tune(svm,Survived~.,data=training,ranges = list(cost=c(0.001,0.01,0.1,1,100)),scale = F)
summary(tuned)   
#This tells that best parameter for cost is 100

model2<-svm(Survived~.,data=training,cost=100,scale = F,probability = T)
model2

q<-predict(model2,testing)
confusionMatrix(q,testing$Survived)
accuracy<-(1-mean(q!=testing$Survived))*100
accuracy


#Accuracy = 71.62 % on cross validation


#XXXXXXXXXXXXXXXXXXXXXXXXXX-------Naive-Bayes--------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

require(e1071)
model3<-naiveBayes(Survived~.,data=training,laplace = 1)
model3
str(model3)

summary(model3)
q<-predict(model3,testing)
q
confusionMatrix(q,testing$Survived)
accuracy<-(1-mean(q!=testing$Survived))*100
accuracy

#77.477%


#XXXXXXXXXXXXXXXXXXXXXXXXXXXX----------KNN----XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-----

library(class)

cl<-factor(c(1,0))
pred5 <-knn((train.new),cl,k=4,prob = T)

#--XXXXXXXXXXXXXXXXXXXXXXXXXX-----Random Forest----XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX------

library(randomForest)
set.seed(2017)
model5<-randomForest(Survived~.,data=training,mtry=5,ntree=900,importance = T, proximity = T)

print(model5)
round(importance(model5),2)
varImpPlot(model5,2)
str(model5)

q<-predict(model5,testing)
confusionMatrix(q,testing$Survived)

accuracy<-(1-mean(q!=testing$Survived))*100
accuracy# --------- 81.1% --------


#--XXXXXXXXXXXXXXXXXXXXXXXXXX-Generalized Boosted Regression Modeling-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX------
library(gbm)
set.seed(2131)
model6<-train(training$Survived~.,data=training,model<-"gbm")
varImp(model6)

#TitleMr.      100.0000
#Sexmale        54.9511
#Fare           52.2655
#Pclass3        37.0735
#Age            23.7799

q<-predict(model6,testing)
accuracy<-(1-mean(q!=testing$Survived))*100
accuracy# --------- 83.78% --------
#it's accuracy is the highest
#let's train the whole dataset by this model
set.seed(2131)
model6<-train(train.new$Survived~.,data=train.new,method="gbm")



#---XXXXXXXXXXXXXXXXXXXXXXXXXXXX-Linear Discriminant Analysis Classification-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
library(MASS)
set.seed(2131)
model7 <- train(training$Survived~.,data=training,model="lda")
varImp(model7)


q<-predict(model7,testing)
accuracy<-(1-mean(q!=testing$Survived))*100
accuracy# --------- 81% --------


#-----XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-------
#-----XXXXXXXXXXXXXXXXXXXXXXX---(PREDICTION)---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-------
#-----XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-------

test$family_size<-test$SibSp+test$Parch
test1<-data.frame(test$Pclass,test$Name,test$Age,test$family_size,test$Fare)
names(test1)=c("Pclass","Name","Age","family_size","Fare")

a<-NULL
for(i in 1:nrow(test)){
  
  a[i]=title_fun(test[i,3])
  
}


head(a)
test1$Name<-NULL
test1$Title=as.factor(a)

sapply(test1,class)

# Imputing test dataset
write.csv(test1,"test1.csv")
test1.imp1 <- read.csv("F:/Projects/R Projects/Titanic survival prediction/test/test1-imp1.csv")
test1<-test1.imp1[,-1]

test1$Pclass<-as.factor(test1$Pclass)
test1$family_size<-as.factor(test1$family_size)
test1$Pclass<-as.factor(test1$Pclass)

sapply(test1,class)

#Now Predict Survival for test dataset by Support Matrix machine algorithm



levels(train.new$Sex) = levels(test1$Sex)
levels(train.new$family_size) = levels(test1$family_size)
sapply(test1,class)

#---------------Predicting using pred1 model(Logistic Regression)------------------
#Logistic Regression
a<-round(predict(pred1,test1,type = "response"))
head(a)


genderclassmodel$Logistic_Regr.<-a

#-----------------------SVM Prediction(pred2 and pred3)-----------------------------

s<-predict(pred2,test1,probability = T)

genderclassmodel$SVM<-s 

#--------------------------------------------------------

levels(test$Name) <- levels(train$Name)
levels(test$Sex) <- levels(train$Sex)
levels(test$Ticket) <- levels(train$Ticket)
levels(test$Cabin) <- levels(train$Cabin)
levels(test$Embarked) <- levels(train$Embarked)

predict(pred3,test,type="class")

#-----------------------------------Naive Bayes------------------------------------

b<-predict(pred4,test1)
head(b)

genderclassmodel$Naive_Bayes<-b

#-----------------------------------KNN-----------------------------------------------



#------------------------------Random Forest-----------------------------------------

a<-predict(pred6,test1)
head(a)

#----------------------

genderclassmodel$Random_Forest <- a

#--------------------------------gbm------------------------------------------

b<-predict(model6,test1)
prediction<-data.frame(test$PassengerId,b)
names(prediction)<-c("PassengerId","Survived")
write.csv(prediction,"gbm.csv")
rm(prediction)


#--------------Linear Discriminant Analysis Classification----------------
b<-predict(pred8,test1)

genderclassmodel$lda<-b
lda_data<-genderclassmodel[,c(1,8)]
head(lda_data)
names(lda_data)<-c("PassengerId","Survived")
write.csv(lda_data,file = "lda.csv")
rm(lda_data)


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#----------------------Writing csv files for kaggle-----------------------------------------

SVM_model<-genderclassmodel[,c(1,6)]
head(SVM_model)
names(SVM_model)<-c("PassengerId","Survived")
write.csv(SVM_model,"SVM_Model.csv")
rm(SVM_model)

DATA<-genderclassmodel[,c(1,5)]
names(DATA)<-c("PassengerId","Survived")
write.csv(DATA,file = "RandomForest1.csv")
rm(DATA)

DATA_Naive<-genderclassmodel[,c(1,4)]
names(DATA_Naive)<-c("PassengerId","Survived")
write.csv(DATA_Naive,file = "Naive_Bayes1.csv")
rm(DATA_Naive)

Logistic_Reg<-genderclassmodel[,c(1,3)]
names(Logistic_Reg)<-c("PassengerId","Survived")
write.csv(Logistic_Reg,file = "Logistic_Reg3.csv")
rm(Logistic_Reg)

gbm_data<-genderclassmodel[,c(1,7)]
head(gbm_data)
names(gbm_data)<-c("PassengerId","Survived")
write.csv(gbm_data,file = "gbm.csv")
rm(gbm_data)


