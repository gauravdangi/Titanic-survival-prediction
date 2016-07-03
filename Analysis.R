#--------------------Predicting survival in titanic-----------------------
#-----------Visualization of data----------------
library(ggplot2)


test <- read.csv("F:/data/Titanic/test.csv")
View(test)
train <- read.csv("F:/data/Titanic/train.csv")
View(train)
gendermodel <- read.csv("F:/data/Titanic/gendermodel.csv")
View(gendermodel)
genderclassmodel <- read.csv("F:/data/Titanic/genderclassmodel.csv")
View(genderclassmodel)

train$Survived <- factor(train$Survived, levels=c(1,0))
levels(train$Survived) <- c("Survived", "Died")
train$Pclass <- as.factor(train$Pclass)
levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

attach(train)

#Data Visualization

pie(table(train$Survived),col = c(3,2))

# let's see if there Survival depend on Age and Fair

ggplot(train,aes(x=Fare,y=Age,col=Survived))+geom_point(aes(shape = Survived))
# Passsengers with high fair survived more than passenngers at low fair

# Most of the Passengers belongs to which class?
ggplot(train,aes(x=Pclass))+geom_bar(aes(fill=(Pclass)))+xlab("Passenger class")

# Ratio of survived and dead passengers according to class

ggplot(train,aes(x=Pclass))+geom_bar(aes(fill=(Survived)))+xlab("Passenger class")

# 1st class passenger survived more

# Let's check relation between survival, age and fair according to passenger's class 
ggplot(train,aes(x=Fare,y=Age,col=Survived))+geom_point(aes(shape=Survived))+facet_wrap(~Pclass)

ggplot(train,aes(x=Age,col=Survived))+geom_histogram(aes(fill=Survived))+facet_wrap(~Pclass)

# Let's see relation between sex and survival

prop.table(table(train$Sex, train$Survived))
ggplot(train,aes(x=Sex))+geom_bar(aes(fill=factor(Survived)))

#This says that female(Women,children) survived more compare to male as they were rescued first

#Now as we know female rescued first therefore they survived...
#But is this applied to all passenger classes

ggplot(train,aes(x=Sex))+geom_bar(aes(fill=factor(Survived)))+
  facet_wrap(~Pclass)



#Most females survived from 1st and 2nd class
#But approximately 50% of female survived which are from 3rd class

#Now consider Embarked -> Port of Embarkation

ggplot(train,aes(x=Embarked))+geom_bar(aes(fill=(Survived)))+
  facet_wrap(~Sex)

ggplot(train,aes(x=Embarked))+geom_bar(aes(fill=(Survived)))+
  facet_wrap(~Pclass+Sex)+ggtitle("Passenger class , Sex")

#Now let's find correlation of variables with Survived variable

train$Survived<-ifelse(grepl("Died",train$Survived),0,1)
train$Survived=as.numeric(train$Survived)
cor(train$Survived,train$Fare)   #0.257
ggplot(train,aes(x=Fare,y=Survived))+geom_point()+geom_smooth()

cor(train$Survived,train$Age)    
ggplot(train,aes(x=Age,y=Survived))+geom_point()+geom_smooth()

train$Survived <- factor(train$Survived, levels=c(1,0))
levels(train$Survived) <- c("Survived", "Died")


#--------------------



#Now lets grep the name for better visualization

# I have uploaded title_fun function 
a<-NULL
for(i in 1:nrow(train)){
  
  a[i]=title_fun(train[i,4])
  
}

head(a)

train$Title=as.factor(a)

#now we can visualize better


ggplot(train,aes(x=Title))+geom_bar(aes(fill=(Survived)))

ggplot(train,aes(x=Title))+geom_bar(aes(fill=(Survived)))+
  facet_wrap(~Pclass)+theme(axis.text.x=element_text(angle=90))+
  ggtitle("Separated by Passenger class")

# Create a variable called family size = Siblings + Parch

train$family_size<-train$SibSp + train$Parch
train$family_size<-as.factor(train$family_size)

ggplot(train,aes(x=family_size))+geom_bar(aes(fill=(Survived)))

ggplot(train,aes(x=family_size))+geom_bar(aes(fill=(Survived)))+facet_wrap(~Pclass)+
  ggtitle("Separated by passenger class")

ggplot(train,aes(x=family_size))+geom_bar(aes(fill=(Survived)))+
       facet_wrap(~Pclass+Title)+ggtitle("Separated by Class,Title")

write.csv(train,"Train_title.csv")
