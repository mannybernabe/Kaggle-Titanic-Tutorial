setwd("~/Documents/GTD/Kaggle Titanic/Kaggle Titanic Tutorial")

remove(list=ls())
cat("\014")


reLoadData.fun<-function(){
#To load data sets after each new submission
train<<-read.csv("train.csv",header=T,stringsAsFactors=FALSE)
test<<-read.csv("test.csv",header=T,stringsAsFactors=FALSE)
}

writeCSV.fun<-function(fileName){
  #Function to write new Kaggle enteries
  sub<-data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
  write.csv(sub,fileName,row.names=F)
}



reLoadData.fun()

## Just Flip a Coin
set.seed(100)
test$Survived<-sample(0:1,nrow(test),replace=T)
submit<-data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
write.csv(submit,file="justguessing.csv",row.names=F)
reLoadData.fun()





## Everybody Dies
head(train)
str(train)

table(train$Survived)
round(prop.table(table(train$Survived)),2)

test$Survived<-rep(0,nrow(test))
submit<-data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
write.csv(submit,file="everybodydies.csv",row.names=F)



## "All the Ladies Make It" Model
rm(list=setdiff(ls(), "reLoadData.fun"))
reLoadData.fun()

table(train$Sex)

round(prop.table(table(train$Sex,train$Survived),1)*100,2)

test$Survived<-0
test$Survived[test$Sex=="female"]<-1
#result: 76.55%


##Considering other factors:Child
train$Child<-0
train$Child[train$Age<18]<-1


aggregate(train$Survived~train$Sex+train$Child,FUN=sum)
aggregate(train$Survived~train$Sex+train$Child,FUN=length)

aggregate(train$Survived~train$Sex+train$Child,
          FUN=function(x){round(sum(x)/length(x)*100,2)})

#Even being a child didnt much improve your chances as male


##Exploring the Fare Factor
#First need to bin a continuous variable
reLoadData.fun()
train$Fare2<-0

train$Fare2[train$Fare>30]<-"+30"
train$Fare2[train$Fare<30 & train$Fare>=20]<-"20-30"
train$Fare2[train$Fare<20 & train$Fare>=10]<-"10-20"
train$Fare2[train$Fare<10]<-"<10"

table(train$Fare2)

SurFarClassSex.table<-aggregate(Survived~Fare2+Pclass+Sex,
          data=train,
          FUN=function(x){round(sum(x)/length(x)*100,2)})

SurFarClassSex.table$Len<-aggregate(Survived~Fare2+Pclass+Sex,
          data=train,
          FUN=function(x){length(x)})$Survived

SurFarClassSex.table
#Table indicates that that women that were 3rd class and paid
#more that $20 for there ticket were much less likey to survive. 

reLoadData.fun()
test$Survived<-0
test$Survived[test$Sex=="female"]<-1
test$Survived[test$Sex=="female" & test$Fare>=20 & test$Pclass==3]<-0

writeCSV.fun("LadiesandClass.csv")
##77.99% accuracy

## Using CART decision trees 
library(rpart); library(rattle);library(rpart.plot);library(RColorBrewer)
reLoadData.fun()

CART.mod<-rpart(Survived~Pclass+
                Sex+Age+SibSp+
                Parch+Fare+Embarked,
                data=train,
                method="class")

fancyRpartPlot(CART.mod)


Prediction <- predict(CART.mod, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "CARTmodel.csv", row.names = FALSE)

#Score of 78.469%



## Reduce complexity cap and min number of passengers for split
reLoadData.fun()

CARTopen.mod<-rpart(Survived~Pclass+Sex+
        Age+SibSp+Parch+
        Fare+Embarked,data=train,
        method="class",
        control=rpart.control(minsplit = 2,cp=0))

fancyRpartPlot(CARTopen.mod)

Prediction <- predict(CARTopen.mod, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "CARTopenModel.csv", row.names = FALSE)

## Score: 74.163% :(
# Score is less regular CART Model due to overfitting


###
### Feature Engineering
###


rm(list= ls()[!(ls() %in% "reLoadData.fun")]) 
reLoadData.fun()

names(train)

train$Name[1]

test$Survived<-NA
combo.df<-rbind(train,test)

combo.df$Name<-as.character(combo.df$Name)

combo.df$Name[1]
strsplit(combo.df$Name[1],"[,.]")
strsplit(combo.df$Name[1],"[,.]")[[1]][2]
sub(' ', '', strsplit(combo.df$Name[1],"[,.]")[[1]][2])

extractTitle.fun<-function(x){
  #Function extract title and elimanates spaces.
  sub(' ', '', strsplit(x, split='[,.]')[[1]][2])
}

combo.df$Title<-sapply(combo.df$Name, FUN=extractTitle.fun)

table(combo.df$Title)
round(100*prop.table(table(combo.df$Title)),2)

combo.df$Title[combo.df$Title %in% c("Mme","Mlle")]<-"Mlle"
combo.df$Title[combo.df$Title %in% c("Capt","Don","Major","Sir")]<-"Sir"
combo.df$Title[combo.df$Title %in% c("Dona","Lady","the Countess", "Jonkheer")]<-"Lady"

combo.df$Title<-as.factor(combo.df$Title)

table(combo.df$Title)
table(combo.df$Title,combo.df$Survived)


combo.df$FamilySize<-combo.df$SibSp+combo.df$Parch+1


extractSurname.fun<-function(x){
  #Function extract surname and elimanates spaces.
  sub(' ', '', strsplit(x, split='[,.]')[[1]][1])
}
  

combo.df$Surname<-sapply(combo.df$Name,FUN=extractSurname.fun)
combo.df$FamilyID<-paste(as.character(combo.df$FamilySize),combo.df$Surname,sep=" ")

combo.df$FamilyID[combo.df$FamilySiz<=2]<-"Small"

famIDs<-data.frame(table(combo.df$FamilyID))

famIDs<-famIDs[famIDs$Freq<=2,]

combo.df$FamilyID[combo.df$FamilyID %in% famIDs$Var1]<-"Small"
combo.df$FamilyID<-factor(combo.df$FamilyID)  

#breack into train and test sets

trainMod.df<-combo.df[1:891,]
testMod.df<-combo.df[892:1309,]


CARTmodFactors.mod<-rpart(Survived~
        Pclass+Sex+Age+SibSp+Parch+
        Fare+Embarked+Title+FamilySize+FamilyID,
        data=trainMod.df,method="class")

Prediction<-predict(CARTmodFactors.mod,newdata = testMod.df,type="class")
submit <- data.frame(PassengerId = testMod.df$PassengerId, Survived = Prediction)
write.csv(submit, file = "./Competition Entries/CARTmodFactorsMod.csv", row.names = FALSE)
#Accuratcy=79.426%




####
### 
###


