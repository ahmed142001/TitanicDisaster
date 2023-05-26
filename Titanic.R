#reading  data
data <- read.csv("train.csv")
test <- read.csv("test.csv")
#viewing the data in dataframe
View(data)
View(test)
#data information 
str(data)
summary(data)
#Passengers survived vs passengers passed away
table(data$Survived)
#propability
prop.table(table(data$Survived))
#propability of survived or not with respect to gender
prop.table(table(data$Sex,data$Survived),margin=1)
#so almost 74% of females survived against only 18% of males
#checking nulls in every coloumn
colSums(is.na(data))
colSums(is.na(test))
#drop unused coloumn 
library(dplyr)
data<-data %>% select(-Name,-Ticket,-Cabin,-take.off)
View(data)  
test<-test %>% select(-Name,-Ticket,-Cabin,-take.off)
View(test)
#convert sex to numerical value and to factor of distinct values
data$Sex<-as.factor(as.numeric(as.factor(data$Sex)))
data$Pclass<-(as.factor(data$Pclass))
data$SibSp<-(as.factor(data$SibSp))
data$Parch<-(as.numeric(data$Parch))
View(data) 
test$Sex<-as.factor(as.numeric(as.factor(test$Sex)))
test$Pclass<-(as.factor(test$Pclass))
test$SibSp<-(as.factor(test$SibSp))
test$Parch<-(as.numeric(test$Parch))
View(test)
summary(data)
#filling nulls
#missForest initially imputes all missing data using the mean/mode
library(missForest)
data.imp = missForest(data)
test.imp = missForest(test)
data = data.imp$ximp
test = test.imp$ximp
colSums(is.na(data))
colSums(is.na(test))
#make survived coloumn as a factor
data$Survived<-as.factor(data$Survived)
View(data)


#random forrest classifier
library(randomForest)
classifier = randomForest(x = data[,c('Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare')], y = data$Survived, ntree = 10)


print(classifier)

#predict
prediction = predict(classifier, test)
output = data.frame(PassengerId = test$PassengerId, Survived = prediction)
summary(output)
write.csv(output, file = 'firstAttemp.csv', row.names = FALSE)

