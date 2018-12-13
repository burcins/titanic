#At first, I added necessary libraries;

library(tidyverse)
library(caret)
library(e1071)
library(randomForest)
library(rpart)

#Then, read the data(at this stage, you need to change the file directory(with setwd() command) on your computer, or add an input file and put in the data for using the same code)

titanic_train <- read.csv('../input/train.csv')
titanic_test <- read.csv('../input/test.csv')
titanic <- bind_rows(titanic_train, titanic_test)
dim(titanic)
head(titanic)
tail(titanic)
glimpse(titanic)
summary(titanic)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)

#to split Title - First Name – Last Name of passengers;

titanic2 <- separate(titanic, Name, c("Last_Name", "First_Name"), sep=", ", extra="merge", convert=T)
titanic2 <- separate(titanic2, First_Name, c("Title", "First_Name"), sep=". ", extra="merge", convert=T)

#After splitting passenger names, I wonder if any Last Name matches btw passengers and is this related with passengers with any Siblings or Parents.

length(unique(titanic2$Last_Name))
titanic2 %>% subset(SibSp>=1 | Parch >= 1)

# It doesn’t seem they matched, then let's see if being with family helps to survive;

titanic2 <- mutate(titanic2, w_family = ifelse((SibSp >=1 | Parch >= 1), TRUE, FALSE))
ggplot(titanic2, aes(w_family, Survived, col=Sex))+geom_jitter() + xlab("With Family") + ggtitle("Figure 1 : Being Family vs. Survive")



#It seems that it has some effects, ok than keep it and go with title cleaning;

table((titanic2$Title))
titanic3 <- titanic2 
titanic3$Title[titanic3$Title=="Major"] <- "Spc"
titanic3$Title[titanic3$Title=="Capt"] <- "Spc"
titanic3$Title[titanic3$Title=="Col"] <- "Spc"
titanic3$Title[titanic3$Title=="Don"] <- "Spc"
titanic3$Title[titanic3$Title=="Dona"] <- "Spc"
titanic3$Title[titanic3$Title=="Dr"] <- "Spc"
titanic3$Title[titanic3$Title=="Jonkheer"] <- "Spc"
titanic3$Title[titanic3$Title=="Lady"] <- "Spc"
titanic3$Title[titanic3$Title=="Master"] <- "Spc"
titanic3$Title[titanic3$Title=="Mlle"] <- "Spc"
titanic3$Title[titanic3$Title=="Mme"] <- "Spc"
titanic3$Title[titanic3$Title=="Rev"] <- "Spc"
titanic3$Title[titanic3$Title=="Sir"] <- "Spc"
titanic3$Title[titanic3$Title=="th"] <- "Spc"
titanic3$Title[titanic3$Title=="Ms"] <- "Miss"
titanic3$Title[titanic3$Title=="Mrs"] <- "Miss"
table(titanic3$Title)

#now it is time to handle with fill missing data,

sapply(titanic3, function(x) {sum(is.na(x))})
titanic4 <- titanic3
titanic4$Age[is.na(titanic3$Age)] <- round(mean(titanic4$Age, na.rm=T),0)
head(titanic4)
titanic5 <- subset(titanic4, select = -c(Cabin))
titanic5$Fare[is.na(titanic5$Fare)] <- median(titanic5$Fare, na.rm=TRUE)
table(titanic5$Embarked)
titanic5$Embarked[is.na(titanic5$Embarked)] <- "S"
sapply(titanic5, function(x) {sum(is.na(x))})
summary(titanic5)
titanic5%>% ggplot(aes(Fare,Age, position="dodge"))+geom_boxplot()
sum(titanic5$Fare>100)

#Not seems any outlier.

#Let’s check the correlation between Fare and Survival rate,
ggplot(titanic5, aes(Fare, Survived,col=Pclass))+geom_jitter() + ggtitle("Figure 2 : Fare vs. Survive")

#For analyzing survivors we need to separate them from our test data, which has not any survived information as expected;

titanic_train2 <- titanic5[1:891,]
titanic_test2 <- titanic5[892:1309,]
ggplot(titanic_train2, aes(Pclass,fill=Survived))+geom_bar(position="dodge")+facet_grid(Title~.,)+ ggtitle("Figure 3 : Effect of Pclass & Title to Survive")
ggplot(titanic_train2, aes(Pclass,Title, col= Survived))+geom_jitter()+facet_grid(Embarked~.,)+ ggtitle("Figure 4 : Effect of Pclass & Title & Embarked to Survive")

#Let’s say from graph that, if 
#1.	you have specific title and embarked to the ship from C, 
#2.	you have “Miss” title and your Pclass is 1 or 2, 
#than you have a big chance to survive. 
#But it seems that if a passenger’s title is “Mr”, unfortunately no matter its Embark or Pclass it has lower chances to live.
#In order to analyse “Mr” titled people, I want to check total range first;

titanic_train2%>% ggplot(aes(Sex,Age, col=Survived, position="jitter"))+geom_point() + ggtitle("Figure 5 : Effect of Pclass & Title & Embarked to Survive")

#It shown from graph that, most of men with “Mr” title and btw age 20 to 40 have less chance to live.
#Lets try to see more at this age range;
All_20_40 <- titanic_train2 %>% subset(Age > 20 & Age <40 & Sex=="male")
Lucky_guys <- titanic_train2 %>% subset(Age > 20 & Age <40 & Survived == 1 & Sex=="male")
ggplot(Lucky_guys, aes(Pclass,fill=Title, position="jitter"))+geom_bar(position="dodge") + ggtitle("Figure 6 : How lucky are our Lucky_Guys 1")
ggplot(Lucky_guys, aes(Pclass,fill=Title))+geom_bar(position="dodge")+facet_grid(Embarked~.,) + ggtitle("Figure 7 : How lucky are our Lucky_Guys 2")
ggplot(Lucky_guys, aes(Age,fill=Title))+geom_bar(position="dodge")+facet_grid(Pclass~.,) + ggtitle("Figure 8 : How lucky are our Lucky_Guys 3")
ggplot(All_20_40, aes(Age,Embarked, col=Title))+geom_jitter()+facet_grid(Pclass~.,) + ggtitle("Figure 9 : How lucky are our Lucky_Guys 4")

#It seems from our graph that, some of passengers grouped in Lucky_guys data might not be in this class, because we filled our NA data in the Age column with Mean of Age data, which was 30. It may misguide us. 
#At last I also want to check, is there any survivor on board with their family;

(Lucky_guys2 <- Lucky_guys %>% subset(SibSp>=1 | Parch >= 1))
ggplot(Lucky_guys2, aes(Pclass, fill=Title))+geom_bar() + ggtitle("Figure 10 : How lucky are our Lucky_Guys 5")

#After this stage, it may help us to predict passengers due to their Generation, for using this data, we can clarify ages btw 20-40 with separate our data to 3 parts due to passenger ages. But we should apply this to all data, rather than our train data, if we plan to use this for making predictions.

titanic5 <- mutate(titanic5, Generation = ifelse(Age < 20, "Young", ifelse(Age<40, "Mature", "Old")))
table((titanic5$Generation))

#After that we can turn back to our train set again,
titanic_train2 <- titanic5[1:891,]
titanic_test2 <- titanic5[892:1309,]

#Now we can check our new Generation column,
ggplot(titanic_train2, aes(Generation, fill=Survived))+geom_bar()+facet_grid(Title~Pclass,) + ggtitle("Figure 11 : Generation vs Survived")
#OK, now we can easily say that nearly all of Misses and Specific Titled people under 20 survived. 

#After all, its prediction time, but firstly I split 20% of our train data, for test our model to avoid overfitting. 
Split <- round(nrow(titanic_train2)*0.8,0)
titanic_train3 <- titanic_train2[1:Split, ]
titanic_trtest <- titanic_train2[(Split+1):nrow(titanic_train2), ]

#Logistic Regression
model <- glm(Survived~Pclass + Title + Sex + Embarked + Generation + w_family, family = "binomial", titanic_train3)
p <- predict(model, titanic_train3, type="response")
p_class <- ifelse(p>0.50, "1", "0")
confusionMatrix(p_class, titanic_train3$Survived)

p_test <- predict(model, titanic_trtest, type="response")
p_testclass <- ifelse(p_test>0.50, "1", "0")
confusionMatrix(p_testclass, titanic_trtest$Survived)


#Decision Tree
model_dt <- rpart(Survived~Pclass + Title + Sex + Embarked + Generation + w_family, titanic_train3)
p_dt <- predict(model_dt, titanic_train3, type="class")
confusionMatrix(p_dt, titanic_train3$Survived)

p_dttest <- predict(model_dt, titanic_trtest, type="class")
confusionMatrix(p_dttest, titanic_trtest$Survived)

#Random Forest
model_rf <- randomForest(Survived~Pclass + Sex + Age + w_family, titanic_train3)
p_rf <- predict(model_rf, titanic_train3)
confusionMatrix(p_rf, titanic_train3$Survived)

p_rftest <- predict(model_rf, titanic_trtest)
confusionMatrix(p_rftest, titanic_trtest$Survived)

#SVM
trctrl <- trainControl(method = "cv", number = 10, repeats = 3)

model_svm <- train(Survived ~Pclass + Sex + Age + w_family, data = titanic_train3, method = "svmRadial",
                   trControl=trctrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 10)

p_svm <- predict(model_svm, titanic_train3)
confusionMatrix(p_svm, titanic_train3$Survived)

p_svmtest <- predict(model_svm, titanic_trtest)
confusionMatrix(p_svmtest, titanic_trtest$Survived)

#In conclusion, although all three tested models predict close results from our test data due to their accuracy rate, 
#SVM model gives the best result with unknown data(our test data, which was split from given train data set). 
#In this case, we can decide to use this model for our prediction for real test data.

p_rf_last <- predict(model_rf, titanic_test2)
result <- data.frame(PassengerId = titanic_test2$PassengerId, Survived = p_rf_last)

# Write results & Submit time

write.csv(result, file = 'my_predictions.csv', row.names = F)
