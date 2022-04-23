library(CrossValidation)
library(rpart)
library(rpart.plot)
library(Metrics)
submission <- read.csv('/Users/dylan/Library/CloudStorage/Box-Box/My Box Notes/Data101/earning_submission.csv')
test <- read.csv('/Users/dylan/Library/CloudStorage/Box-Box/My Box Notes/Data101/Earnings_Test_Students-1.csv')
train <- read.csv('/Users/dylan/Library/CloudStorage/Box-Box/My Box Notes/Data101/Earnings_Train2022-1.csv')

Earning_Tree<- rpart(Earnings ~ GPA+Major+Graduation_Year+Number_Of_Professional_Connections+Number_Of_Credits+Number_Of_Parking_Tickets, data = train,method = "anova", control = rpart.control(cp = 0.0000000000001, minsplit = 1, minbucket = 1),)
#rpart.plot(Earning_Tree)
#cross_validate(train,Earning_Tree,1,0.7)

train[,8] <-""
colnames(train)[8] <- "Testing"
train[,9] <-""
colnames(train)[9] <- "Difference"

prediction <- predict(Earning_Tree, test, type="vector")
submission$Earnings<-prediction 
train$Testing <- predict(Earning_Tree, train, type="vector")
write.csv(submission, 'SubmitFinal.csv', row.names=FALSE)
train[9] = (train$Earnings - train$Testing)/train$Earnings * 100

#(mean(train$Earnings) - mean(train$Testing))/mean(train$Earnings) * 100
mse(train$Earnings, train$Testing)