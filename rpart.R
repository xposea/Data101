library(CrossValidation)
library(rpart)
library(rpart.plot)
final <- read.csv("C:/Users/Dylan/Downloads/M2022testSNoGrade.csv")
tester <- read.csv('/Users/dylan/Library/CloudStorage/Box-Box/My Box Notes/earning_submission.csv')
train <- read.csv("C:/Users/Dylan/Downloads/M2022train.csv")

trainSet <- sample(2,nrow(train), replace = T, prob = c(0.7,0.3))
training <- train[trainSet==1,]
testing <- train[trainSet==2,]

Grade_Tree<- rpart(Grade ~ Score+Major+Seniority, data = training,method = "class", control = rpart.control(cp = 0.0000001, minsplit = 4, minbucket = 2),)
rpart.plot(Grade_Tree)
cross_validate(train,Grade_Tree,20,0.7)

#printcp(Grade_Tree)
#plotcp(Grade_Tree)

#tree.fit=prune(Grade_Tree, cp = 0.0029155)
#rpart.plot(tree.fit, cex=.45)

pruneTree <- prune(Grade_Tree, cp = Grade_Tree$cptable[which.min(Grade_Tree$cptable[,"xerror"]),"CP"])

rpart.plot(pruneTree)

cross_validate(train,pruneTree,20,0.75)
prediction <- predict(pruneTree, final, type="class")
tester$Grade<-prediction 
write.csv(tester, 'SubmitFinal.csv', row.names=FALSE)

