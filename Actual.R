train <- read.csv("C:/Users/Dylan/Downloads/M2022testSNoGrade.csv")
tester <- read.csv("C:/Users/Dylan/Downloads/tester.csv")

#train[,2] <-""
colnames(train)[5] <- "Testing"
train[,6] <-""
colnames(train)[6] <- "Correct?"
#train[train$Seniority == "Freshman",4] = "1"
#train[train$Seniority ==  "Sophomore",4] = "2"
#train[train$Seniority == "Junior",4] = "3"
#train[train$Seniority == "Senior",4] = "4"
#Major subsets

Econ <- subset(train, train$Major == "Economics")
Stat <- subset(train, train$Major == "Statistics")
Comp <- subset(train, train$Major == "CS")
Psych <- subset(train, train$Major == "Psychology")
Freshman <- subset(train, train$Seniority == "Freshman")
Sophomore <- subset(train, train$Seniority == "Sophomore")
Junior <- subset(train, train$Seniority == "Junior")
Senior <- subset(train, train$Seniority == "Senior")

#Econ graph
table1 <- table(train$Grade, train$Seniority)
mosaicplot(table1)
table2 <- table(train$Grade, train$Major)
mosaicplot(table2)
table3 <- table(Econ$Grade, Econ$Seniority)
mosaicplot(table3)
table4 <- table(Comp$Grade, Comp$Seniority)
mosaicplot(table4)
table5 <- table(Psych$Grade, Psych$Seniority)
mosaicplot(table5)
table6 <- table(Stat$Grade, Stat$Seniority)
mosaicplot(table6)
#boxplot(Econ$Score ~ Econ$Grade, data = train)
boxplot(Freshman$Score ~ Freshman$Grade, data = Freshman)
boxplot(Sophomore$Score ~ Sophomore$Grade, data = Sophomore)
boxplot(Junior$Score ~ Junior$Grade, data = Junior)
boxplot(Senior$Score ~ Senior$Grade, data = Senior)

summary(Freshman[Freshman$Grade == "A",])
summary(Freshman[Freshman$Grade == "B",])
summary(Freshman[Freshman$Grade == "C",])
summary(Freshman[Freshman$Grade == "D",])
summary(Freshman[Freshman$Grade == "F",])

summary(Sophomore[Sophomore$Grade == "A",])
summary(Sophomore[Sophomore$Grade == "B",])
summary(Sophomore[Sophomore$Grade == "C",])
summary(Sophomore[Sophomore$Grade == "D",])
summary(Sophomore[Sophomore$Grade == "F",])

summary(Junior[Junior$Grade == "A",])
summary(Junior[Junior$Grade == "B",])
summary(Junior[Junior$Grade == "C",])
summary(Junior[Junior$Grade == "D",])
summary(Junior[Junior$Grade == "F",])

summary(Senior[Senior$Grade == "A",])
summary(Senior[Senior$Grade == "B",])
summary(Senior[Senior$Grade == "C",])
summary(Senior[Senior$Grade == "D",])
summary(Senior[Senior$Grade == "F",])

table(Econ$Grade, Econ$Seniority)
table(Stat$Grade, Stat$Seniority)
table(Comp$Grade, Comp$Seniority)
table(Psych$Grade, Psych$Seniority)


correct <- subset(train, train$`Correct?` == "yes")
percent <- (700 - length(correct$`Correct?`))/length(train$`Correct?`)

tester[train$Seniority == "Freshman" & train$Major == "Psychology" & train$Score < 27 ,2] = "D"
tester[train$Seniority == "Freshman" & train$Major == "Psychology" & train$Score >= 27 & train$Score < 44 ,2] = "C"
tester[train$Seniority == "Freshman" & train$Major == "Psychology" & train$Score >= 44 & train$Score < 55 ,2] = "B"
tester[train$Seniority == "Freshman" & train$Major == "Psychology" & train$Score >= 55,2] = "A"

tester[train$Seniority == "Sophomore" & train$Major == "Psychology" & train$Score < 35 ,2] = "F"
tester[train$Seniority == "Sophomore" & train$Major == "Psychology" & train$Score >= 35 & train$Score < 40 ,2] = "D"
tester[train$Seniority == "Sophomore" & train$Major == "Psychology" & train$Score >= 40 & train$Score < 54 ,2] = "C"
tester[train$Seniority == "Sophomore" & train$Major == "Psychology" & train$Score >= 54 & train$Score < 70 ,2] = "B"
tester[train$Seniority == "Sophomore" & train$Major == "Psychology" & train$Score >= 70,2] = "A"

tester[train$Seniority == "Junior" & train$Major == "Psychology" & train$Score <= 31 ,2] = "F"
tester[train$Seniority == "Junior" & train$Major == "Psychology" & train$Score >= 31 & train$Score < 40 ,2] = "D"
tester[train$Seniority == "Junior" & train$Major == "Psychology" & train$Score >= 40 & train$Score < 56 ,2] = "C"
tester[train$Seniority == "Junior" & train$Major == "Psychology" & train$Score >= 56 & train$Score < 71 ,2] = "B"
tester[train$Seniority == "Junior" & train$Major == "Psychology" & train$Score >= 71,2] = "A"

tester[train$Seniority == "Senior" & train$Major == "Psychology" & train$Score < 33 ,2] = "F"
tester[train$Seniority == "Senior" & train$Major == "Psychology" & train$Score >= 33 & train$Score < 42 ,2] = "D"
tester[train$Seniority == "Senior" & train$Major == "Psychology" & train$Score >= 42 & train$Score < 57 ,2] = "C"
tester[train$Seniority == "Senior" & train$Major == "Psychology" & train$Score >= 57 & train$Score < 70 ,2] = "B"
tester[train$Seniority == "Senior" & train$Major == "Psychology" & train$Score >= 70,2] = "A"

tester[train$Seniority == "Freshman" & train$Major == "Statistics" & train$Score < 46 ,2] = "D"
tester[train$Seniority == "Freshman" & train$Major == "Statistics" & train$Score >= 46 & train$Score < 56 ,2] = "C"
tester[train$Seniority == "Freshman" & train$Major == "Statistics" & train$Score >= 56 & train$Score < 75 ,2] = "B"
tester[train$Seniority == "Freshman" & train$Major == "Statistics" & train$Score >= 75,2] = "A"

tester[train$Seniority == "Sophomore" & train$Major == "Statistics" & train$Score < 46 ,2] = "F"
tester[train$Seniority == "Sophomore" & train$Major == "Statistics" & train$Score >= 46 & train$Score < 56 ,2] = "D"
tester[train$Seniority == "Sophomore" & train$Major == "Statistics" & train$Score >= 56 & train$Score < 75 ,2] = "C"
tester[train$Seniority == "Sophomore" & train$Major == "Statistics" & train$Score >= 75 & train$Score < 84 ,2] = "B"
tester[train$Seniority == "Sophomore" & train$Major == "Statistics" & train$Score >= 84,2] = "A"

tester[train$Seniority == "Junior" & train$Major == "Statistics" & train$Score <= 40 ,2] = "F"
tester[train$Seniority == "Junior" & train$Major == "Statistics" & train$Score >= 40 & train$Score < 55 ,2] = "D"
tester[train$Seniority == "Junior" & train$Major == "Statistics" & train$Score >= 55 & train$Score < 75 ,2] = "C"
tester[train$Seniority == "Junior" & train$Major == "Statistics" & train$Score >= 75 & train$Score < 85 ,2] = "B"
tester[train$Seniority == "Junior" & train$Major == "Statistics" & train$Score >= 85,2] = "A"

tester[train$Seniority == "Senior" & train$Major == "Statistics" & train$Score < 44 ,2] = "F"
tester[train$Seniority == "Senior" & train$Major == "Statistics" & train$Score >= 44 & train$Score < 54 ,2] = "D"
tester[train$Seniority == "Senior" & train$Major == "Statistics" & train$Score >= 54 & train$Score < 76 ,2] = "C"
tester[train$Seniority == "Senior" & train$Major == "Statistics" & train$Score >= 76 & train$Score < 86 ,2] = "B"
tester[train$Seniority == "Senior" & train$Major == "Statistics" & train$Score >= 86,2] = "A"

tester[train$Seniority == "Freshman" & train$Major == "CS" & train$Score < 51 ,2] = "D"
tester[train$Seniority == "Freshman" & train$Major == "CS" & train$Score >= 51 & train$Score < 60 ,2] = "C"
tester[train$Seniority == "Freshman" & train$Major == "CS" & train$Score >= 60 & train$Score < 81 ,2] = "B"
tester[train$Seniority == "Freshman" & train$Major == "CS" & train$Score >= 81,2] = "A"

tester[train$Seniority == "Sophomore" & train$Major == "CS" & train$Score < 50 ,2] = "F"
tester[train$Seniority == "Sophomore" & train$Major == "CS" & train$Score >= 50 & train$Score < 53 ,2] = "D"
tester[train$Seniority == "Sophomore" & train$Major == "CS" & train$Score >= 53 & train$Score < 75 ,2] = "C"
tester[train$Seniority == "Sophomore" & train$Major == "CS" & train$Score >= 75 & train$Score < 90 ,2] = "B"
tester[train$Seniority == "Sophomore" & train$Major == "CS" & train$Score >= 90,2] = "A"

tester[train$Seniority == "Junior" & train$Major == "CS" & train$Score <= 49 ,2] = "F"
tester[train$Seniority == "Junior" & train$Major == "CS" & train$Score >= 49 & train$Score < 62 ,2] = "D"
tester[train$Seniority == "Junior" & train$Major == "CS" & train$Score >= 62 & train$Score < 77 ,2] = "C"
tester[train$Seniority == "Junior" & train$Major == "CS" & train$Score >= 77 & train$Score < 91 ,2] = "B"
tester[train$Seniority == "Junior" & train$Major == "CS" & train$Score >= 91,2] = "A"

tester[train$Seniority == "Senior" & train$Major == "CS" & train$Score < 52 ,2] = "F"
tester[train$Seniority == "Senior" & train$Major == "CS" & train$Score >= 52 & train$Score < 61 ,2] = "D"
tester[train$Seniority == "Senior" & train$Major == "CS" & train$Score >= 61 & train$Score < 81 ,2] = "C"
tester[train$Seniority == "Senior" & train$Major == "CS" & train$Score >= 81 & train$Score < 91 ,2] = "B"
tester[train$Seniority == "Senior" & train$Major == "CS" & train$Score >= 91,2] = "A"

tester[train$Seniority == "Freshman" & train$Major == "Economics" & train$Score < 40 ,2] = "D"
tester[train$Seniority == "Freshman" & train$Major == "Economics" & train$Score >= 40 & train$Score < 51 ,2] = "C"
tester[train$Seniority == "Freshman" & train$Major == "Economics" & train$Score >= 51 & train$Score < 63 ,2] = "B"
tester[train$Seniority == "Freshman" & train$Major == "Economics" & train$Score >= 63,2] = "A"

tester[train$Seniority == "Sophomore" & train$Major == "Economics" & train$Score < 39 ,2] = "F"
tester[train$Seniority == "Sophomore" & train$Major == "Economics" & train$Score >= 39 & train$Score < 51 ,2] = "D"
tester[train$Seniority == "Sophomore" & train$Major == "Economics" & train$Score >= 51 & train$Score < 68 ,2] = "C"
tester[train$Seniority == "Sophomore" & train$Major == "Economics" & train$Score >= 68 & train$Score < 79 ,2] = "B"
tester[train$Seniority == "Sophomore" & train$Major == "Economics" & train$Score >= 79,2] = "A"

tester[train$Seniority == "Junior" & train$Major == "Economics" & train$Score <= 42 ,2] = "F"
tester[train$Seniority == "Junior" & train$Major == "Economics" & train$Score >= 42 & train$Score < 47 ,2] = "D"
tester[train$Seniority == "Junior" & train$Major == "Economics" & train$Score >= 47 & train$Score < 63 ,2] = "C"
tester[train$Seniority == "Junior" & train$Major == "Economics" & train$Score >= 63 & train$Score < 80 ,2] = "B"
tester[train$Seniority == "Junior" & train$Major == "Economics" & train$Score >= 80,2] = "A"

tester[train$Seniority == "Senior" & train$Major == "Economics" & train$Score < 45 ,2] = "F"
tester[train$Seniority == "Senior" & train$Major == "Economics" & train$Score >= 45 & train$Score < 50 ,2] = "D"
tester[train$Seniority == "Senior" & train$Major == "Economics" & train$Score >= 50 & train$Score < 66 ,2] = "C"
tester[train$Seniority == "Senior" & train$Major == "Economics" & train$Score >= 66 & train$Score < 81 ,2] = "B"
tester[train$Seniority == "Senior" & train$Major == "Economics" & train$Score >= 81,2] = "A"


train[train$Grade == train$Testing, 6] = "yes"
train[train$Grade != train$Testing, 6] = "no"

EconFreshman <- subset(train, train$Major == "Economics" & train$Seniority == "Freshman")
EconSophomore <- subset(train, train$Major == "Economics" & train$Seniority == "Sophomore")
EconJunior <- subset(train, train$Major == "Economics" & train$Seniority == "Junior")
EconSenior <- subset(train, train$Major == "Economics" & train$Seniority == "Senior")

StatFreshman <- subset(train, train$Major == "Statistics" & train$Seniority == "Freshman")
StatSophomore <- subset(train, train$Major == "Statistics" & train$Seniority == "Sophomore")
StatJunior <- subset(train, train$Major == "Statistics" & train$Seniority == "Junior")
StatSenior <- subset(train, train$Major == "Statistics" & train$Seniority == "Senior")

PsychFreshman <- subset(train, train$Major == "Psychology" & train$Seniority == "Freshman")
PsychSophomore <- subset(train, train$Major == "Psychology" & train$Seniority == "Sophomore")
PsychJunior <- subset(train, train$Major == "Psychology" & train$Seniority == "Junior")
PsychSenior <- subset(train, train$Major == "Psychology" & train$Seniority == "Senior")

CompFreshman <- subset(train, train$Major == "CS" & train$Seniority == "Freshman")
CompSophomore <- subset(train, train$Major == "CS" & train$Seniority == "Sophomore")
CompJunior <- subset(train, train$Major == "CS" & train$Seniority == "Junior")
CompSenior <- subset(train, train$Major == "CS" & train$Seniority == "Senior")

nrow(train[train$`Correct?` == "no",])/nrow(train)

write.csv(tester,"C:/Users/Dylan/Downloads/tester.csv",row.names = F)

