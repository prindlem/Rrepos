library(randomForest)
library(caret) 

#Set seed for reproducibility
set.seed(916)

#Upload the data from Kaggle
train <- read.csv("~/train.csv", stringsAsFactors = F)
test <- read.csv("~/test.csv", stringsAsFactors = F)

#Created an extra column so train and test have an equal amount of columns
test$Survived <- NA

#Combined train and test by Row
titanicFull <- rbind(train, test)

#Replaced all the missing values with S
titanicFull$Embarked[titanicFull$Embarked==""]="S"

apply(titanicFull,2, function(x) length(unique(x)))

#Combined the columns to come up with the size of the family
titanicFull$FamilySize <-titanicFull$SibSp + titanicFull$Parch + 1

#Replaced all numeric values with character values, so it can be converted into a factor 
titanicFull$FamilySized[titanicFull$FamilySize == 1]   <- 'Single'
titanicFull$FamilySized[titanicFull$FamilySize < 4 & titanicFull$FamilySize >= 2]   <- 'Small'
titanicFull$FamilySized[titanicFull$FamilySize >= 4]   <- 'Big'
titanicFull$FamilySized=as.factor(titanicFull$FamilySized)


ticket.unique <- rep(0, nrow(titanicFull))
tickets <- unique(titanicFull$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(titanicFull$Ticket == current.ticket)
  
  
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}

titanicFull$ticket.unique <- ticket.unique


#Replaced all numeric values with character values, so it can be converted into a factor
titanicFull$ticket.size[titanicFull$ticket.unique == 1]   <- 'Single'
titanicFull$ticket.size[titanicFull$ticket.unique < 5 & titanicFull$ticket.unique>= 2]   <- 'Small'
titanicFull$ticket.size[titanicFull$ticket.unique >= 5]   <- 'Big'

#Create Titles for each name in the table
names <- titanicFull$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)

titanicFull$title <- title

#renaming all the different woman prefixes to  avoid overfitting
titanicFull$title[titanicFull$title == 'Mlle']           <- 'Miss' 
titanicFull$title[titanicFull$title == 'Ms']             <- 'Miss'
titanicFull$title[titanicFull$title == 'Lady']           <- 'Miss'
titanicFull$title[titanicFull$title == 'Dona']           <- 'Miss'
titanicFull$title[titanicFull$title == 'Mme']            <- 'Mrs' 

#renaming all ship employees titles prefixes to  avoid overfitting
titanicFull$title[titanicFull$title == 'Capt']           <- 'Officer' 
titanicFull$title[titanicFull$title == 'Col']            <- 'Officer' 
titanicFull$title[titanicFull$title == 'Major']          <- 'Officer'
titanicFull$title[titanicFull$title == 'Dr']             <- 'Officer'
titanicFull$title[titanicFull$title == 'Rev']            <- 'Officer'
titanicFull$title[titanicFull$title == 'Don']            <- 'Officer'
titanicFull$title[titanicFull$title == 'Sir']            <- 'Officer'
titanicFull$title[titanicFull$title == 'the Countess']   <- 'Officer'
titanicFull$title[titanicFull$title == 'Jonkheer']       <- 'Officer'


titanicFull$ticket.size <- as.factor(titanicFull$ticket.size)
titanicFull$title <- as.factor(titanicFull$title)

#Converted 4 columns in factors
cols=c("Survived","Pclass","Sex","Embarked")
for (i in cols){
  titanicFull[,i]=as.factor(titanicFull[,i])
}

randForestData <- titanicFull[1:1309, c("Pclass", "title","Sex","Embarked","FamilySized","ticket.size")]
response <- as.factor(train$Survived)
randForestData$Survived=as.factor(titanicFull$Survived)

ind <- createDataPartition(randForestData$Survived,times=1,p=0.67942,list=FALSE)
train_val <- randForestData[-892:-1309,]
test_val <-  randForestData[892:1309,]

model <- randomForest(x = train_val[,-7],y=train_val[,7], importance = TRUE, ntree = 1000)
model

train_val1=train_val[,-4:-5]
test_val1=test_val[,-4:-5]

rf.2 <- randomForest(x = train_val1[,-5],y=train_val1[,5], importance = TRUE, ntree = 1000)


varImpPlot(model)

cv10_1 <- createMultiFolds(train_val1[,5], k = 10, times = 10)

# Set up caret's trainControl object per above.
ctrl_1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv10_1)

rf.5<- train(x = train_val1[,-5], y = train_val1[,5], method = "rf", tuneLength = 3, ntree = 1000, trControl =ctrl_1)

submission <- data.frame(PassengerId = test$PassengerId)

submission$Survived <- predict(rf.5,newdata = test_val1)

write.csv(submission, file = "Randomforest_R_submission.csv", row.names=FALSE)


