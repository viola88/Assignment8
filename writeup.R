rm(list = ls()) # to clear the workspace
dev.off(dev.list()["RStudioGD"]) # close all graphical devices
setwd("~/Desktop/Practical Machine Learning")

## How well did you do your exercise?

# Finding out, how many steps you were running etc. is easy. The harder question is to find out how well did you perform an 
# exercise. This assignment is about finding out exactly that. The underlying data is taken from  http://groupware.les.inf.puc-rio
# .br/har and contains data of almost 20,000 workouts for training as well as data of 20 workouts for testing. The task is to 
# grading how well the person performed: A,B,C,D or E.

## Read in data
training <- read.table("pml-training.csv", header = TRUE, sep = ",", dec = ".", fill = TRUE)
evaluating <- read.table("pml-testing.csv", header = TRUE, sep = ",", dec = ".", fill = TRUE)
summary(training)
summary(evaluating)

## Data Preprocessing

# There appear to be many NA's in quite a lot of data points in training data (1287472, see below). We will delete those data points as they are not 
# representative enough. Moreover, there are some columns, which only consists of NA's in the evaluating data set. We also delete 
# these data points as well as the user id. 
sum(is.na(training))
for (i in 159:1){
  if (sum(is.na(training[[i]]))>10000){    
    training <- training[,-i]
    evaluating <- evaluating[,-i]
  }
}
for (j in 93:1){
  if (sum(is.na(evaluating[[j]]))>19){    
    training <- training[,-j]
    evaluating <- evaluating[,-j]
  }
}
training <- training[,-1]
evaluating <- evaluating[,-1]
evaluating <- evaluating[,-ncol(evaluating)]
evaluating <- evaluating[,-5]
evaluating <- evaluating[,-4]
training <- training[,-5]
training <- training[,-4]

## Model building
library(randomForest)
library(caret)
library(e1071)
# For the model we need a classification algorithm. From the lectures I learned that the random forest seems to be most promising.

# There are 20 individuals for testing. Before, there are 19622 individuals in the training group. We will build a model and test 
# it on the training group. For this, we will use cross validation, that is we will divide the training group into a test group 
# and a training group and this about ten times (the order shouldn't matter). Each time the training group will consist of 80 % 
# of the training data and the rest 20 % are used for testing.

k <- 5
crossvalidation <- 1:k
predicteddata <- data.frame()
referencedata <- data.frame()
training$id <- sample(1:k,nrow(training),replace=TRUE)

for (i in 1:k){
  trainingset <- subset(training, id %in% crossvalidation[-i])
  testingset <- subset(training,id %in% c(i))
  trainingset <- trainingset[,-ncol(trainingset)]
  testingset <- testingset[,-ncol(testingset)]
  model <- randomForest(trainingset$classe ~., data=trainingset,ntree=100)
  temp <- as.data.frame(predict(model,testingset[,-(ncol(testingset))]))
  predicteddata <- rbind(predicteddata,temp)
  referencedata <- rbind(referencedata,as.data.frame(testingset[,ncol(testingset)]))
}

# As a result we have the following confusion Matrix
confusionMatrix(predicteddata[,1],referencedata[,1])
result <- as.data.frame(predict(model,evaluating))

answers = rep("A", 20)

  
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

then create a folder where you want the files to be written. Set that to be your working directory and run:
  
  
  pml_write_files(answers)

