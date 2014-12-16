pml-assignement
===============

This is practical machine learning repo
##Load all libraries used, and setting seed for reproducibility. 

    library(ElemStatLearn)
    library(caret)
    library(rpart)
    library(randomForest)
    set.seed(357)

## load the training set data from local directory
  training <- read.table("pml-training.csv", header=TRUE, sep=",", na.strings=c("NA",""))
  training <- training[,-1] # Remove the first column that represents a ID Row

##create a training and validating set
  inTrain = createDataPartition(training$classe, p=0.60, list=FALSE)
  training = training[inTrain,]
  validating = training[-inTrain,]

##check column with 60% data in it if there is more than 60% keep otherwise remove
  sum((colSums(!is.na(training[,-ncol(training)])) < 0.6*nrow(training)))
  Keep <- c((colSums(!is.na(training[,-ncol(training)])) >= 0.6*nrow(training)))
  training   <-  training[,Keep]
  validating <- validating[,Keep]

##develop random forest model
  model <- randomForest(classe~.,data=training)
  print(model)
  importance(model)

##Evaluate  model results using confusion Matrix.
  confusionMatrix(predict(model,newdata=validating[,-ncol(validating)]),validating$classe)

## get the test data in to R console from local directry
testing <- read.csv("pml-testing.csv", header=TRUE, sep=",", na.strings=c("NA",""))

testing <- testing[,-1] # Remove the first column that represents a ID Row
testing <- testing[ , Keep] # Keep the same columns of testing dataset
testing <- testing[,-ncol(testing)] # Remove the problem ID
testing<-pml_CSV

# Coerce testing set to same class and strucuture of training set 
  testing <- rbind(training[100, -59] , testing) 
# Apply the ID Row to row.names and 100 for dummy row from testing set 
  row.names(testing) <- c(100, 1:20)
## predict new testing data set
  predictions <- predict(model,newdata=testing[-1,])
  print(predictions)

##create answer text file
  answers = rep("A", 20)
  pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
       }
    }

    pml_write_files(answers)
