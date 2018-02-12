

#### Load needed libraries & Set working directory
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(ggplot2)

setwd("C:/Users/Mike/Desktop/DataScienceCourseRA_/MachineLearningProject")

### URL location variables
Train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
Test  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

### download the data
trainSet <- read.csv(url(Train), na.strings=c("NA","#DIV/0!",""))
testSet  <- read.csv(url(Test), na.strings=c("NA","#DIV/0!",""))

# Partition Training Set to 75%
set.seed(2468)
inTrain  <- createDataPartition(trainSet$classe, p=0.75, list=FALSE)
TrSet <- trainSet[inTrain, ]
TeSet  <- trainSet[-inTrain, ]
dim(TrSet)
dim(TeSet)

### remove variables with NZV
NZV <- nearZeroVar(TrSet)
TrSet <- TrSet[, -NZV]
TeSet  <- TeSet[, -NZV]
dim(TrSet)
dim(TeSet)


#### Remove variables that are mostly NA
MostNA    <- sapply(TrSet, function(x) mean(is.na(x))) > 0.75
TrSet <- TrSet[, MostNA==FALSE]
TeSet  <- TeSet[, MostNA==FALSE]
dim(TrSet)
dim(TeSet)

### remove ID variables 
TrSet <- TrSet[, -(1:2)]
TeSet  <- TeSet[, -(1:2)]
dim(TrSet)
dim(TeSet)

### Transform the myTesting and testing data sets

clean1 <- colnames(TrSet)
clean2 <- colnames(TrSet[, -57])  # remove the classe column
TeSet <- TeSet[clean1]         # allow only variables in myTesting that are also in myTraining
testSet <- testSet[clean2]             # allow only variables in testing that are also in myTraining

dim(TeSet)
dim(teStSet)

### Coerce the data into the same type

for (i in 1:length(testSet) ) {
        for(j in 1:length(TrSet)) {
                if( length( grep(names(TrSet[i]), names(testSet)[j]) ) == 1)  {
                        class(testSet[j]) <- class(TrSet[i])
                }      
        }      
}

# To get the same class between testing and myTraining
testSet <- rbind(TrSet[2, -57] , testSet)
testSet <- testSet[-1,]



##Random Forest Plot
set.seed(2468)

RFFit <- randomForest(classe ~ ., data=TrSet)
plot(RFFit)

##Random Forest Test (Confusion Matrix)
RFitPred <- predict(RFFit, TeSet, type = "class")
RFConMtrx <- confusionMatrix(RFitPred, TeSet$classe)
RFConMtrx

## Plot Random Forest Confusion 
plot(RFConMtrx$table, col = RFConMtrx$byClass, 
     main = paste("RF Confusion Matrix - Accuracy =", 
                  round(RFConMtrx$overall['Accuracy'], 4)))


set.seed(2468)
##Decision tree model
DTFit <- rpart(classe ~ ., data=TrSet, method="class")
## Decision Tree plot
fancyRpartPlot(DTFit)

##Decision Tree (Confusion Matrix)
DTFitPred <- predict(DTFit, TeSet, type = "class")
DTConMtrx <- confusionMatrix(DTFitPred, TeSet$classe)
DTConMtrx

###Decision Tree Confusion
plot(DTConMtrx$table, col = DTConMtrx$byClass, 
     main = paste("Decision Tree Confusion Matrix - Accuracy =", ... = 
                          round(DTConMtrx$overall['Accuracy'], 4)))


set.seed(2468)
##GBoost model
GBMC <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
GBMFit  <- train(classe ~ ., data=TrSet, method = "gbm",
                 trControl = GBMC, verbose = FALSE)
GBMFit$finalModel

##GBoost (Confusion Matrix)
GBMPred <- predict(GBMFit, newdata=TeSet)
GBMConMtrx <- confusionMatrix(GBMPred, TeSet$classe)
GBMConMtrx

###GBoost Confusion
plot(GBMConMtrx$table, col = GBMConMtrx$byClass, 
     main = paste("GBM Confusion Matrix - Accuracy =", ... = 
                          round(GBMConMtrx$overall['Accuracy'], 4)))



###Test data prediction

TestPred <- predict(RFFit, newdata=testSet)



### Create function to write predictions to files

pml_write_files <- function(x) {
        n <- length(x)
        for(i in 1:n) {
                filename <- paste0("problem_id_", i, ".txt")
                write.table(x[i], file=filename, quote=F, row.names=F, col.names=F)
        }
}


# create prediction files to submit
pml_write_files(TestPred)