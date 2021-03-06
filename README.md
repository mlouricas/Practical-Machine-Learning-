# Practical-Machine-Learning-
FInal Project

---
title: "Practical Machine Learning - Final Project"
author: "MJL"
date: "February 10, 2018"
output: html_document
---
--------------------------------------------


# Executive Summary
The following information was created as a Final project in the Practical Machine Learning course in the Data Science specialization from the Johns Hopkins University through the CourseRA program.
I created this prediction model write up in an attempt to answer 20 final quiz questions regarding the performance of my model selection. I have used three different prediction modeling methods, Random Forest, Decision Tree, and Generalized Boosting. Before actually creating the models and choosing the most accurate model to use, the data was downloaded from the web and cleansed.

# Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: [link](http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

# Data
The training data for this project are available here:

[link](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)

The test data are available here:

[link](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)

The data for this project come from this source:[link](http://groupware.les.inf.puc-rio.br/har). 

# What should be submitted
The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

# Package Load, Data Load & Cleaning

### Load needed libraries & Set working directory
```{r PackageLoad, echo=TRUE}
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
setwd("C:/Users/Mike/Desktop/DataScienceCourseRA_/MachineLearningProject")

```

### Getting and loading the data
Download the testing and training data from the sites referenced above.  The training set is then partitioned into subsets, 75% as the training data and 25% as the testing set.  

```{r GetLoadData, echo=TRUE}
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
```

#### Both training and test data sets have 160 necessary columns.  Training has 75% (14,718) rows and Test data has 25% (4,904) rows

### Data Cleaning
The variables still are made up of a significant amount of NA's.  These need to be removed as well as the Near Zero Variance (NZV) variables and the first column which is the observation ID.

#### Remove variables with NZV
```{r CleanNZV, echo=TRUE}

### remove variables with NZV
NZV <- nearZeroVar(TrSet)
TrSet <- TrSet[, -NZV]
TeSet  <- TeSet[, -NZV]
dim(TrSet)
dim(TeSet)
```

After removing NZV's, 128 Variables remain.

#### Remove variables that are mostly NA
```{r CleanNA, echo=TRUE}
#### Remove variables that are mostly NA
MostNA    <- sapply(TrSet, function(x) mean(is.na(x))) > 0.75
TrSet <- TrSet[, MostNA==FALSE]
TeSet <- TeSet[, MostNA==FALSE]
dim(TrSet)
dim(TeSet)
```

After removing fields consisting of over 75% NA's, 59 Variables remain.

#### Remove ID variables (ID and Name)
```{r CleanIDs, echo=TRUE}
### remove ID variables 
TrSet <- TrSet[, -(1:2)]
TeSet <- TeSet[, -(1:2)]
dim(TrSet)
dim(TeSet)
```

Finalizing the cleaning process of removing two fields that add no value, 57 total fields remain

### Transform the myTesting and testing data sets
```{r Trans, echo=TRUE}
clean1 <- colnames(TrSet)
# remove the classe column
clean2 <- colnames(TrSet[, -57])  
# allow only variables in TeSet that are also in TrSet
TeSet <- TeSet[clean1]      
# allow only variables in testSet that are also in TrSet
testSet <- testSet[clean2]             

dim(TeSet)
dim(testSet)

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
```

# Prediction Modeling (Algorithms)

3 models (Random Forest, Decision Tree, and Generalized Boosting) are used to determine the most accurate algorithm (using the Train Dataset) to apply to the Test data.  

### Random Forest Plotting and Modeling

```{r RForest, echo=TRUE}
set.seed(2468)

##Random Forest Plot
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
```


### Decision Tree Plotting and Modeling

```{r DTree, echo=TRUE}
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

```


### Generalized Boosting Plotting and Modeling

```{r GBoost, echo=TRUE}
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

```

# Model Selection and Test Cases
Generalized Boosting and Random Forest prediction methologies (0.9969 & 0.9998 respectively) were both significantly better than the Decision tree method (0.8697).  I have selected the most accurate method (Random Forest) to use on the test data set regarding the Test Cases. The expected out-of-sample error is 0.02% (100-99.98%).


#### Predict on Test data
```{r RFTest, echo=TRUE}
###Test data prediction
TestPred <- predict(RFFit, newdata=testSet)

```

### Create function to write predictions to files
```{r Test, echo=TRUE}
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
TestPred
