## Análise preliminar

library (skimr)
library(caret)
library(lubridate)
library(corrplot)

# Load training data

theURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

if(file_test("-f", "pml-training.csv") == FALSE) 
  {
  download.file(theURL,"pml-training.csv",mode = "wb")
}

traindata <- read.csv("pml-training.csv", na.strings = c("","NA","NULL"))

# Data recognition & primary adjusts
dataquality <- skim_without_charts(traindata)
dataquality <- subset(dataquality, complete_rate > 0.05)
traindata1 <- traindata[, dataquality$skim_variable]

## Eliminate 100 features completely or almost empty of data

traindata1$cvtd_timestamp <- dmy_hm(traindata1$cvtd_timestamp)

plot(raw_timestamp_part_1 ~ cvtd_timestamp, data=traindata1)

# We can also eliminate time stamp part 1 since its the same as cvtd time stamp
# Also X since its only a index

traindata1 <- traindata1[,-c(5,6)]

# Features 7 to 58 refers to measures on body parts and movements; some of then are higly correlated

corrplot(cor(traindata1[,7:58]),
         method = "square",
         type = "upper", diag = FALSE,
         cex.var = 0.2)


## Create data partition

set.seed(1969)
indice <- createDataPartition(y= traindata1$classe, p = .70, list=FALSE)
training <- traindata1[indice,]
testing <- traindata1[-indice,]

preProc <- preProcess(training[, -c(1:9)], method = "pca", thresh = .9)

training <- predict(preProc, training)

ml_xgboost <- train(classe ~ .,
                    method = "xgbTree",
                    data = training)

train_pred <- predict(ml_xgboost, training[,-4])
confusionMatrix(train_pred, as.factor(training$classe))

varImp(ml_xgboost)

## teste

testing <- predict(preProc, testing)

test_pred <- predict(ml_xgboost, testing[,-4])
confusionMatrix(test_pred, as.factor(testing$classe))

# Load test data

theURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(file_test("-f", "pml-testing.csv") == FALSE) 
{
  download.file(theURL,"pml-testing.csv",mode = "wb")
}

testdata <- read.csv("pml-testing.csv", na.strings = c("","NA","NULL"))

names.use <- names(testdata)[(names(testdata) %in% dataquality$skim_variable)]

testdata1 <- testdata[ , names.use]

testdata1$cvtd_timestamp <- dmy_hm(testdata1$cvtd_timestamp)

testdata1 <- testdata1[,-c(1,3)]

testdata1 <- predict(preProc, testdata)