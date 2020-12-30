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




