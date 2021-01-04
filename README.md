---
title: "Practical Machine Learning Course Project"
author: "Jose Augusto Barros de Oliveira"
date: "02/01/2021"
output:
  rmdformats::readthedown:
    highlight: tango
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library (skimr)
library(caret)
library(lubridate)
library(corrplot)
library(tidyverse)
```

## Summary  

The goal of this project is predict whether weight-lift exercises were performed as recommended. It uses the experimental design and data from this project on [Human Activity Recognition] (http://groupware.les.inf.puc-rio.br/har).
The dataset contains 160 features and 19622 cases. After some Exploratory Data Analysis (EDA), we decide to go through modeling with 60 features, besides the target. We performed Principal Component Analysis (PCA) in order to reduce the dimensionality, ending with 20 components that explained 90% of the variance.
The data was splited in two sets, the training one with 70% of cases.
We choose a Xg Boost model and used cross validation (CV) with K = 10 to tuning parameters.
The overall model accuracy on test set was xx% and the Confusion Matrix showed better results than the original paper.


## Data ingestion, data spliting and first analysis

```{r dataingestion}
# Train
theURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
if(file_test("-f", "pml-training.csv") == FALSE) 
  {
  download.file(theURL,"pml-training.csv",mode = "wb")
}
traindata <- read.csv("pml-training.csv", na.strings = c("","NA","NULL"))
# Spliting
set.seed(1969)
indice <- createDataPartition(y= traindata$classe, p = .70, list=FALSE)
training <- traindata[indice,]
testing <- traindata[-indice,]
# Data assessment
dataquality <- skim_without_charts(training)
head(dataquality,10)
```
It seems that we have two classes of features, regarding their origin. The majority relate to the measures of the exercises and the others are metadata from the experiments (e.g. names e timestamp).   

### Metadata features  

We found a perfect correlation between *raw_timestamp_part_1* and *cvtd_timestamp*, meaning that they express the same thing.

```{r}
g1 <- ggplot(data=training, aes(x=as.numeric(raw_timestamp_part_1), y = dmy_hm(cvtd_timestamp))) +
  geom_point(shape =1, position=position_jitter(width=2,height=2)) +
  labs(x = "raw_timestamp_part_1", y = "cvtd_timestamp") +
  ggtitle ("Cases by raw_timestamp_part_1 and cvtd_timestamp")
g1
```

We found an almost deterministic relationship between *num_window* and *classe*, suggesting that the experimental design carried out series of repeated measures by classe and subject, as showed in the next plot.

```{r}
temp <- training %>%
  count(classe, num_window,user_name)
g2 <- ggplot(temp, aes(fill=classe, y=n, x=num_window)) + 
    geom_bar(position="fill", stat="identity") + facet_grid (user_name ~ classe) +
  theme(axis.text.x= element_text(size = 8, angle = 90)) + ggtitle ("Proportion of cases by num_window, user_name and classe")
rm(temp)
g2
```

Thus, *num_window* by itself is enough to predict classe with 100% accuracy.

```{r}
chisq.test(training$num_window, training$classe)
```



### Measurement features

Although the instructions for this project grants the use of any feature available on the training dataset, we decide to stick with the principles of the original paper and kept only the measurement variables, as the original main goal was to use the measures of movement as predictors.

There are `r nrow(subset(dataquality, complete_rate < 0.05)) `  features with complete rate equals to 0.02. This missingness is probably Missing at Random (MAR), since there is a pattern, being the amount of missing values the same across the features and cases. We choose to drop those features because we couldn't figure out a better approach.

```{r}
temp <- subset(dataquality, complete_rate > 0.05)
training <- training[, temp$skim_variable]
rm(temp)
training <- training[,-c(1:3,5:8)] # Drop metadata variables
skim_without_charts(training)
```

## Pre processing

Some variables are high correlated:

```{r}
corrplot(cor(training[,-1]),
         method = "square",
         type = "upper", diag = FALSE,
         tl.cex = 0.6)
```

The use of Principal Component Analysis (PCA) to reduce dimensionality seems to be a good approach in this case. We kept 90% of the original variance, ending with 20 components. It is notewothy that centering and scaling the variables are carried out along with the PCA pre process.

```{r}
preProc <- preProcess(training[, -1], method = "pca", thresh = .9)
preProc
training <- predict(preProc, training)
```


## Modeling

A XGBoost model was trained by using the Caret package. Cross Validation with k = 10 was used to tuning the model's parameters.

```{r, cache= TRUE}
fitControl <- trainControl (method = "repeatedcv",
                            number = 10,
                            repeats = 10)
if(file_test("-f", "ml_xgboost.RDS") == FALSE) 
  {
  ml_xgboost <- train(classe ~ .,
                    method = "xgbTree",
                    trControl = fitControl,
                    data = training)
  saveRDS(ml_xgboost, "ml_xgboost.RDS")
}
ml_xgboost <- readRDS("ml_xgboost.RDS")
ml_xgboost
```
