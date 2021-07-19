if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(vcd)) install.packages("vcd", repos = "http://cran.us.r-project.org")
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(party)) install.packages("party", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(matrixStats)
library(lubridate)
library(knitr)
library(vcd)
library(mlbench)
library(party)
library(ggplot2)
library(nnet)
library(randomForest)
library(MLmetrics)

## 1. Introduction

# data is downloaded from the website and loaded to a data frame
dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data", dl)

# rename, to make sure that data import only starts when download is finished
file.rename(from = dl, to = "transfusion.csv") 
transfusion <- read.csv("transfusion.csv", header=TRUE)

# Check after data import, if the data is structured:
str(transfusion)

# Final test set will be 20% of the dataset
set.seed(1) 
df <- transfusion
partition <- createDataPartition(df[,5], times = 1, p = 0.80, list = FALSE)
train <- transfusion[partition,]
test <- transfusion[-partition,]

#rm(dl, df, partition)


## 2. Methods and Analysis

# change the originals columns names to make it easier for typing
colnames(train) <- c("recency", "total_frequency", "total_monetary","time","donation")
colnames(test) <- c("recency", "total_frequency", "total_monetary","time","donation")


#### 2.2.1 Some general data analysis

nrow(train) # count rows in train data set

summary(train) # Shows first summary of train data

str(train) # Shows all data types of train data

# change data types for the different columns to make calculations and analysis easier
train <- as.data.frame(train) %>% mutate(total_frequency = as.numeric(total_frequency),
                                            total_monetary = as.numeric(total_monetary),
                                            donation = as.factor(donation))
test <- as.data.frame(test) %>% mutate(total_frequency = as.numeric(total_frequency),
                                            total_monetary = as.numeric(total_monetary),
                                            donation = as.factor(donation))
str(train)
str(test)

#### 2.2.2 Correlations and relationships

panel.hist <- function(x, ...) # scatterplot matrix to compare columns/features
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(train[1:5], main="scattered matrix for train dataset", panel = panel.smooth,
      cex = 1, pch = 22, bg = "light blue",
      diag.panel = panel.hist, cex.labels = 1.5, font.labels = 2)

cor(train[1:4]) # Show correlation matrix for all numeric features

train[ , c("total_monetary")] <- list(NULL) # delete column total_monetary in train set
test[ , c("total_monetary")] <- list(NULL) # delete column total_monetary in test set

head(train) # show first 6 rows of train table

#### 2.2.3 Analysing recency

train %>%
  ggplot(aes(x = recency)) +
  geom_histogram(
    aes(y = stat(density)),
    breaks = seq(min(train$recency) - 10, max(train$recency) + 10),
    colour = "black",
    fill = "deepskyblue") +
  geom_function(
    fun = function(x) dnorm(x, mean(train$recency), sd(train$recency)),
    xlim = c(min(train$recency) - 10, max(train$recency) + 10),
    size = 1) +
  labs(
    title = "recency",
    x = "recency = number of months since last donation",
    y = "probability density"
  )

#### 2.2.4 Analysing total_frequency

train %>%
  ggplot(aes(x = total_frequency)) +
  geom_histogram(
    aes(y = stat(density)),
    breaks = seq(min(train$total_frequency) - 10, max(train$total_frequency) + 10),
    colour = "black",
    fill = "red") +
  geom_function(
    fun = function(x) dnorm(x, mean(train$total_frequency), sd(train$total_frequency)),
    xlim = c(min(train$total_frequency) - 10, max(train$total_frequency) + 10),
    size = 1) +
  labs(
    title = "frequency",
    x = "frequency = total number of donations done",
    y = "probability density"
  )

#### 2.2.5 Analysing time

Now I will check the time (number of months since first donation of a person):
```{r}
train %>%
  ggplot(aes(x = time)) +
  geom_histogram(
    aes(y = stat(density)),
    breaks = seq(min(train$time) - 10, max(train$time) + 10),
    colour = "black",
    fill = "yellow") +
  geom_function(
    fun = function(x) dnorm(x, mean(train$time), sd(train$time)),
    xlim = c(min(train$time) - 10, max(train$time) + 10),
    size = 1) +
  labs(
    title = "time",
    x = "time = number of months since first dondation",
    y = "probability density"
  )

max(train$time)


#### 2.2.6 Analysing total_frequency vs. time

gplot(train) +
  geom_point(aes(x = total_frequency, y = time),
             size = 1) +
  labs(title = "total_frequency vs. time",
       x = "total number of donations",
       y = "number of months since first donation") +
  theme_linedraw() +
  theme(axis.line.x = element_line(size = 2))


#### 2.2.7 Analysing recency vs. total_frequency
# compare now the number of months since last donation with the total number of donations:
ggplot(train) +
  geom_point(aes(x = recency, y = total_frequency),
             size = 2) +
  labs(title = "recency vs. total_frequency",
       x = "number of months since last donation",
       y = "total number of donations") +
  theme_linedraw() +
  theme(axis.line.x = element_line(size = 2))


#### 2.2.8 Analysing recency vs. time
#compare the number of months since last donation versus the number of months since first donation:
ggplot(train) +
  geom_point(aes(x = recency, y = time),
             size = 2) +
  geom_line(aes(x = time, y = time),
            color = "green",
            size = 1)+
  labs(title = "recency vs. time",
       x = "number of months since last donation",
       y = "number of months since first donation") +
  theme_linedraw() +
  theme(axis.line.x = element_line(size = 2))


#### 2.2.9 Analysing donation
# compare the donating results in the train data set:
summary(train$donation)


### 2.3 Data Cleaning

#### 2.3.1 Check if data in train and test set are from the same population

# definition of a normalization function
norm <- function(x){
            return((x-min(x)) / (max(x) - min(x)))
}

#apply norm function to columns 1-3 in the train and test data set
train_norm <- as.data.frame(lapply(train[1:3], norm))
test_norm <- as.data.frame(lapply(test[1:3], norm))

# plot the overview based on norm function
boxplot(train_norm, 
        main="train", 
        notch=TRUE, 
        na.action=NULL, 
        drop=FALSE, 
        lex.order=FALSE)
boxplot(test_norm, 
        main="test", 
        notch=TRUE, 
        na.action=NULL, 
        drop=FALSE, 
        lex.order=FALSE)

rm(train_norm, test_norm) # remove again to clear workspace

#### 2.3.2 Outlier detection - check for extreme values
# Check outliers recency
boxplot.stats(train$recency)$out # check for outliers in recency columns

# Check outliers total_frequency
boxplot.stats(train$total_frequency)$out # check for outliers in total_frequency columns

# Check outliers time
boxplot.stats(train$time)$out # check for outliers in time columns


#### 2.3.3 Check for duplicates
nrow(train) # shows amount of all rows in train set
nrow(distinct(train)) # shows amount of all distinct rows in train set
#train <- distinct(train) # this is not used: take all distinct values and save them back in train


### 2.4 Insights based on the data exploration
# no R code in this section

### 2.5 Final data check
anyNA(train) # Check for NA's in train data set
anyNA(test) # Check for NA's in test data set


## 3 Modeling approach

### 3.1 Preparation for modeling

# I will change the factor value: "0 -> no", "1 -> yes" to make it more clear and to 
# avoid mistakes by confusion of data
# change levels of factors from 0 to "no" and 1 to "yes" for later usage in kNN
levels(train$donation) <- c("no", "yes") 
levels(test$donation) <- c("no", "yes")

# Caret library is used for data splitting. I choose a ratio of 75% / 25% to 
# have enough train data
set.seed(1)
df <- train
partition <- createDataPartition(df[,1], times = 1, p = 0.75, list = FALSE)
train <- df[partition,] # Create the training sample
validation = df[-partition,] # Create the test sample
rm(df, partition) # remove temporary variables to clear cache

# 10 fold crossvalidation with 4 repeats are used for my models
crossvalidation <- trainControl(method = "repeatedcv", 
                                number = 10,
                                repeats = 4,
                                classProbs = TRUE, 
                                summaryFunction = mnLogLoss)

#### 3.2 Modeling: Decision Trees

# Decision Tree plotting using tree
set.seed(1)
model.decisiontree = ctree(donation ~ ., data = train)
plot(model.decisiontree)

# Decision tree using caret package
set.seed(1)
model.tree = train(donation ~ ., # Create Decision Tree with LogLoss based on Caret
                  data = train, 
                  method = "rpart", 
                  metric = "logLoss",
                  maximize = FALSE,
                  trControl = crossvalidation)
score = predict(model.tree, newdata = validation)
conf_matrix = confusionMatrix(score,validation$donation)
ll = min(model.tree$results$logLoss)

# create result table to collect all accuracy values from the different models
modeling_results <- tibble(method = "Decision Tree",
                           logLoss = ll, 
                           Accuracy = conf_matrix$overall['Accuracy'])


#### 3.3 Modeling: Random Forest
set.seed(1)
model.rf <- train(donation ~ ., 
                   data = train,
                   method = "rf",
                   metric = "logLoss",
                   maximize = FALSE,
                   trControl = crossvalidation)
score <- predict(model.rf, newdata = validation)
ll = min(model.rf$results$logLoss)
conf_matrix <- confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results, # add to result overview
                          tibble(method = "Random Forest",
                              logLoss = ll, 
                              Accuracy = conf_matrix$overall['Accuracy']))

### 3.4 Modeling: Logistic Regression
set.seed(1)
model.LogReg <- train(donation~.,
                     data=train,
                     method="glm",
                     family=binomial,
                     metric = "logLoss",
                     maximize = FALSE,
                     trControl = crossvalidation) 
score <- predict(model.LogReg, newdata = validation)
ll = min(model.LogReg$results$logLoss)
conf_matrix <- confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results,  # add to result overview
                          tibble(method = "Logistic Regression",
                          logLoss = ll, 
                          Accuracy = conf_matrix$overall['Accuracy']))

#### 3.5 Modeling: kNN
set.seed(1)
model.knn <- train(donation ~ ., 
                   data = train,
                   method = "knn",
                   metric = "logLoss",
                   maximize = FALSE,
                   trControl = crossvalidation)
score <- predict(model.knn, newdata = validation)
ll = min(model.knn$results$logLoss)
conf_matrix <- confusionMatrix(score, validation$donation) # Check accuracy with validation set
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "kNN",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))


### 3.6 Modeling: Neuronal Network
set.seed(1)
model.nnet <- train(donation~.,
                    data=train, 
                    method="nnet", 
                    metric="logLoss",
                    maximize = FALSE,
                    preProc=c("center","scale"), # for nnet values should always be scaled
                    trControl = crossvalidation)
score <- predict(model.nnet, newdata = validation)
ll = min(model.nnet$results$logLoss)
conf_matrix <- confusionMatrix(score, validation$donation) # Check accuracy with validation set

modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "Neuronal Network",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

### 3.7 Overview of different model results
knitr::kable(modeling_results, caption = "modeling results")


### 3.8 Optimizing modeling results
#### 3.8.1 Optimizing features

# add a new column for donation rate into train data set
train <- mutate(train, don_rate = total_frequency / time)

# add a new column for faithfulness into train data set
train <- mutate(train, faith = recency / total_frequency)

# add new column for people, who probably won't come back again
train <- mutate(train, churn_rate = recency / time)

# move donation column to the last column
train <- train %>% select(-donation,donation)

# check correlation of the new columns
cor(train[1:6])

# add all three new columns above also into our train and validation data set
test <- mutate(test, don_rate = total_frequency / time,
               faith = recency / total_frequency,
               churn_rate = recency / time)
validation <- mutate(validation, don_rate = total_frequency / time,
                     faith = recency / total_frequency,
                     churn_rate = recency / time)

# re-run the models:

# Decision Tree
set.seed(1)
model.tree_opt_f = train(donation ~ ., # Create Decision Tree with LogLoss based on Caret
                   data = train, 
                   method = "rpart", 
                   metric = "logLoss",
                   maximize = FALSE,
                   trControl = crossvalidation)
score = predict(model.tree_opt_f, newdata = validation)
ll = min(model.tree_opt_f$results$logLoss)
conf_matrix = confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "Decision Tree Caret optimized by new features",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Random Forest
set.seed(1)
model.rf_opt_f <- train(donation ~ ., 
                  data = train,
                  method = "rf",
                  metric = "logLoss",
                  maximize = FALSE,
                  trControl = crossvalidation)
score <- predict(model.rf_opt_f, newdata = validation)
ll = min(model.rf_opt_f$results$logLoss)
conf_matrix <- confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "Random Forest optimized by new features",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Logistic Regression
set.seed(1)
model.LogReg_opt_f <- train(donation~.,
                      data=train,
                      method="glm",
                      family=binomial,
                      metric = "logLoss",
                      maximize = FALSE,
                      trControl = crossvalidation) 
score <- predict(model.LogReg_opt_f, newdata = validation)
ll = min(model.LogReg_opt_f$results$logLoss)
conf_matrix <- confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results,  # add to result overview
                              tibble(method = "Logistic Regression optimized by new features",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Modeling: kNN
set.seed(1)
model.knn_opt_f <- train(donation ~ ., 
                   data = train,
                   method = "knn",
                   metric = "logLoss",
                   maximize = FALSE,
                   trControl = crossvalidation)
score <- predict(model.knn_opt_f, newdata = validation)
ll = min(model.knn_opt_f$results$logLoss)
conf_matrix <- confusionMatrix(score, validation$donation) # Check accuracy with validation set
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "kNN optimized by new features",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Modeling: Neuronal Network
set.seed(1)
model.nnet_opt_f <- train(donation~.,
                    data=train, 
                    method="nnet", 
                    metric="logLoss",
                    maximize = FALSE,
                    preProc=c("center","scale"), # for nnet values should always be scaled
                    trControl = crossvalidation)
score <- predict(model.nnet_opt_f, newdata = validation)
ll = min(model.nnet_opt_f$results$logLoss)
conf_matrix <- confusionMatrix(score, validation$donation) # Check accuracy with validation set

modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "Neuronal Network optimized by new features",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

knitr::kable(modeling_results, caption = "modeling results")


#### 3.8.2 Optimizing with TuneGrid

# Decision Tree
set.seed(1)
tunegrid_dc <- expand.grid(cp=.2) # I will use 1 to 30 here to make it not too big
model.tree_opt_tg = train(donation ~ ., # Create Decision Tree with LogLoss based on Caret
                         data = train, 
                         method = "rpart", 
                         metric = "logLoss",
                         maximize = FALSE,
                         trControl = crossvalidation,
                         tuneGrid = tunegrid_dc)
score = predict(model.tree_opt_tg, newdata = validation)
ll = min(model.tree_opt_tg$results$logLoss)
conf_matrix = confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "Decision Tree Caret optimized by TuneGrid",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Random Forest
tunegrid_rf <- expand.grid(mtry=2) # 1 would be less and >5 more complex, I choose middle
set.seed(1)
model.rf_opt_tg <- train(donation ~ ., 
                        data = train,
                        method = "rf",
                        metric = "logLoss",
                        maximize = FALSE,
                        trControl = crossvalidation,
                        tuneGrid = tunegrid_rf)
score <- predict(model.rf_opt_tg, newdata = validation)
ll = min(model.rf_opt_tg$results$logLoss)
conf_matrix <- confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "Random Forest optimized by TuneGrid",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Logistic Regression
set.seed(1)
tunegrid_logreg <- expand.grid(parameter=2)
model.LogReg_opt_tg <- train(donation~.,
                            data=train,
                            method="glm",
                            family=binomial,
                            metric = "logLoss",
                            maximize = FALSE,
                            trControl = crossvalidation,
                            tuneGrid = tunegrid_logreg) 
score <- predict(model.LogReg_opt_tg, newdata = validation)
ll = min(model.LogReg_opt_tg$results$logLoss)
conf_matrix <- confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results,  # add to result overview
                              tibble(method = "Logistic Regression optimized by TuneGrid",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Modeling: kNN
set.seed(1)
tunegrid_knn <- expand.grid(k=c(1:30))
model.knn_opt_tg <- train(donation ~ ., 
                         data = train,
                         method = "knn",
                         metric = "logLoss",
                         maximize = FALSE,
                         trControl = crossvalidation,
                         tuneGrid = tunegrid_knn)
score <- predict(model.knn_opt_tg, newdata = validation)
ll = min(model.knn_opt_tg$results$logLoss)
conf_matrix <- confusionMatrix(score, validation$donation) # Check accuracy with validation set
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "kNN optimized by TuneGrid",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Modeling: Neuronal Network
set.seed(1)
tunegrid_nnet = expand.grid(size=c(3:5), decay=c(0.1,0.5))
model.nnet_opt_tg <- train(donation~.,
                          data=train, 
                          method="nnet", 
                          metric="logLoss",
                          maximize = FALSE,
                          preProc=c("center","scale"), # for nnet values should always be scaled
                          trControl = crossvalidation,
                          tuneGrid = tunegrid_nnet)
score <- predict(model.nnet_opt_tg, newdata = validation)
ll = min(model.nnet_opt_tg$results$logLoss)
conf_matrix <- confusionMatrix(score, validation$donation) # Check accuracy with validation set
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "Neuronal Network optimized by TuneGrid",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

knitr::kable(modeling_results, caption = "modeling results")

#### 3.8.4 Optimizing using scaling
summary(train)

# Decision Tree
set.seed(1)
tunegrid_dc <- expand.grid(cp=.2) # I will use 1 to 30 here to make it not too big
model.tree_opt_tgs = train(donation ~ ., # Create Decision Tree with LogLoss based on Caret
                          data = train, 
                          method = "rpart", 
                          metric = "logLoss",
                          maximize = FALSE,
                          trControl = crossvalidation,
                          tuneGrid = tunegrid_dc,
                          preProc = c("center","scale"))
score = predict(model.tree_opt_tgs, newdata = validation)
ll = min(model.tree_opt_tgs$results$logLoss)
conf_matrix = confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "Decision Tree Caret optimized by TuneGrid and scaling",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Random Forest
tunegrid_rf <- expand.grid(mtry=2) # 1 would be less and >5 more complex, I choose middle
set.seed(1)
model.rf_opt_tgs <- train(donation ~ ., 
                         data = train,
                         method = "rf",
                         metric = "logLoss",
                         maximize = FALSE,
                         trControl = crossvalidation,
                         tuneGrid = tunegrid_rf,
                         preProc = c("center","scale"))
score <- predict(model.rf_opt_tgs, newdata = validation)
ll = min(model.rf_opt_tgs$results$logLoss)
conf_matrix <- confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "Random Forest optimized by TuneGrid and scaling",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Logistic Regression
set.seed(1)
tunegrid_logreg <- expand.grid(parameter=2)
model.LogReg_opt_tgs <- train(donation~.,
                             data=train,
                             method="glm",
                             family=binomial,
                             metric = "logLoss",
                             maximize = FALSE,
                             trControl = crossvalidation,
                             tuneGrid = tunegrid_logreg,
                             preProc = c("center","scale")) 
score <- predict(model.LogReg_opt_tgs, newdata = validation)
ll = min(model.LogReg_opt_tgs$results$logLoss)
conf_matrix <- confusionMatrix(score,validation$donation)
modeling_results <- bind_rows(modeling_results,  # add to result overview
                              tibble(method = "Logistic Regression optimized by TuneGrid and scaling",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

# Modeling: kNN
set.seed(1)
tunegrid_knn <- expand.grid(k=c(1:30))
model.knn_opt_tgs <- train(donation ~ ., 
                          data = train,
                          method = "knn",
                          metric = "logLoss",
                          maximize = FALSE,
                          trControl = crossvalidation,
                          tuneGrid = tunegrid_knn,
                          preProc = c("center","scale"))
score <- predict(model.knn_opt_tgs, newdata = validation)
ll = min(model.knn_opt_tgs$results$logLoss)
conf_matrix <- confusionMatrix(score, validation$donation) # Check accuracy with validation set
modeling_results <- bind_rows(modeling_results, # add to result overview
                              tibble(method = "kNN optimized by TuneGrid and scaling",
                                     logLoss = ll, 
                                     Accuracy = conf_matrix$overall['Accuracy']))

## 4. Results

### 4.1 Presentation of modeling results

# Show results based on logLoss value (desc sorting)
modeling_results %>% arrange(modeling_results$logLoss)

# Show results based on Accuracy value (asc sorting)
modeling_results %>% arrange(desc(modeling_results$Accuracy))

# collect the results using resamples function into one plot
results <- resamples(list(Decision_Tree = model.tree,
                          Random_Forest = model.rf,
                          kNN = model.knn,
                          LogRegression = model.LogReg, 
                          Neuronal_Net = model.nnet,
                          Decision_Tree_feature_optimized = model.tree_opt_f,
                          Random_Forest_feature_opt = model.rf_opt_f,
                          kNN_feature_opt = model.knn_opt_f,
                          LogRegression_feature_opt = model.LogReg_opt_f, 
                          Neuronal_Net_feature_opt = model.nnet_opt_f,
                          Decision_Tree_tunegrid = model.tree_opt_tg,
                          Random_Forest_tunegrid = model.rf_opt_tg,
                          kNN_tunegrid = model.knn_opt_tg,
                          LogRegression_tunegrid = model.LogReg_opt_tg, 
                          Neuronal_Net_tunegrid = model.nnet_opt_tg,
                          Decision_Tree_feature_tunegrid_scaling = model.tree_opt_tgs,
                          Random_Forest_feature_tunegrid_scaling = model.rf_opt_tgs,
                          kNN_feature_tunegrid_scaling = model.knn_opt_tgs,
                          LogRegression_tunegrid_scaling = model.LogReg_opt_tgs))
# show summary of the results 
summary(results)

# create a dot plot and a bwplot to visually see the results
bwplot(results)

# show Confusion Matrix for our Neuronal Network with feature and TuneGrid optimization
confusionMatrix(predict(model.nnet_opt_tg, newdata = validation), validation$donation) 

### 4.3 Use final model on final test data set
score <- predict(model.nnet_opt_tg, newdata = test)
confusionMatrix(score, test$donation) # Check accuracy with validation set

# clear tables to save space
rm(list=ls()) # clear everything from environment


