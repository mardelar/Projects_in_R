#####  Check packages that are downloaded. Load Libraries  #####

library(ggplot2) 
library(readr) 
library(caTools) 
library(rmarkdown)
library(e1071) 

#####  Bring in cleaned data set  #####

loans <- read.csv(file.choose())

#####  Initial look at data  #####

summary(loans)

str(loans)

#####  Data Transformation  #####

loans$credit.policy <- as.factor(loans$credit.policy)
loans$inq.last.6mths <-as.factor(loans$inq.last.6mths)
loans$delinq.2yrs <- as.factor(loans$delinq.2yrs)
loans$pub.rec <- as.factor(loans$pub.rec)
loans$not.fully.paid <- as.factor(loans$not.fully.paid)

##### Data Exploration  #####

ggplot(loans, aes(fico)) + geom_histogram(aes(fill=not.fully.paid), color='black') + theme_bw()

ggplot(loans, aes(factor(purpose))) + geom_bar(aes(fill=not.fully.paid), position='dodge') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(loans, aes(int.rate, fico)) + geom_point(aes(color=not.fully.paid), alpha=0.5) + theme_bw()

#####  Train and test the data (70/30) #####

set.seed(101)

sample <-sample.split(loans$not.fully.paid, SplitRatio = 0.7)

train <- subset(loans, sample == TRUE)

test <- subset(loans, sample == FALSE)

#####  Create SVM Model  #####

model <- svm(not.fully.paid ~., data = train[1:14])

summary(model)

predict.values <- predict(model,test[1:13])

table(predict.values, test$not.fully.paid)

#####  Tune model  #####

tuned.svm <-svm(not.fully.paid ~., data=train[1:14], kernal='radial', cost =70, gamma=0.2)

predicted.values <- predict(tuned.svm, test[1:13])

table(predicted.values, test$not.fully.paid)

#####  Evaluate the model  #####

accuracy <- (2133 + 102)/(2133 + 358 + 280 + 102)

print(accuracy)

precision <- 102/(102+358)

print(precision)

recall <- 102/(102+280)

print(recall)

























