#####  Install and Call Libraries  #####


install.packages("tidyverse")
install.packages("lubridate")
install.packages("corrplot")
install.packages("zoo")
install.packages("randomForest")
install.packages("dpylr")
install.packages("scales")

library(tidyverse)
library(lubridate)
library(corrplot)
library(zoo)
library(plyr)
library(magrittr)
library(ROSE)
library(caTools)
library(ROCR)
library(caret)
library(randomForest)
library(dplyr)
library(e1071)
library(scales)
        
        
#####  Load Data  #####

loandata <- read.csv((file.choose()))

dim(loandata)

head(loandata)

loandata1 <- loandata %>% 
  select(loan_status, issue_d, loan_amnt, emp_title, emp_length, verification_status, 
         home_ownership, annual_inc, purpose, inq_last_6mths, desc,
         open_acc, pub_rec, revol_util, dti, total_acc, delinq_2yrs, 
         earliest_cr_line, mths_since_last_delinq)

head(loandata1)

str(loandata1)

loandata2 <- loandata1 %>% 
  mutate(default = ifelse(loan_status=="Charged Off",1,0))

loandata2$earliest_cr_line <- as.Date(as.yearmon(loandata$earliest_cr_line, "%b-%y"))

loandata2$issue_d <- as.Date(as.yearmon(loandata$issue_d, "%b-%y"))

loandata3 <- loandata2 %>%
  mutate(time_history = issue_d - earliest_cr_line)

head(loandata3$revol_util)

loandata4 <- loandata3 %>%
  mutate(revol_util = as.numeric(sub("%","", revol_util)))

head(loandata4$revol_util)

loandata5 <- loandata4 %>%
  mutate(empdetailgiven = as.numeric(!is.na(emp_title)),
         reasonforloan = as.numeric(!is.na(desc)), 
         previousdefault = as.numeric(!is.na(mths_since_last_delinq)))

loandata6 <- loandata5 %>%
  mutate(emplengthgiven = ifelse(emp_length == "n/a", 1, 0),
         emp_length = ifelse(emp_length == "< 1 year" | emp_length == "n/a", 0, emp_length),
         emp_length = as.numeric(gsub("\\D", "", emp_length)),
         home_ownership = ifelse(home_ownership == "NONE", "OTHER", home_ownership))

loandata7 <- loandata6 %>% 
  select(default, loan_amnt, reasonforloan, empdetailgiven, emplengthgiven, emp_length, verification_status, 
         home_ownership, annual_inc, purpose, time_history, inq_last_6mths, 
         open_acc, pub_rec, revol_util, dti, total_acc, delinq_2yrs, previousdefault)

factorcolumns <- c("default", "reasonforloan", "empdetailgiven", "emplengthgiven",
                   "verification_status", "home_ownership", "purpose", "previousdefault")

loandata7 %<>% mutate_each_(funs(factor(.)), factorcolumns)

loandata7$time_history <- as.numeric(loandata7$time_history)

str(loandata7)

Loanamountdist <- ggplot(data=loandata7, aes(x=loandata7$loan_amnt)) + 
  geom_histogram(aes(y=..density..),
                 col='black', 
                 fill='dodgerblue1', 
                 alpha=0.3) +
  geom_density(adjust=3)

print(Loanamountdist + theme(plot.title=element_text(face="bold")) + ggtitle('Distribution of the loan amounts'))

homedist <- ggplot(data=loandata7, aes(x=loandata7$home_ownership, fill=default)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), position='stack', alpha=0.5) + scale_y_continuous(labels=scales::percent)

print(homedist + theme(plot.title=element_text(face="bold")) + ggtitle('Home Ownership vs Loan Default'))

loandata7$time_history <- abs(loandata7$time_history)

CreditAge <- ggplot(data=loandata7, aes(x=loandata7$time_history, fill=default)) + 
  geom_histogram(alpha=0.5) 

print(CreditAge + theme(plot.title=element_text(face="bold")) + ggtitle('Credit Account Age vs Default'))


numcol <- sapply(loandata7,is.numeric)
pearsoncor <- cor(loandata7[numcol], use="complete.obs")
corrplot(pearsoncor, "number")

openaccplot <- ggplot(loandata7, aes(open_acc)) + geom_histogram(col='black',fill='dodgerblue1', alpha=0.3) 

print(openaccplot + theme(plot.title=element_text(face="bold")) + ggtitle('Histogram of number of open accounts'))

totaccplot <- ggplot(loandata7, aes(total_acc)) + geom_histogram(col='black',fill='dodgerblue1', alpha=0.3)

print(totaccplot + theme(plot.title=element_text(face="bold")) + ggtitle('Histogram of total number of accounts'))

plot(open_acc ~ default, data=loandata7)

plot(total_acc ~ default, data=loandata7)

results <- aov(open_acc ~ default, data=loandata7)

summary(results)

results2 <- aov(total_acc ~ default, data=loandata7)

summary(results2)

index <- which(colnames(loandata7)=="open_acc")

loandata10 <- loandata7[,-c(index)]

table(loandata10$default)

balanceddata <- ovun.sample(default ~ . , 
                            data=loandata10, method = "under", N = 11340, seed = 1)$data
table(balanceddata$default)

set.seed(101)

sample <- sample.split(balanceddata$default, SplitRatio = .75)

train <- subset(balanceddata, sample == TRUE)

test  <- subset(balanceddata, sample == FALSE)

#####  Naive Bayes Model  #####

naiveBayesModel <- naiveBayes(default ~ loan_amnt + reasonforloan + empdetailgiven + emplengthgiven
                              + verification_status + home_ownership + annual_inc + purpose
                              + previousdefault + pub_rec, data = train)

nbPredictions <- predict(naiveBayesModel, test)

nbTable <- table(Predicted = nbPredictions, Actual = test$default)

nbAccuracy <- sum(diag(nbTable)) / sum(nbTable)

print(paste('Naive Bayes Accuracy:', nbAccuracy))

##### SVM Model #####

svmModel <- svm(default ~ loan_amnt + emplengthgiven
                + verification_status + home_ownership + annual_inc + purpose
                + previousdefault + pub_rec, family=binomial(link='logit'), data = train, kernel = "radial", cost = 1, scale = FALSE)


svmPredictions <- predict(svmModel, test[, -which(names(test) == "default")])


svmTable <- table(Predicted = svmPredictions, Actual = test$default)
svmAccuracy <- sum(diag(svmTable)) / sum(svmTable)

print(paste('SVM Accuracy:', svmAccuracy))

#####  Logistic  Regression  #####

logisticreg <- glm(default ~ loan_amnt + emplengthgiven
                   + verification_status + home_ownership + annual_inc + purpose
                   + previousdefault + pub_rec, family=binomial(link='logit'), data = train)

actualvalues <- test$default

pred <- predict(logisticreg,test, type ='response')

pred <- ifelse(pred > 0.5,1,0)

compare <- data.frame(actual = actualvalues, predicted = pred)

misClasificError <- mean(pred != actualvalues)

print(paste('Accuracy',1-misClasificError))

str(train)

#####  Random Forest  #####

rfmodel <- randomForest(default ~ loan_amnt + reasonforloan + empdetailgiven + emplengthgiven
                        + verification_status + home_ownership + annual_inc + purpose
                        + previousdefault + pub_rec, data=train, importance = TRUE, ntree = 200, na.action = na.omit)
predrf <- predict(rfmodel, test, type='class')
rfoutput <- confusionMatrix(test$default, predrf)
paste0(rfoutput$overall[1])



val <- as.numeric(paste0(pred))
predObj <- prediction(val,actualvalues)
rocObj <- performance(predObj, measure="tpr", x.measure="fpr") 
aucObj <- performance(predObj, measure="auc")


predrfprob <- predict(rfmodel, test, type = "prob")
val2 <- as.numeric(paste0(predrfprob[,2]))
predObj2 <- prediction(val2, actualvalues)
rocObj2 <- performance(predObj2, measure="tpr", x.measure="fpr") 
aucObj2 <- performance(predObj2, measure="auc")

plot(rocObj, col = "blue", lwd = 1, main = "ROC Curves")
plot(rocObj2, add = TRUE, col = "red")
abline(a=0, b=1)

aucObj@y.values[[1]]
aucObj2@y.values[[1]]

accuracy_logistic <- 0.5776  
accuracy_randomForest <- 0.5774  
accuracy_svm <- 0.5014  
accuracy_naiveBayes <- 0.5571  

#####  Model Comparison  #####

model_accuracy <- c(Logistic_Regression = accuracy_logistic,
                      Random_Forest = accuracy_randomForest,
                      SVM = accuracy_svm,
                      Naive_Bayes = accuracy_naiveBayes)

accuracy_data <- data.frame(Model = names(model_accuracy), Accuracy = model_accuracy * 100)

accuracy_plot <- ggplot(accuracy_data, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Accuracy Percentage") +
  ggtitle("Comparison of Model Accuracies")

print(accuracy_plot)

accuracy_plot_line <- ggplot(accuracy_data, aes(x = Model, y = Accuracy, group = 1)) + 
  geom_line(aes(color = Model), size = 1) +  
  geom_point(aes(color = Model), size = 3) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Logistic_Regression" = "blue", 
                                "Random_Forest" = "red", 
                                "SVM" = "green", 
                                "Naive_Bayes" = "purple")) + 
  ylab("Accuracy Percentage") +
  ggtitle("Comparison of Model Accuracies (Line Plot)") +
  theme(legend.title = element_blank()) 

print(accuracy_plot_line)

importanceScores <- importance(rfmodel)

print(importanceScores)


























