#####  Call up the libraries  #####

library(dplyr) 
library(tidyverse)
library(stargazer)
library(descr) 
library(forcats) 
library(e1071) 
library(caret) 

#####  Download data  #####

loan <- read.csv(file.choose())

#####  Filter Data  #####

loan <- filter(loan, loan_status!="")

loan$status <- ifelse(loan$loan_status=="Current" | 
                        loan$loan_status=="Fully Paid" |
                        loan$loan_status=="Does not meet the credit policy.  Status:Fully Paid","good","bad")

loan$status <- as.factor(loan$status)

#####  Categorize new FICO & dti variables #####

loan$fico <- (loan$fico_range_high+loan$fico_range_low)/2



summary(loan$fico)

loan$ficocat <- cut(loan$fico, breaks=c(0,687,742,1000),
                    labels=c("bottom 25%","middle 50%", "top 25%"))

table(loan$ficocat)

ggplot(loan, aes(x = ficocat)) + 
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "FICO Categories", x = "FICO Category", y = "Count") +
  theme_minimal()



summary(loan$dti)

loan$dticat <- cut(loan$dti, breaks=c(0,8.2,18.68,100),
                   labels=c("bottom 25%","middle 50%", "top 25%"))

table(loan$dticat)

ggplot(loan, aes(x = dticat)) + 
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "dti Categories", x = "dti Category", y = "Count") +
  theme_minimal()



#####  Naive Bayes  #####

table(loan$purpose)

loan$purpose <- factor(loan$purpose)

levels(loan$purpose)

loan$purpose <- fct_collapse(loan$purpose, other=c("renewable_energy", "other"))

levels(loan$purpose)

table(loan$purpose)

ggplot(loan, aes(x = purpose)) + 
  geom_bar(fill = "purple", color = "black") + 
  coord_flip() + 
  labs(title = "Loan Purpose", x = "Purpose", y = "Count") +
  theme_minimal()

#####  Train and Test data  #####

loan <- select(loan, status, ficocat, dticat, purpose)

set.seed(364)

loan$rand <- runif(nrow(loan))

train <- filter(loan, rand<=0.8)

test <- filter(loan, rand>0.8)

classifier <- naiveBayes(status ~ ficocat+dticat+purpose,train)

classifier

prediction <- predict(classifier, select(test, ficocat, dticat, purpose), type="raw")

summary(prediction)

test$status_pred <- ifelse(prediction[,"good"] > 0.65, "good", "bad")

table(test$status_pred)

test1<-factor(test$status)

test2<-factor(test$status_pred)

confusionMatrix(test2, test1)





























