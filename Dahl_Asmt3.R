#####  Install Packages  #####

install.packages("openxlsx")
library(openxlsx)
install.packages('ISLR')
library(ISLR)
install.packages('caTools')
library(caTools)
install.packages('dplyr')
library(dplyr)
install.packages('neuralnet')
library(neuralnet)
install.packages('caret')
library(caret)
install.packages('ROSE')
library(ROSE)

#####  Load Data Set  #####

loan7 <- read_excel("C:/Users/marde/OneDrive/DSIM_606/loan7.xlsx")

print(head(loan7,3))

#####  Select all variables for Neural Network model  #####

new_loan7 <- select(loan7, fico, dti, int_rate, annual_inc, status)

#####  Normalize variables using Min-Max scaling  #####

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

numeric_columns <- sapply(new_loan7, is.numeric)

new_loan7[numeric_columns] <- as.data.frame(lapply(new_loan7[numeric_columns], normalize))

new_loan7 <- new_loan7 %>%
  mutate(status = ifelse(status == "good", 1, 0))

#####  Set.seed is for reproducibility  #####

set.seed(7) 

##### Split train and test sets @ 70%  #####

split <- sample.split(new_loan7$status, SplitRatio = 0.7)
train_set <- new_loan7[split, ]
test_set <- new_loan7[!split, ]

train_set_balanced <- ovun.sample(status ~ ., data = train_set, 
                                  method = "over", N = nrow(train_set))$data



#####  Run Neural Network model with 1 hidden layer and 4 neurons  #####

nn_model <- neuralnet(status ~ fico + dti + int_rate + annual_inc, 
                      data = train_set_balanced, 
                      hidden = c(4),  
                      linear.output = FALSE)  

plot(nn_model)

#####  Test Neural Network model  #####

test_predictions <- compute(nn_model, test_set[, c("fico", "dti", "int_rate", "annual_inc")])

predicted_values <- test_predictions$net.result

predicted_classes <- ifelse(predicted_values > .5, 1, 0) 

predicted_classes <- factor(predicted_classes, levels = c(1,0))

actual_classes <- factor(test_set$status, levels = c(1,0))

#####  Evaluate the model using Confusion Matrix  #####

confusionMatrix(predicted_classes, actual_classes)











