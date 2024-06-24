#####  Install and Call  #####

install.packages("dplyr")
install.packages("tidyverse")
install.packages("readxl")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("e1071")
install.packages("caTools")
install.packages("caret")
install.packages("reshape2")
install.packages("Metrics")


library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(corrplot)
library(caTools)
library(caret)
library(e1071)
library(reshape2)
library(Metrics)


#####  Load Data  #####

projectdata <- read_excel("C:/Users/marde/OneDrive/DSIM_608/Project data (1).xlsx")

projectdata2 <- read_excel("C:/Users/marde/OneDrive/DSIM_608/Project_sheet2.xlsx")


#####  Convert Time  #####

convert_mixed_dates <- function(date_column) {
  sapply(date_column, function(x) {
    if(grepl("-", x)) {
      
      tryCatch(as.POSIXct(x, format = "%d-%b-%Y %H:%M:%S", tz = "UTC"),
               error = function(e) NA)
    } else {
      
      tryCatch(as.POSIXct(as.numeric(x) * (24*60*60), origin="1899-12-30", tz = "UTC"),
               error = function(e) NA)
    }
  }, USE.NAMES = FALSE)
}

projectdata2$EVENT_TIME <- convert_mixed_dates(projectdata2$EVENT_TIME)

print(head(projectdata2))

projectdata2$EVENT_TIME <- as.POSIXct(projectdata2$EVENT_TIME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#####   Round EVENT_TIME to top of the hour  #####

projectdata2$HourRounded <- floor_date(projectdata2$EVENT_TIME, "hour")

#####  Remove EVENT_TIME  #####

projectdata2 <- projectdata2 %>%
  select(-EVENT_TIME)

#####  REMOVE DUPLICATE ENTRIES

projectdata2_unique <- projectdata2[seq(1, nrow(projectdata2), by = 4), ]

#####  Rename column to join  #####

projectdata2_unique <- rename(projectdata2_unique, "Date and Time" = "HourRounded")

left_join(projectdata, projectdata2_unique, by = "Date and Time")

#####  Add 1.1 to replace V in column  #####

projectdata$"Door to Bed Time for Last ED Patient" [3] <- 1.1

projectdata$DEPT_NEDOC_SCORE <- projectdata2_unique$DEPT_NEDOC_SCORE

str(projectdata)

#####  Convert to minutes  #####

projectdata$`Door to Bed Time for Last ED Patient` <- as.numeric(as.character(projectdata$`Door to Bed Time for Last ED Patient`))
projectdata$`Door to Bed Time for Last ED Patient` <- projectdata$`Door to Bed Time for Last ED Patient` * 60

projectdata$`Longest Admit Time Waiting in ED` <- projectdata$`Longest Admit Time Waiting in ED` * 60

str(projectdata)

names(projectdata)

#####  Convert NEDOC score to numeric  #####

projectdata$DEPT_NEDOC_SCORE <- as.numeric(as.character(projectdata$DEPT_NEDOC_SCORE))

projectdata$NEDOC_Category <- cut(projectdata$DEPT_NEDOC_SCORE,
                                  breaks = c(-Inf, 60, 100, 140, Inf),
                                  labels = c("Busy", "Very Busy", "Overcrowded", "Extremely Overcrowded"),
                                  right = FALSE)

projectdata <- projectdata %>%
  mutate(
    DateAndTime = as.POSIXct(`Date and Time`, format = "%Y-%m-%d %H:%M:%S"),
    Hour = hour(DateAndTime),
    DayOfWeek = wday(DateAndTime, label = TRUE),
    Month = month(DateAndTime),
    Year = year(DateAndTime)
  )

ggplot(projectdata, aes(x = Hour, fill = NEDOC_Category)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Hour of Day", y = "Percentage", fill = "NEDOC Category",
       title = "ED Overcrowding by Hour of Day") +
  theme_minimal()

ggplot(projectdata, aes(x = DayOfWeek, fill = NEDOC_Category)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Day of Week", y = "Percentage", fill = "NEDOC Category",
       title = "ED Overcrowding by Day of Week") +
  theme_minimal()

projectdata <- projectdata %>%
  mutate(
    DayOfWeek = wday(`Date and Time`, label = TRUE), # Ensure this exists
    IsWeekend = if_else(DayOfWeek %in% c('Sat', 'Sun'), 1, 0) # 1 for weekends, 0 for weekdays
  )

projectdata <- projectdata %>%
  mutate(
    TimeOfDay = case_when(
      Hour >= 6 & Hour < 12 ~ 'Morning',
      Hour >= 12 & Hour < 18 ~ 'Afternoon',
      Hour >= 18 & Hour < 24 ~ 'Evening',
      TRUE ~ 'Night'
    )
  )

projectdata$NEDOC_Category <- as.factor(projectdata$NEDOC_Category)

str(projectdata$NEDOC_Category)

projectdata$DayOfWeek <- as.factor(projectdata$DayOfWeek)

str(projectdata$DayOfWeek)

projectdata$TimeOfDay <- as.factor(projectdata$TimeOfDay)

summary(projectdata)

#####  Linear Regression  #####

summary(factor(projectdata$TimeOfDay))

projectdata$DayOfWeek <- wday(projectdata$DateAndTime, label = TRUE)

contrasts(projectdata$DayOfWeek) <- contr.treatment(levels(projectdata$DayOfWeek))

lm_model <- lm(DEPT_NEDOC_SCORE ~ DayOfWeek + `# of ED Pts` + `# of ED Pts Waiting IP Bed` + `# of Critical Care Pts - display` + 
                 `Door to Bed Time for Last ED Patient` + `Longest Admit Time Waiting in ED` + 
                 Hour + TimeOfDay, 
               data = projectdata)

summary(lm_model)


#####  SVM Regression Model  #####

#####  Split the Data  #####

set.seed(123)  
trainingIndex <- createDataPartition(projectdata$DEPT_NEDOC_SCORE, p = .7, list = FALSE)
trainingData <- projectdata[trainingIndex, ]
testingData <- projectdata[-trainingIndex, ]

zeroVarVars <- sapply(trainingData, function(x) length(unique(x)) == 1)

names(zeroVarVars[zeroVarVars])

trainingDataClean <- trainingData[, !zeroVarVars]

######  Train the SVM model  #####

svmModel <- svm(DEPT_NEDOC_SCORE ~ ., data = trainingDataClean, type = "eps-regression", kernel = "radial")

summary(svmModel)

#####  SVM Tuning  #####

tune.grid <- expand.grid(cost = 10^(-1:2), gamma = c(0.01, 0.1, 1))

tune.result <- tune(svm, train.x = DEPT_NEDOC_SCORE ~ ., data = trainingDataClean, 
                    kernel = "radial", ranges = tune.grid, 
                    tunecontrol = tune.control(cross = 10))

print(tune.result$best.parameters)

#####  Retrain SVM Model  #####

trainingData2 <- trainingData %>%
  select(-`# of ED Beds`, -`# of IP Beds`, -Month, -Year)

svmModel2 <- svm(DEPT_NEDOC_SCORE ~ ., data = trainingData2, 
                         type = "eps-regression",
                         kernel = "radial",
                         cost = 3, 
                         gamma = 0.01)

summary(svmModel2)

#####  Evaluate SVM Model  #####

predictions2 <- predict(svmModel2, newdata = testingData)

maeOptimized <- mae(testingData$DEPT_NEDOC_SCORE, predictions2)

rmseOptimized <- rmse(testingData$DEPT_NEDOC_SCORE, predictions2)

actuals <- testingData$DEPT_NEDOC_SCORE

predicted <- predictions2

r2Optimized <- cor(actuals, predicted)^2

cat("Mean Absolute Error (MAE):", maeOptimized, "\n")

cat("Root Mean Squared Error (RMSE):", rmseOptimized, "\n")

cat("R-squared (R²):", r2Optimized, "\n")

print(paste("Mean Absolute Error (MAE):", maeOptimized))

print(paste("Root Mean Squared Error (RMSE):", rmseOptimized))

print(paste("R-squared (R²):", r2Optimized))

#####  SVM Classification Model  #####

projectdata$NEDOC_Category <- as.factor(projectdata$NEDOC_Category)

set.seed(123)

trainingIndex <- createDataPartition(projectdata$NEDOC_Category, p = 0.7, list = FALSE)

trainingData <- projectdata[trainingIndex, ]

testingData <- projectdata[-trainingIndex, ]

trainingData <- trainingData %>% select(-`# of ED Beds`, -`# of IP Beds`, -Month, -Year)

svmModel <- svm(NEDOC_Category ~ ., data = trainingData, type = "C-classification", kernel = "radial")

predictions <- predict(svmModel, newdata = testingData)

confusionMatrix(predictions, testingData$NEDOC_Category)

summary(svmModel)

#####  Tune SVM Category Model  #####

tuningResult <- tune(svm, NEDOC_Category ~ ., data = trainingData,
                     type = "C-classification",
                     kernel = "radial",
                     ranges = list(cost = 10^(-1:2), gamma = 10^(-2:1)))

svmModel3 <- svm(NEDOC_Category ~ ., data = trainingData, type = "C-classification",
                         kernel = "radial", cost = tuningResult$best.parameters$cost,
                         gamma = tuningResult$best.parameters$gamma)
print(svmModel3)

tuneGrid <- expand.grid(sigma = 1 / (2 * tuningResult$best.parameters$gamma), 
                        C = tuningResult$best.parameters$cost)


trainControl <- trainControl(method = "cv", number = 10)


svmModelCV <- train(NEDOC_Category ~ ., data = trainingData,
                    method = "svmRadial",
                    trControl = trainControl,
                    preProcess = c("center", "scale"),
                    tuneGrid = tuneGrid)

print(svmModelCV)

summary(svmModelCV)

svmModelCV$results

ggplot(trainingData, aes(x = DEPT_NEDOC_SCORE, fill = NEDOC_Category)) +
  geom_histogram(alpha = 0.6, binwidth = 1) +
  labs(title = "Distribution of DEPT_NEDOC_SCORE by NEDOC Category",
       x = "DEPT_NEDOC_SCORE",
       y = "Count") +
  theme_minimal() +
  facet_wrap(~NEDOC_Category)

testingData$predicted_DEPT_NEDOC_SCORE <- predict(svmModel2, newdata = testingData)

#####  Scatter Plot  #####

ggplot(testingData, aes(x = DEPT_NEDOC_SCORE, y = predicted_DEPT_NEDOC_SCORE)) +
  geom_point(aes(color = NEDOC_Category), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Busy" = "blue", "Very Busy" = "green", "Overcrowded" = "yellow", "Extremely Overcrowded" = "red")) +
  labs(title = "Actual vs. Predicted DEPT_NEDOC_SCORE",
       x = "Actual DEPT_NEDOC_SCORE",
       y = "Predicted DEPT_NEDOC_SCORE") +
  theme_minimal()

#####  Confusion Matrix/ NEDOC Category  #####

confMat <- confusionMatrix(predictions, testingData$NEDOC_Category)

confMat_long <- melt(confMat$table)

confMat_long <- melt(as.matrix(confMat$table), varnames = c("Actual", "Predicted"))

ggplot(confMat_long, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(label = value), vjust = 1.5, color = "black") +
  labs(x = "Predicted", y = "Actual",
       title = "Confusion Matrix for SVM Classification",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####  Naive Bayes  #####

#####  Train & Test  #####

set.seed(123)

trainingIndex <- createDataPartition(projectdata$NEDOC_Category, p = 0.7, list = FALSE)

trainingDataNB <- projectdata[trainingIndex, ]

testingDataNB <- projectdata[-trainingIndex, ]

trainingDataNB <- trainingDataNB %>%
  select(DEPT_NEDOC_SCORE, TimeOfDay, DayOfWeek, `# of ED Pts`, `# of Critical Care Pts - display`, NEDOC_Category)

testingDataNB <- testingDataNB %>%
  select(DEPT_NEDOC_SCORE, TimeOfDay, DayOfWeek, `# of ED Pts`, `# of Critical Care Pts - display`, NEDOC_Category)

nbModel <- naiveBayes(NEDOC_Category ~ ., data = trainingDataNB)

print(nbModel)

#####  Evaluate Model  #####

nbPredictions <- predict(nbModel, newdata = testingDataNB)

nbConfMat <- confusionMatrix(nbPredictions, testingDataNB$NEDOC_Category)

print(nbConfMat)

nbAccuracy <- sum(diag(nbConfMat$table)) / sum(nbConfMat$table)

print(paste("Accuracy of Naïve Bayes Model:", nbAccuracy))

ggplot(projectdata, aes(x = DEPT_NEDOC_SCORE, fill = NEDOC_Category)) +
  geom_density(alpha = 0.5) +
  labs(title = "Probability Density Function of DEPT_NEDOC_SCORE by NEDOC Category",
       x = "DEPT_NEDOC_SCORE",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") 

ggplot(projectdata, aes(x = `# of ED Pts`, fill = NEDOC_Category)) +
  geom_density(alpha = 0.5) +
  labs(title = "Probability Density Function of # of ED Pts by NEDOC Category",
       x = "# of ED Pts",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") 

ggplot(projectdata, aes(x = `# of Critical Care Pts - display`, fill = NEDOC_Category)) +
  geom_density(alpha = 0.5) +
  labs(title = "Probability Density Function of # of Critical Care Pts by NEDOC Category",
       x = "# of Critical Care Pts - display",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") 























































