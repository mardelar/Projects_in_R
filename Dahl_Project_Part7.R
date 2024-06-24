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
install.packages("randomForest")
install.packages("reshape2")

library(randomForest)
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
    DayOfWeek = wday(`Date and Time`, label = TRUE), 
    IsWeekend = if_else(DayOfWeek %in% c('Sat', 'Sun'), 1, 0) 
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

#####  Change Columns with spaces to CamelBack  #####

names(projectdata) <- gsub(" ", "_", names(projectdata))

projectdata <- projectdata %>%
  rename(CriticalCarePts = `#_of_Critical_Care_Pts_-_display`)

projectdata <- projectdata %>%
  rename( numEDBeds = `#_of_ED_Beds`,
          numIPBeds =  `#_of_IP_Beds`,
          numEDPts = `#_of_ED_Pts`,
          numEDPtsWaiting = `#_of_ED_Pts_Waiting_IP_Bed`,
          DoorToBedTimeLastPts = Door_to_Bed_Time_for_Last_ED_Patient)
projectdata <- projectdata %>%
  rename(LongestAdmitWaitTime = Longest_Admit_Time_Waiting_in_ED)



#####  Random Forest Model  #####

set.seed(123)

#####  Remove unwanted columns  #####
projectdata <- projectdata[, !(names(projectdata) %in% c("DEPT_NEDOC_SCORE", "numEDBeds", "numIPBeds", "Month", "Year", "IsWeekend", "Date_and_Time"))]

#####  List Predictor Variables  #####
predictors <- c("numEDPts", "numEDPtsWaiting", "CriticalCarePts", "DoorToBedTimeLastPts", "LongestAdmitWaitTime", "Hour", "DayOfWeek", "TimeOfDay")
projectdata <- projectdata %>%
  mutate(across(all_of(predictors), as.numeric),
         NEDOC_Category = as.factor(NEDOC_Category))

#####  Train and Test Model  #####

partition <- createDataPartition(projectdata$NEDOC_Category, p = 0.8, list = FALSE)
training_set <- projectdata[partition, ]
test_set <- projectdata[-partition, ]

control <- trainControl(method = "cv", number = 10, savePredictions = "final")
rf_grid <- expand.grid(mtry = c(2, 3, 4, 5))  

rf_formula <- as.formula(paste("NEDOC_Category ~", paste(predictors, collapse = "+")))
rf_model <- train(rf_formula,
                  data = training_set,
                  method = "rf",
                  trControl = control,
                  tuneGrid = rf_grid,
                  ntree = 500)

print(rf_model)

predictions <- predict(rf_model, newdata = test_set)

#####  Evaluate Model Performance w/Confusion Matrix  #####

conf_mat <- confusionMatrix(predictions, test_set$NEDOC_Category)
print(conf_mat)

#####  Determine Variable Importance  #####

importance_vals <- varImp(rf_model, scale = TRUE)
print(importance_vals)
plot(importance_vals)

importance_df <- data.frame(
  Variable = c("numEDPts", "CriticalCarePts", "LongestAdmitWaitTime", "DoorToBedTimeLastPts", 
               "Hour", "numEDPtsWaiting", "DayOfWeek", "TimeOfDay"),
  Importance = c(100.00, 84.72, 66.23, 61.74, 56.65, 52.03, 24.61, 0.00)
)

importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE),]

#####  Graph Variable Importance  #####
  
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
    geom_bar(stat = "identity") +
    coord_flip() + 
    scale_fill_gradient(low = "yellow", high = "firebrick") + 
    theme_minimal() +
    labs(title = "Variable Importance in Predicting ED Overcrowding", 
         x = "Relative Importance", y = "Predictor Variables") +
    theme(axis.title.y = element_blank(), 
          legend.title = element_blank()) 
  












