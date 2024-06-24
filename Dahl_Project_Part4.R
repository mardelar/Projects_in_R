#####  Install and Call  #####

install.packages("dplyr")
install.packages("tidyverse")
install.packages("readxl")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("psych")
install.packages("corrplot")
install.packages("stats")

library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(psych)
library(corrplot)
library(stats)


#####  Load Data  #####

projectdata <- read_excel("C:/Users/marde/OneDrive/DSIM_608/Project data (1).xlsx")

projectdata2 <- read_excel("C:/Users/marde/OneDrive/DSIM_608/Project_sheet2.xlsx")

#####  Convert Dates  #####

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

projectdata2 <- projectdata2 %>%
  select(-EVENT_TIME)

projectdata2_unique <- projectdata2[seq(1, nrow(projectdata2), by = 4), ]


projectdata2_unique <- rename(projectdata2_unique, "Date and Time" = "HourRounded")

projectdata$"Door to Bed Time for Last ED Patient" [3] <- 1.1

left_join(projectdata, projectdata2_unique, by = "Date and Time")



projectdata$DEPT_NEDOC_SCORE <- projectdata2_unique$DEPT_NEDOC_SCORE


str(projectdata)

projectdata$`Door to Bed Time for Last ED Patient` <- as.numeric(as.character(projectdata$`Door to Bed Time for Last ED Patient`))
projectdata$`Door to Bed Time for Last ED Patient` <- projectdata$`Door to Bed Time for Last ED Patient` * 60

projectdata$`Longest Admit Time Waiting in ED` <- projectdata$`Longest Admit Time Waiting in ED` * 60

str(projectdata)

names(projectdata)


model <- lm(`Door to Bed Time for Last ED Patient` ~ ., data = projectdata)


summary(model)


ggplot(projectdata, aes(x=`# of ED Pts`, y=`Door to Bed Time for Last ED Patient`)) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) +  # Add the regression line
  theme_minimal() +  
  labs(x="# of ED Pts", y="Door to Bed Time for Last ED Patient", title="Linear Regression Model")

ggplot(projectdata, aes(x=`# of ED Pts Waiting IP Bed`, y=`Door to Bed Time for Last ED Patient`)) +
  geom_point() +  
  geom_smooth(method="lm", se=FALSE) +  
  theme_minimal() +  
  labs(x="# of ED Pts Waiting IP Bed", y="Door to Bed Time for Last ED Patient", title="Linear Regression Model")

projectdata <- projectdata %>%
  mutate(NEDOC_binary = ifelse(DEPT_NEDOC_SCORE > median(DEPT_NEDOC_SCORE, na.rm = TRUE), 1, 0))


hours <- 0:23  # Hours of the day
days_of_week <- factor(1:7, levels = 1:7, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))# Assuming you have a model like lm(CriticalCarePatients ~ Hour + DayOfWeek, data = yourData)
hours <- 0:23  # Hours of the day
days_of_week <- factor(1:7, levels = 1:7, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


predict_data <- expand.grid(DayOfWeek = days_of_week, Hour = hours)


predict_data <- expand.grid(DayOfWeek = days_of_week, Hour = hours)


hours <- 0:23  # Hours of the day
days_of_week <- factor(1:7, levels = 1:7, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


predict_data <- expand.grid(DayOfWeek = days_of_week, Hour = hours)

head(projectdata)
      
model <- lm(DEPT_NEDOC_SCORE ~ ., data = projectdata)

summary(model)



















