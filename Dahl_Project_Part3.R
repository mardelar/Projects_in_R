#####  Install and Call  #####

install.packages("dplyr")
install.packages("tidyverse")
install.packages("readxl")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("psych")
install.packages("corrplot")

library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(psych)
library(corrplot)

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

projectdata2$EVENT_TIME <- as.POSIXct(projectdata2$EVENT_TIME, origin="1970-01-01", tz="UTC")

projectdata2$EVENT_TIME <- format(projectdata2$EVENT_TIME, "%Y-%m-%d %H:%M:%S")

print(head(projectdata2))

projectdata2$EVENT_TIME <- as.POSIXct(projectdata2$EVENT_TIME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#####   Round EVENT_TIME to top of the hour  #####

projectdata2$HourRounded <- floor_date(projectdata2$EVENT_TIME, "hour")

projectdata2_filtered <- projectdata2 %>%
  group_by(HourRounded) %>%
  mutate(Diff = abs(difftime(EVENT_TIME, HourRounded, units = "mins"))) %>%
  arrange(Diff) %>%
  slice(1) %>%
  ungroup() %>%
  select(-Diff) 

print(head(projectdata2_filtered))

projectdata2 <- projectdata2 %>% select(-EVENT_TIME)

print(head(projectdata2))

projectdata$`Door to Bed Time for Last ED Patient` <- round(projectdata$`Door to Bed Time for Last ED Patient`, 2)

print(head(projectdata))


#####  Join both Datasets  #####

projectdata <- left_join(projectdata, projectdata2 %>% 
                           select(`HourRounded`, `DEPT_NEDOC_SCORE`), 
                         by = c("Date and Time" = "HourRounded"))

#####  Remove NA's  #####

projectdata <- projectdata[!is.na(projectdata$`Door to Bed Time for Last ED Patient`), ]

str(projectdata)

summary(projectdata)

describe(projectdata)

#####  Histogram for # of ED Pts  #####

ggplot(projectdata, aes(x = `# of ED Pts`)) + 
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of # of ED Patients", x = "# of ED Patients")

#####  Scatter Plot between # of ED Pts and DEPT_NEDOC_SCORE  #####

ggplot(projectdata, aes(x = `# of ED Pts`, y = DEPT_NEDOC_SCORE)) + 
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Relationship Between # of ED Patients and DEPT_NEDOC_SCORE",
       x = "# of ED Patients", y = "DEPT_NEDOC_SCORE")

#####  Box Plot for Door to Bed Time for Last ED Patient  #####

ggplot(projectdata, aes(y = `Door to Bed Time for Last ED Patient`)) + 
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Box Plot of Door to Bed Time for Last ED Patient", y = "Time (Hours)")

#####  Hypothesis Test  #####

t_test_result <- t.test(projectdata$DEPT_NEDOC_SCORE, mu = 100)

print(t_test_result)

#####  Make NEDOC Score a binary. Used median for the threshold  #####

projectdata$Group <- ifelse(projectdata$DEPT_NEDOC_SCORE > median(projectdata$DEPT_NEDOC_SCORE, na.rm = TRUE),
                            "1", "0")

ggplot(projectdata, aes(x = Group, y = DEPT_NEDOC_SCORE, fill = Group)) +
  geom_boxplot() +
  labs(title = "DEPT_NEDOC_SCORE by Group", x = "Group", y = "DEPT_NEDOC_SCORE") +
  theme_minimal()

#####  Anova  #####

anova_result <- aov(DEPT_NEDOC_SCORE ~ Group, data = projectdata)
summary(anova_result)

##### Correlation Matrix  #####

numeric_vars <- projectdata[sapply(projectdata, is.numeric)]

cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of ED Variables", order = "original")















