install.packages("dyplr")
install.packages("ggplot2")


library(tidyverse)
library(stargazer)
library(descr)

loan7 <- read.csv(file.choose(), skip = 1)

summary(loan7)

table(loan7$loan_status)

loan7 <- filter(loan7, loan_status!="")

loan7 <- loan7 %>% filter(loan_status != "Does not meet the credit policy. Status:Fully Paid" &
                            loan_status != "Does not meet the credit policy. Status:Charged Off")

loan7$status <- ifelse(loan7$loan_status == "Current" | 
                         loan7$loan_status == "Fully Paid", "good","bad")

table(loan7$status)

loan7$int_rate <- as.numeric(gsub("%","", loan7$int_rate))

ggplot(loan7, aes(x=status,y=int_rate)) + geom_boxplot()

loan7$fico <- (loan7$fico_range_high+loan7$fico_range_low)/2

loan7 <- as.data.frame(loan7)

stargazer(select(filter(loan7, status == "good"), dti, fico), median = TRUE, type = "text")

stargazer(select(filter(loan7, status == "bad"), dti, fico),  median = TRUE, type = "text")

stargazer(select(filter(loan7, status == "good"), annual_inc, int_rate), median = TRUE, type = "text")

stargazer(select(filter(loan7, status == "bad"), annual_inc, int_rate ),  median = TRUE, type = "text")

ggplot(aes(x = dti, color = status) ,data = loan7) + geom_density()

ggplot(aes(x = fico, color = status) ,data = loan7) + geom_density()

ggplot(loan7, aes(x = annual_inc, fill = status)) +
  geom_histogram(bins = 30, alpha = 0.6) + labs(x = "Annual Income", y = "Count") + theme_minimal()

ggplot(loan7, aes(x=status,y=annual_inc)) + geom_boxplot()

loan7 <- loan7 %>% filter(!is.na(dti), !is.na(int_rate), !is.na(annual_inc), !is.na(status))

loan7 <- as.data.frame(loan7)

loan7 <- select(loan7, fico, dti, annual_inc, int_rate, status)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
loan7 <- loan7 %>%
  mutate(fico_n = normalize(fico), dti_n = normalize(dti),
         annual_inc_n = normalize(annual_inc),
         int_rate_n = normalize(int_rate))

summary(select(loan7, fico, fico_n, dti, dti_n, annual_inc, annual_inc_n,
               int_rate, int_rate_n))

loan7 <- loan7 %>%
  mutate(fico_n_log = log(fico_n +1), dti_n_log = log(dti_n + 1),
         annual_inc_n_log = log(annual_inc_n + 1),
         int_rate_n_log = log(int_rate_n + 1))

summary(select(loan7, fico_n_log, dti_n_log, annual_inc_n_log, int_rate_n_log))

loan7$status <- as.factor(loan7$status)

model <- glm(status ~  fico_n + dti_n + int_rate_n + annual_inc_n_log,
             data = loan7, family = "binomial")

summary(model)

loan7$status_numeric <- ifelse(loan7$status == "good", 1, 0)

predictions <- predict(model, type = "response")

predicted_status <- ifelse(predictions > 0.5, 1, 0)

accuracy <- mean(predicted_status == loan7$status_numeric)

summary(model)

summary(accuracy)

print(accuracy)

###### Save as an RDS File

saveRDS(loan7, "loan7_clean.rds") 

write.csv(loan7, "loan7_clean.csv", row.names = FALSE)







