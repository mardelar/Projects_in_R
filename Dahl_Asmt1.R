#install.packages("ggplot2")

install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

mydata<-read.csv(file.choose())

mydata$Date <- as.Date(mydata$Date, "%Y-%m-%d")

mydata <- select(mydata, Date, Close)

mydata <- rename(mydata, SP500=Close)                      

mydata <- arrange(mydata, Date)

mydata <- filter(mydata, Date>=as.Date("1990-01-01"))

mydata$SP500_lag <- lag(mydata$SP500)                     

mydata$SP500return <- (mydata$SP500-mydata$SP500_lag)/mydata$SP500_lag*100                      

summary(mydata$SP500return)

mydata <- arrange(mydata, SP500return)

ggplot(mydata, aes(x=SP500return)) + geom_histogram() 

library(lubridate)

mydata$month=month(mydata$Date)

mydata <- arrange(mydata, Date)

monthly <- filter(mydata, mydata$month != lead(mydata$month))

monthly <- select(monthly, Date, month, SP500)


##### DIY Assignment #####

foodata<-read.csv(file.choose())

foodata$Date <- as.Date(foodata$Date, "%Y-%m-%d")

foodata <- select(foodata, Date, Close)

foodata <- rename(foodata, NASDQ = Close)

foodata$NASDQ_lag <- lag(foodata$NASDQ)

foodata$NASDQreturn <- (foodata$NASDQ-foodata$NASDQ_lag)/foodata$NASDQ_lag*100


jan_feb <- foodata %>% filter(between(Date, as.Date("2020-01-01"), as.Date("2020-03-01")))

mean_return <- mean(jan_feb$NASDQreturn, na.rm = TRUE)

best_day <- foodata %>% arrange(desc(NASDQreturn)) %>% head(1)

worst_day <- foodata %>% arrange(NASDQreturn) %>% head(1)


summary(foodata$NASDQreturn)




jan20_return <- foodata %>% filter(between(Date, as.Date("2020-01-01"), as.Date("2020-02-01")))

jan21_return <- foodata %>% filter(between(Date, as.Date("2021-01-01"), as.Date("2021-02-01")))

ggplot(jan20_return, aes(x=NASDQreturn)) + geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  theme_minimal() + labs(title = "Histogram of January 2020 Returns", x = "January 2020 Returns", y = "Frequency")

ggplot(jan21_return, aes(x = Date, y = NASDQreturn)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = "Time Plot of Daily Returns for January 2021",
       x = "Date", y = "Daily Return") + 
  theme_minimal() 

library(lubridate)

foodata$month = month(foodata$Date)

foodata <- arrange(foodata, Date)

foomonthly <- filter(foodata, foodata$month != lead(foodata$month))

foomonthly <- select(foomonthly, Date, month, NASDQ, NASDQ_lag, NASDQreturn)

foomonthly <- foomonthly %>%
  arrange(Date) %>%
  mutate(NASDQ_lag = lag(NASDQreturn)) %>%
  mutate(monthlyreturn = (NASDQreturn - NASDQ_lag) / NASDQ_lag)

avg_monthlyreturn <- foomonthly %>%
  filter(!is.na(monthlyreturn)) %>%
  summarise(Avgreturn = mean(monthlyreturn, na.rm = TRUE))

best_month <- foomonthly %>% arrange(desc(monthlyreturn)) %>% head(1)

worst_month <- foomonthly %>% arrange(monthlyreturn) %>% head(1)






