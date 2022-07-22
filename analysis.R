## This file includes the raw code for the analysis

## Setup
library(tidyverse)
library(lubridate)

## Load data
activity_data <- read_csv("activity.zip")

## Total steps per day
# steps by day
daily_steps <- activity_data %>%
    group_by(date) %>% summarize(steps = sum(steps, na.rm = TRUE))
# histogram of daily steps
qplot(daily_steps$steps, fill = I("blue"), bins = 30, 
      main = "Figure 1: Histogram of total steps per day", 
      xlab = "Number of steps")
# Formatted mean and median
summarize(daily_steps, Mean=mean(steps), Median=median(steps)) %>% 
    xtable(caption = "Steps per day", digits=c(0,2,0)) %>%
    print(type = "html")

## Average daily activity pattern
daily_pattern <- activity_data %>%
    group_by(interval) %>% summarize(steps = mean(steps, na.rm = TRUE))
qplot(interval, steps, data = daily_pattern, 
      geom = c("line"), 
      ylab = "Average steps", 
      xlab = "5-minute intervals", 
      main = "Figure 2: Average Daily Pattern of Steps")

## Missing values
sum(is.na(activity_data$steps))
round(mean(is.na(activity_data$steps)) * 100, 2)
activity_data %>% group_by(date) %>% 
    summarize(missing_share = mean(is.na(steps))) %>% filter(missing > 0)
activity_imputed <- activity_data %>% group_by(interval) %>% 
    mutate(steps = if_else(is.na(steps), mean(steps, na.rm = TRUE), steps))
daily_imputed <- activity_imputed %>%
    group_by(date) %>% summarize(steps = sum(steps, na.rm = TRUE))
summarize(daily_imputed, Mean=mean(steps), Median=median(steps))
qplot(daily_imputed$steps, fill = I("blue"), bins = 30, 
      main = "Figure 3: Histogram of total steps per day with missing values imputed",
      xlab = "Number of steps")

