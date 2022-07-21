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

