library(tidyverse)
library(lubridate)
library(rStrava)

# get the authetication token and refresh it
stoken <- httr::config(token = readRDS('/home/pi/hdd/R/strava/.httr-oauth')[[1]])
stoken$auth_token$refresh()

# get a list of all my activities
my_acts <- get_activity_list(stoken)

# create an activity summary then only look at Runs
run_summary <- compile_activities(my_acts, units = "imperial") %>%
  filter(type == "Run") 

