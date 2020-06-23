# devtools::install_github("fawda123/rStrava")
library(rStrava)
library(tidyverse)
library(lubridate)
library(magrittr)
library(dplyr)
library(zoo)

# create the authentication token
stoken <- httr::config(token = strava_oauth(Sys.getenv("strava_app_name"), 
                                            Sys.getenv("strava_app_client_id"), 
                                            Sys.getenv("strava_app_secret")))

# get a list of all my activities
my_acts <- rStrava::get_activity_list(stoken)

# create an activity summary then only look at Runs
run_summary <- rStrava::compile_activities(my_acts) %>%
  filter(type == "Run")

# lets sort out some variables
run_summary$start_date <- lubridate::ymd_hms(run_summary$start_date)
run_summary$start_date_day <- as.Date(run_summary$start_date)
start_date <- min(run_summary$start_date_day)

# convert time to minutes
run_summary$elapsed_time <- run_summary$elapsed_time/60

# convert distance to miles
run_summary$distance <- run_summary$distance/10*6.21

# create a dataframe of just dates we use to join on
date_frame <- tibble(date = seq.Date(from = start_date,
                          to = Sys.Date(),
                          by = "days"))

# left join from run summary the data we need, replace NA with 0, and do a bunch of rolling sums
analysis <- date_frame %>%
  dplyr::left_join(run_summary[,c("start_date_day", "distance", "elapsed_time", "total_elevation_gain")], 
            by = c("date" = "start_date_day")) %>%
  dplyr::mutate_if(is.numeric, coalesce, 0) %>%
  dplyr::mutate(run_flag = dplyr::case_when(distance == 0 ~ 0, TRUE ~ 1),
         pace = elapsed_time/distance) %>%
  dplyr::mutate(distance_roll_7 = zoo::rollapplyr(distance, width = 7, FUN = sum, partial = TRUE),
         distance_roll_14 = zoo::rollapplyr(distance, width = 14, FUN = sum, partial = TRUE),
         distance_roll_28 = zoo::rollapplyr(distance, width = 28, FUN = sum, partial = TRUE),
         distance_roll_365 = zoo::rollapplyr(distance, width = 365, FUN = sum, partial = TRUE),
         elapsed_time_roll_7 = zoo::rollapplyr(elapsed_time, width = 7, FUN = sum, partial = TRUE),
         elapsed_time_roll_14 = zoo::rollapplyr(elapsed_time, width = 14, FUN = sum, partial = TRUE),
         elapsed_time_roll_28 = zoo::rollapplyr(elapsed_time, width = 28, FUN = sum, partial = TRUE),
         elapsed_time_roll_365 = zoo::rollapplyr(elapsed_time, width = 365, FUN = sum, partial = TRUE),   
         total_elevation_gain_roll_7 = zoo::rollapplyr(total_elevation_gain, width = 7, FUN = sum, partial = TRUE),
         total_elevation_gain_roll_14 = zoo::rollapplyr(total_elevation_gain, width = 14, FUN = sum, partial = TRUE),
         total_elevation_gain_roll_28 = zoo::rollapplyr(total_elevation_gain, width = 28, FUN = sum, partial = TRUE),
         total_elevation_gain_roll_365 = zoo::rollapplyr(total_elevation_gain, width = 365, FUN = sum, partial = TRUE),
         run_count_roll_7 = zoo::rollapplyr(run_flag, width = 7, FUN = sum, partial = TRUE),
         run_count_roll_14 = zoo::rollapplyr(run_flag, width = 14, FUN = sum, partial = TRUE),
         run_count_roll_28 = zoo::rollapplyr(run_flag, width = 28, FUN = sum, partial = TRUE),
         run_count_roll_365 = zoo::rollapplyr(run_flag, width = 365, FUN = sum, partial = TRUE)        
         )

model <- analysis %>% 
  filter(run_flag == 1) %>%
  select(-date, -elapsed_time, -run_flag) %>%
  lm(data = ., pace ~ .)

summary(model)

rf <- analysis %>% 
  filter(run_flag == 1) %>%
  select(-date, -elapsed_time, -run_flag) %>%
  randomForest::randomForest(., pace ~ .)

summary(model_rf)