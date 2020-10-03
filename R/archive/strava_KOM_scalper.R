# strava KOM scalper

# get some bounding box
# find nearest segments
# calculate effort to top
# calculate your max effort available
# rank by do-ability

library(tidyverse)
library(rStrava)

# create the authentication token
stoken <- httr::config(token = strava_oauth(Sys.getenv("strava_app_name"), 
                                            Sys.getenv("strava_app_client_id"), 
                                            Sys.getenv("strava_app_secret")))

# get all runs, remove non-training/long runs
my_acts <- get_activity_list(stoken)
run_summary <- compile_activities(my_acts, units = "imperial") %>%
  filter(type == "Run", workout_type == 0)
run_summary$start_date <- ymd_hms(run_summary$start_date)

# get the last 20 runs
last_20_runs <- run_summary %>% 
  top_n(20, start_date)

# set search box for bounding
bounding <- c("53.480824, -2.227870, 53.485549, -2.21390")

# get nearest segments and their ids
nearby_segments <- get_explore(stoken, bounding, activity_type = "running")
nearby_ids <- nearby_segments$segments %>% 
  map_int("id")

nearby_leaderboards <- nearby_ids %>% map_df(get_leaderboard(stoken, ., nleaders = 1))
