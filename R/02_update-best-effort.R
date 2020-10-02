library(tidyverse)
library(lubridate)
library(pbapply)
library(rStrava)

# load in strava functions i made
source("~/usb/R/strava/R/xx_strava-functions.R")

# get the authetication token and refresh it
print(glue("01. refreshing auth token"))
stoken <- httr::config(token = readRDS('~/usb/R/strava/.httr-oauth')[[1]])
stoken$auth_token$refresh()

# get a list of all my activities
print(glue("02. getting list of all activities"))
my_acts <- get_activity_list(stoken)

# create an activity summary then only look at Runs
print(glue("03. compiling activities"))
run_summary <- compile_activities(my_acts, units = "imperial") %>%
  filter(type == "Run") 

# read in current stream file
old_best_efforts <- readRDS("data/all_best.rds")

# get rid of any we already have
run_small <- run_summary %>% 
  anti_join(old_best_efforts, by = "id")

# check for how many rows, if more than 100 only get 100
if(nrow(run_small) > 100){
  rows <- 100
} else {
  rows <- nrow(run_small)
}

# get the data
if(rows > 0){
  print(glue("04. getting {rows} of new data"))
  all_best <- map_dfr(run_small$id[1:rows], tidy_best_efforts) %>%
    left_join(run_summary[, c("id","start_date")]) %>%
    mutate(moving_mins = moving_time/60,
           elapsed_mins = elapsed_time/60,
           start_date = ymd_hms(start_date))
  
  # bind the old with the new
  new_df <- bind_rows(old_best, all_best)
  
  # save to hdd
  print(glue("05. saving to hdd"))
  saveRDS(new_df, "data/all_best.rds")
}
# fin