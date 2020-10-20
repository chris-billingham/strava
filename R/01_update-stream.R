suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(rStrava)
  library(glue)
  library(here)
})

# load in strava functions i made
source(here("R/xx_strava-functions.R"))

# get the authetication token and refresh it
print(glue("01. refreshing auth token"))
stoken <- httr::config(token = readRDS(here(".httr-oauth"))[[1]])
stoken$auth_token$refresh()

# get a list of all my activities
print(glue("02. getting list of all activities"))
my_acts <- get_activity_list(stoken)

# create an activity summary then only look at Runs
print(glue("03. compiling activities"))
run_summary <- compile_activities(my_acts, units = "imperial") %>%
  filter(type == "Run") 

# read in current stream file
old_stream <- readRDS(here("data/all_stream.rds"))

# create the stream function
get_streams_df <- function(row, df) {
  # truncate the df
  new_df <- df[row,]
  
  # get the stream
  df_stream <- get_activity_streams(new_df, stoken)
 
  # return the stream
  return(df_stream)
}

# get rid of any we already have
run_small <- run_summary %>% 
  anti_join(old_stream, by = "id")

# check for how many rows, if more than 100 only get 100
if(nrow(run_small) > 100){
  rows <- 100
} else {
  rows <- nrow(run_small)
}

# create a DF with all data from all activity streams
if(rows > 0) {
  print(glue("04. getting {rows} activities worth of new data"))
  
  # set up columns we want
  cols <- c("id", "time", "moving", "cadence", "distance", "lat", "lng", "heartrate", 
            "velocity_smooth", "altitude", "grade_smooth")
  
  # get the stream, we have to be a bit faffier with the select and rename because for my older runs
  # i didn't have cadence or heartrate as my phone didn't track so we just need to cover ourselves for
  # missing columns
  all_stream <- map_dfr(seq(1, rows, 1), get_streams_df, df = run_small) %>%
    select(any_of(cols)) %>%
    rename_all(recode, id = "id", time = "time_s", moving = "moving", cadence = "cadence", distance = "distance_mi",
               lat = "lat", lng = "lng", heartrate = "heartrate_bpm", velocity_smooth = "velocity_smooth_mph",
               altitude = "altitude_ft", grade_smooth = "grade_smooth_pct")
  
  # bind the old with the new
  new_df <- bind_rows(old_stream, all_stream)
  
  # save to hdd
  print(glue("05. saving to hdd"))
  saveRDS(new_df, here("data/all_stream.rds"))
}
# fin

