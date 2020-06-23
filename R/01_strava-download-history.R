library(tidyverse)
library(lubridate)
library(pbapply)
library(rStrava)

# load in strava functions i made
source("/home/pi/hdd/R/xx_strava-functions.R")

# get the authetication token and refresh it
stoken <- httr::config(token = readRDS('/home/pi/hdd/R/strava/.httr-oauth')[[1]])
stoken$auth_token$refresh()

# get a list of all my activities
my_acts <- get_activity_list(stoken)

# create an activity summary then only look at Runs
run_summary <- compile_activities(my_acts, units = "imperial") %>%
  filter(type == "Run") 

pboptions(nout = 1000)
# get all the best efforts for every activity and add the start dates
# let's do it two ways
all_best <- pblapply(run_summary$id, tidy_best_efforts) %>% 
  bind_rows() %>%
  left_join(run_summary[, c("id","start_date")]) %>%
  mutate(moving_mins = moving_time/60,
         elapsed_mins = elapsed_time/60,
         start_date = ymd_hms(start_date))

saveRDS(all_best, "data/all_best.rds")

get_streams_df <- function(row, df) {
  print(row)
  new_df <- df[row,]
  df_stream <- get_activity_streams(new_df, stoken)
  # wait 9 secs to not hit the rate limit
  Sys.sleep(9)
  return(df_stream)
}


# create a DF with all data from all activity streams
all_stream <- map_dfr(seq(1, nrow(run_summary), 1), get_streams_df, df = run_summary) %>%
  select(id = id,
         time_s = time,
         moving = moving,
         cadence = cadence,
         distance_mi = distance,
         lat = lat,
         lng = lng,
         heartrate_bpm = heartrate,
         velocity_smooth_mph = velocity_smooth,
         altitude_ft = altitude,
         grade_smooth_pct = grade_smooth
         )

saveRDS(all_stream, "data/all_stream.rds")


