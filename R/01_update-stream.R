# load in strava functions i made
source(here::here("R/xx_strava-functions.R"))

# get the authentication token and refresh it
logger::log_info("01. refreshing auth token")
strava_refresh_token()

# get a list of all my activities
logger::log_info("02. getting list of all activities")
my_acts <- rStrava::get_activity_list(stoken)

# create an activity summary then only look at Runs
logger::log_info("03. compiling activities")
run_summary <- rStrava::compile_activities(my_acts, units = "imperial") |>
  # only get runs
  dplyr::filter(type == "Run") |>
  # only get runs with speed (where it's 0 means treadmill)
  dplyr::filter(max_speed > 0)

# read in current stream file
old_stream <- arrow::read_parquet(here::here("data/all_stream.parquet"))

# create the stream function
get_streams_df <- function(row, df) {
  # truncate the df
  new_df <- df[row,]
  
  # get the stream
  df_stream <- rStrava::get_activity_streams(new_df, stoken)
 
  # return the stream
  return(df_stream)
}

# get rid of any we already have
run_small <- run_summary |>
  dplyr::anti_join(old_stream, by = "id")

# check for how many rows, if more than 100 only get 100
if(nrow(run_small) > 100){
  rows <- 100
} else {
  rows <- nrow(run_small)
}

logger::log_info(glue::glue("04. getting {rows} activities worth of new data"))
# create a DF with all data from all activity streams
if(rows > 0) {

  # set up columns we want
  cols <- c("id", "time", "moving", "cadence", "distance", "lat", "lng", "heartrate", 
            "velocity_smooth", "altitude", "grade_smooth")
  
  # get the stream, we have to be a bit faffier with the select and rename because for my older runs
  # i didn't have cadence or heartrate as my phone didn't track so we just need to cover ourselves for
  # missing columns
  all_stream <- purrr::map_dfr(seq(1, rows, 1), get_streams_df, df = run_small, .progress = TRUE) |>
    dplyr::select(dplyr::any_of(cols)) |>
    dplyr::rename_all(dplyr::recode, id = "id", time = "time_s", moving = "moving", cadence = "cadence", distance = "distance_mi",
               lat = "lat", lng = "lng", heartrate = "heartrate_bpm", velocity_smooth = "velocity_smooth_mph",
               altitude = "altitude_ft", grade_smooth = "grade_smooth_pct")
  
  # bind the old with the new
  new_df <- dplyr::bind_rows(old_stream, all_stream)
  
  # save to hdd
  logger::log_info("05. saving to hdd")
  arrow::write_parquet(new_df, here::here("data/all_stream.parquet"))
}
# fin

