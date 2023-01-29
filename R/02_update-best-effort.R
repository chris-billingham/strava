suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(rStrava)
  library(glue)
  library(here)
})

# load in strava functions i made
source(here::here("R/xx_strava-functions.R"))

logger::log_info("01. refreshing auth token")
# if we don't have an auth token, go get one, otherwise refresh what we have
if(!fs::file_exists(here::here(".httr-oauth"))){
  stoken <- httr::config(token = strava_oauth(app_name = Sys.getenv("strava_app_name"), 
                                              app_client_id = Sys.getenv("strava_app_client_id"), 
                                              app_secret = Sys.getenv("strava_app_secret"), 
                                              app_scope = "activity:read_all",
                                              cache = TRUE))
} else {
  stoken <- httr::config(token = readRDS(here::here(".httr-oauth"))[[1]])
  stoken$auth_token$refresh()
}

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
old_best <- arrow::read_parquet(here::here("data/all_best.parquet"))

# get rid of any we already have
run_small <- run_summary |> 
  dplyr::anti_join(old_best, by = "id")

# check for how many rows, if more than 100 only get 100
if(nrow(run_small) > 100){
  rows <- 100
} else {
  rows <- nrow(run_small)
}

logger::log_info(glue::glue("04. getting {rows} activities worth of new data"))
# get the data
if(rows > 0){
  all_best <- purrr::map_dfr(run_small$id[1:rows], tidy_best_efforts, .progress = TRUE) |>
    dplyr::left_join(run_summary[, c("id","start_date")], by = "id") |>
    dplyr::mutate(moving_mins = moving_time/60,
                  elapsed_mins = elapsed_time/60,
                  start_date = lubridate::ymd_hms(start_date))
  
  # bind the old with the new
  new_df <- dplyr::bind_rows(old_best, all_best)
  
  # save to hdd
  logger::log_info("05. saving to hdd")
  arrow::write_parquet(new_df, here::here("data/all_best.parquet"))
}
# fin