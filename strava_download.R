# devtools::install_github("fawda123/rStrava")


library(rStrava)
library(fossil)
library(tidyverse)
library(pbapply)
library(lubridate)
library(darksky)
library(janitor)
library(magrittr)

# create the authentication token
stoken <- httr::config(token = strava_oauth(Sys.getenv("strava_app_name"), 
                                            Sys.getenv("strava_app_client_id"), 
                                            Sys.getenv("strava_app_secret")))

# get a list of all my activities
my_acts <- get_activity_list(stoken)

# create an activity summary then only look at Runs
run_summary <- compile_activities(my_acts) %>%
  filter(type == "Run")
run_summary$start_date <- ymd_hms(run_summary$start_date)
  
# get all the best efforts for every activity and add the start dates
# let's do it two ways
all_best <- pblapply(run_summary$id, tidy_best_efforts) %>% 
  bind_rows() %>%
  left_join(run_summary[, c("id","start_date")])

all_best$start_date <- ymd_hms(all_best$start_date)
all_best$pr_rank <- as.ordered(all_best$pr_rank)

# create a DF with all data from all activity streams
all_stream <- pblapply(run_summary$id, tidy_stream) %>%
  bind_rows()

