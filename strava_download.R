# devtools::install_github("fawda123/rStrava")

library(jsonlite)
library(rStrava)
library(fossil)
library(tidyverse)
library(pbapply)
library(lubridate)
library(darksky)
library(janitor)

# setup accessing strave
app_name <- Sys.getenv("app_name") # chosen by Strava
app_client_id  <- Sys.getenv("app_client_id") # an integer, assigned by Strava
app_secret <- Sys.getenv("app_secret") # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))

# get a list of all my activities
my_acts <- get_activity_list(stoken)

# create an activity summary then only look at Runs
run_summary <- compile_activities(my_acts) %>%
  filter(type == "Run")


# get all the best efforts for every activity and add the start dates
# let's do it two ways
all_best <- pblapply(run_summary$id, tidy_best_efforts) %>% 
  bind_rows() %>%
  left_join(run_summary[,c("id","start_date")])

saveRDS(all_best, "data/best_efforts.rds")

# create a DF with all data from all activity streams
all_stream <- pblapply(run_summary$id, tidy_stream) %>%
  bind_rows()

