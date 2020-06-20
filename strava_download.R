library(tidyverse)
library(rStrava)

# get the authetication token and refresh it
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
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
  left_join(run_summary[, c("id","start_date")])

all_best$moving_mins <- all_best$moving_time/60
all_best$elapsed_mins <- all_best$elapsed_time/60
all_best$start_date <- ymd_hms(all_best$start_date)

all_best <- all_best %>% 
  group_by(distance) %>% 
  arrange(elapsed_mins) %>% 
  mutate(elapsed_rank = row_number()) %>% 
  arrange(moving_mins) %>% 
  mutate(moving_rank = row_number())

# create a DF with all data from all activity streams
all_stream <- pblapply(run_summary$id, tidy_stream) %>%
  bind_rows()

