# 
devtools::install_github("fawda123/rStrava")

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
# %>%  clean_names()

# get all the best efforts for every activity and add the start dates
# let's do it two ways
all_best <- pblapply(run_summary$id, tidy_best_efforts) %>% 
  bind_rows() %>%
  left_join(run_summary[,c("id","start_date")])

all_best <- map_df(run_summary$id, tidy_best_efforts) %>%
  left_join(run_summary[,c("id","start_date")])



# create a DF with all data from all activity streams
all_stream <- pblapply(run_summary$id, tidy_stream) %>%
  bind_rows()

# work out the bearing for each data point
run_bearing <- append(0,sapply(2:(nrow(all_stream)), function(i){
  earth.bear(all_stream$lng[i-1], all_stream$lat[i-1],
             all_stream$lng[i], all_stream$lat[i])}))

# add run_bearing into the all_stream

all_weather$windBearing_run <- ifelse(all_weather$windBearing>180,all_weather$windBearing - 180, all_weather$windBearing + 180)

test_stream$bearing_diff <- ((test_weather$windBearing_run[1] - test_stream$run_bearing)*pi)/180

test_stream$wind_boost <- cos(test_stream$bearing_diff) * test_weather$windSpeed


# some light visualisations
all_stream %>% 
  semi_join(run_summary[1:100,], by = c("id")) %>% 
  ggplot(aes(lng, lat)) + 
    geom_point(aes(size = 1)) + 
    facet_wrap(~id, scales = "free") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())

# let's combine strava with dark sky
# ok i've written that as a function
# let's get it all

all_weather <- pblapply(run_summary$id, tidy_weather) %>%
  bind_rows()

# some other stuff

# meters per second into miles per minute
all_stream %<>% mutate(pace = 26.8224 / velocity_smooth)

# just calculate it based on distance and time
run_summary %<>% mutate(pace = (elapsed_time / 60) / (distance*0.6214))




test <- postcodes %>% 
  mutate(post_outward = str_extract(postcodes$Postcode, "([^ ]+)")) %>%
  filter(`In Use?` == "Yes") %>%
  group_by(post_outward) %>%
  summarise(lat_av = mean(Latitude), lon_av = mean(Longitude))

bear <- function(i){
  earth.bear(test$lng[i-1], test$lat[i-1],
             test$lng[i], test$lat[i] )}

hest <- all_stream %>% group_by(id) %>% by_slice(bear(nrow(.)))
