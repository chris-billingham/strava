# devtools::install_github("fawda123/rStrava")

library(jsonlite)
library(rStrava)
library(tidyverse)
library(pbapply)

# setup accessing strave
app_name <- Sys.getenv("app_name") # chosen by usery
app_client_id  <- Sys.getenv("app_client_id") # an integer, assigned by Strava
app_secret <- Sys.getenv("app_secret") # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))

# get a list of all my activities
my_acts <- get_activity_list(stoken)

# create an activity summary then only look at Runs
act_summary <- compile_activities(my_acts)
act_summary %<>% filter(type == "Run")

# get all the best efforts for every activity and add the start dates
all_best <- pblapply(act_summary$id, best_efforts) %>% 
  bind_rows() %>%
  left_join(act_summary[,c("id","start_date")])



# create a DF with all data from all activity streams
all_list <- lapply(act_df[,1],read_stream)

all_stream_df <- rbind.fill(all_list)


# Now to cross reference the best efforts with locations
# and look for commonalities

seq(Sys.Date()-10, Sys.Date(), "1 day") %>% 
  map(~get_forecast_for(53.2519, -1.9375, units = "uk2", .x)) %>% 
  map_df("hourly") %>% 
  ggplot(aes(x=time, y=temperature)) +
  geom_line()

london_year <- seq(Sys.Date()-730, Sys.Date(), "1 day") %>% 
  map(~get_forecast_for(51.5011, -0.14128, units = "uk2", .x))

buxton_y_hours <- buxton_year %>%
  map_df("hourly") %>% 
  ggplot(aes(x=time, y=temperature)) +
  geom_line()

london_hours_2y <- london_year %>% map_df("hourly")

test <- postcodes %>% 
  mutate(post_outward = str_extract(postcodes$Postcode, "([^ ]+)")) %>%
  filter(`In Use?` == "Yes") %>%
  group_by(post_outward) %>%
  summarise(lat_av = mean(Latitude), lon_av = mean(Longitude))

