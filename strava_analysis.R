library(tidyverse)
library(magrittr)

all_best %>% 
  filter(!is.na(pr_rank)) %>% 
  ggplot(aes(start_date, elapsed_time, colour = pr_rank)) + 
    geom_point() + 
    facet_wrap( ~distance, scales = "free")



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
