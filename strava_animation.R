# select Rochdale Canal 10Ks
# horizontal is run number
# vertical is distance
# tick is clock

# need to work out when I turn back. probably using distance from start as it's an out and back.

library(tidyverse)
library(gganimate)
library(lubridate)
library(RColorBrewer)
library(geosphere)

rochdale_10k_mini <- run_summary %>%
  filter(substr(name,1,8) == "Rochdale") %>%
  filter(distance > 6.15, distance < 6.5) %>%
  arrange(elapsed_time) %>%
  mutate(rank = row_number()) %>%
  select(id, rank, elapsed_time, start_date)

rochdale_streams <- rochdale_10k_streams %>%
  left_join(rochdale_10k_mini, by = "id") %>%
  mutate(start_date = ymd_hms(start_date))

starter <- rochdale_streams %>%
  filter(time == 0) %>%
  select(id, lat, lng)

ender <- rochdale_streams %>%
  group_by(id) %>% 
  filter(time == max(time)) %>%
  ungroup() %>%
  select(id, lat, lng)

colnames(starter) <- c("id", "start_lat", "start_lng")
colnames(ender) <- c("id", "end_lat", "end_lng")

rochdale_streams %>%
  inner_join(ender) %>%
  mutate(distance_end = pmap_dbl(., ~
                           distm(x = c(..17, ..16), y = c(..10, ..9), fun = distHaversine))/1000) %>%
  #filter(velocity_smooth > 2, velocity_smooth < 4) %>%
  ggplot(aes(start_date, distance_end, colour = velocity_smooth)) +
  geom_jitter(show.legend = FALSE) +
  scale_colour_gradient2() +
  labs(title = 'Time in seconds: {frame_time}', x = 'All 10Ks on Rochdale Canal', y = 'Distance Travelled from End') +
  transition_time(time)

rochdale_streams %>%
  inner_join(starter) %>%
  mutate(distance_end = pmap_dbl(., ~
                                   distm(x = c(..17, ..16), y = c(..10, ..9), fun = distHaversine))/1000) %>%
  #filter(velocity_smooth > 2, velocity_smooth < 4) %>%
  ggplot(aes(distance_end, altitude, colour = month(start_date))) +
  geom_jitter(show.legend = FALSE) +
  scale_colour_gradient2() +
  labs(title = 'All Rochdale Canal 10Ks - Time in seconds: {frame_time}', 
       x = 'Distance Traveled from the End', 
       y = 'Elevation') +
  transition_time(time)

