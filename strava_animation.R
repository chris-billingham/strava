# select Rochdale Canal 10Ks
# horizontal is run number
# vertical is distance
# tick is clock

# need to work out when I turn back. probably using distance from start as it's an out and back.

library(tidyverse)
library(gganimate)
library(lubridate)
library(RColorBrewer)

rochdale_10k <- run_summary %>%
  filter(substr(name,1,8) == "Rochdale") %>%
  filter(distance > 10, distance < 12) %>%
  arrange(elapsed_time) %>%
  mutate(rank = row_number()) %>%
  select(id, rank, elapsed_time, start_date)

rochdale_stream <- all_stream %>%
  inner_join(rochdale_10k) %>%
  arrange(rank, time) 

starter <- rochdale_stream %>%
  filter(time == 0) %>%
  select(id, lat, lng)

ender <- rochdale_stream %>%
  group_by(id) %>% 
  filter(time == max(time)) %>%
  ungroup() %>%
  select(id, lat, lng)

colnames(starter) <- c("id", "start_lat", "start_lng")
colnames(ender) <- c("id", "end_lat", "end_lng")

rochdale_stream %>%
  inner_join(ender) %>%
  mutate(distance_start = 111.045 * sqrt((lat-end_lat)^2 + (lng-end_lng)^2)) %>%
  #filter(velocity_smooth > 2, velocity_smooth < 4) %>%
  ggplot(aes(start_date, distance_start, colour = velocity_smooth)) +
  geom_jitter(show.legend = FALSE) +
  scale_colour_gradient2() +
  labs(title = 'Time in seconds: {frame_time}', x = 'All 10Ks on Rochdale Canal', y = 'Distance Travelled from Start') +
  transition_time(time)

rochdale_stream %>%
  inner_join(starter) %>%
  mutate(distance_start = 111.045 * sqrt((lat-start_lat)^2 + (lng-start_lng)^2)) %>%
  #filter(velocity_smooth > 2, velocity_smooth < 4) %>%
  ggplot(aes(distance_start, altitude, colour = velocity_smooth)) +
  geom_jitter(show.legend = FALSE) +
  scale_colour_gradient2() +
  labs(title = 'All Rochdale Canal 10Ks - Time in seconds: {frame_time}', x = 'Distance Traveled from the Start', y = 'Elevation') +
  transition_time(time)

