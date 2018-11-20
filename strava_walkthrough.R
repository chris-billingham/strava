library(tidyverse)
# devtools::install_github("fawda123/rStrava")
library(rStrava)
library(lubridate)
library(scales)

# in here put in how to get a Strava developers key

# create the authentication token, if we haven't cached then cache, otherwise use the cache
if(file.exists(".httr-oauth") == TRUE) {
  stoken <- httr::config(token = strava_oauth(Sys.getenv("strava_app_name"), 
                                              Sys.getenv("strava_app_client_id"), 
                                              Sys.getenv("strava_app_secret"), cache = TRUE))
} else {
  stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
}

# get a list of all my activities
my_acts <- get_activity_list(stoken)

# create an activity summary then only look at Runs, i am an old man and like imperial units
run_summary <- compile_activities(my_acts, units = "imperial") %>%
  filter(type == "Run") 

# interesting. however we only want runs that i did during my last work
# i am a man of routine so i have a set naming structure
rochdale_10k <- run_summary %>%
  filter(substr(name,1,8) == "Rochdale") %>%
  filter(distance > 6.15, distance < 6.5) %>%
  mutate(average_pace = 60 / average_speed,
         start_date = ymd_hms(start_date),
         start_date_local = ymd_hms(start_date_local))

# let's look at how that's distributed over time
rochdale_10k %>% 
  group_by(month = floor_date(start_date_local, "month")) %>%
  summarise(freq = n(), average_pace = mean(average_pace)) %>%
  ggplot(aes(month, freq)) +
    geom_col() +
    scale_x_datetime(date_breaks = "1 months", labels = date_format("%b %Y")) +
    theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) +
    labs(title = "10Ks per month over time at N Brown",
         y = "Frequency per month",
         x = "Month and Year")

# hey October 17 was good! It is all very up and down, there's no real consistency
# let's look at pace, here higher is slower. first i need to create a function that makes pace pretty
pretty_pace <- function(pace, unit = "imperial") {
  if(!is.numeric(pace) && !is.integer(pace)) stop('Pace must be either a numeric or integer variable')
  mins <- floor(pace)
  seconds <- formatC((pace - mins) * 60, width = 2, format = "d", flag = "0")
  pretty_pace <- paste0(as.character(mins), ":", seconds, ifelse(unit == "imperial", " min/mi", " min/km"))
  return(pretty_pace)
}

# right let's look at pace per month
rochdale_10k %>% 
  group_by(month = floor_date(start_date_local, "month")) %>%
  summarise(freq = n(), average_pace = mean(average_pace)) %>%
  ggplot(aes(month, average_pace)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 months", labels = date_format("%b %Y")) +
  scale_y_continuous(labels = function(x) pretty_pace(x), limits = c(7.5, 9.5), breaks = seq(7, 10, 0.25)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) +
  labs(title = "Average 10K pace per month over time at N Brown",
       y = "Pace in Minutes per Mile",
       x = "Month and Year")

# let's get some more detail on that
rochdale_10k %>%
  ggplot(aes(start_date_local, average_pace)) +
  geom_point() +
  geom_smooth(span = 0.25) +
  scale_x_datetime(date_breaks = "1 months", labels = date_format("%b %Y")) +
  scale_y_continuous(labels = function(x) pretty_pace(x), limits = c(7, 9.5), breaks = seq(7, 10, 0.25)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) +
  labs(title = "All 10Ks Pace over time at N Brown",
       y = "Month and Year",
       x = "Pace")

# 2017 was a good year for super quick 10ks. Never quite broke 7:30 mi/miles though
# you can see if the smoothed pace how cyclic my running was

# time to get more into the detail
# i want the stream data for this, however by using dplyr filters I lose the actframe class
# from my subset so I have to add that back in
rochdale_10k <- structure(.Data = rochdale_10k, class = c("actframe", "data.frame"))

# now we should be able to pass this to get_activity_streams to get all the well activity streams
rochdale_10k_streams <- get_activity_streams(rochdale_10k, stoken)

# velocity seems to be in mph but I'd rather know pace, miles per min
rochdale_10k_streams <- rochdale_10k_streams %>%
  mutate(pace_smooth = 60 / velocity_smooth)

### use patchwork to put these two charts side by side

# heart rate through the course of a run
strrochdale_10k_streams %>%
  filter(moving == TRUE) %>%
  filter(pace_smooth < 15) %>%
  ggplot(aes(distance, heartrate)) +
    geom_point(alpha = 0.01) +
    geom_smooth(span = 0.2, colour = "green")

# what has my pace looked like generally over the course of a run
rochdale_10k_streams %>%
  filter(moving == TRUE) %>%
  filter(pace_smooth < 15) %>%
  ggplot(aes(distance, pace_smooth)) +
    geom_point(alpha = 0.01) + 
    geom_smooth() +
    scale_y_continuous(labels = function(x) pretty_pace(x), limits = c(5, 15), breaks = seq(5, 15, 1))


# so i tend to set off a bit fast, slow down to steady, then speed up over time to the end. maybe with a lull
# just before the last dash, but then the last dash is legging it. interesting






%>%
  arrange(elapsed_time) %>%
  mutate(rank = row_number()) %>%
  select(id, rank, elapsed_time, start_date)


rochdale_10k_stream <- get_activity_streams(run_summary, stoken)
