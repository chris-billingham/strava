library(tidyverse)
# devtools::install_github("fawda123/rStrava")
library(rStrava)
library(lubridate)
library(scales)
library(corrplot)

source("strava_functions.R")

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
  filter(str_detect(name, "Rochdale")) %>%
  filter(distance > 6.15, distance < 6.5) %>%
  mutate(average_pace = 60 / average_speed,
         start_date = ymd_hms(start_date),
         start_date_local = ymd_hms(start_date_local))

# let's pull out the times of all these runs and visualise it
rochdale_10k %>%
  mutate(time_of_run = hour(start_date_local) + minute(start_date_local)/60) %>%
  ggplot(aes(time_of_run)) +
  geom_histogram(bins = 36) +
  scale_x_continuous(labels = function(x) pretty_time(x), limits = c(11, 15), breaks = seq(11, 15, 0.5)) +
  labs(title = "10Ks by start time whilst at N Brown",
       y = "Frequency",
       x = "Time of start of run")

# let's see if i like any days of the week
rochdale_10k %>%
  mutate(day_of_run = wday(start_date_local, label = TRUE)) %>%
  ggplot(aes(day_of_run)) + geom_histogram(stat = "count") +
  labs(title = "10Ks by day of the week whilst at N Brown",
       y = "Frequency",
       x = "Day of Run")

# combine the two. wow I like to run on a Monday just before 13:30...
rochdale_10k %>%
  mutate(day_of_run = wday(start_date_local, label = TRUE),
         time_of_run = hour(start_date_local) + minute(start_date_local)/60) %>%
  ggplot(aes(time_of_run)) +
  geom_histogram(bins = 12) +
  facet_wrap(~day_of_run) +
  scale_x_continuous(labels = function(x) pretty_time(x), limits = c(12.5, 14.5), breaks = seq(12.5, 14.5, 0.5)) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  labs(title = "10Ks by start time whilst at N Brown by day of the week",
       y = "Frequency",
       x = "Time of start of run")

# let's look at how that's distributed over time
rochdale_10k %>% 
  group_by(month = floor_date(start_date_local, "month")) %>%
  summarise(freq = n(), average_pace = mean(average_pace)) %>%
  ggplot(aes(month, freq)) +
    geom_col() +
    scale_x_datetime(date_breaks = "1 months", labels = date_format("%b %Y")) +
    scale_y_continuous(breaks = seq(0, 16, 2)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0)) +
    labs(title = "10Ks per month over time at N Brown",
         y = "Frequency per month",
         x = "Month and Year")

# hey October 17 was good! It is all very up and down, there's no real consistency
# let's look at pace, here higher is slower. first i need to create a function that makes pace pretty

# let's get some more detail on that
rochdale_10k %>%
  ggplot(aes(start_date_local, average_pace)) +
  geom_point() +
  geom_smooth(span = 0.25) +
  scale_x_datetime(date_breaks = "1 months", labels = date_format("%b %Y")) +
  scale_y_continuous(labels = function(x) pretty_pace(x), 
                     limits = c(7, 9.5), 
                     breaks = seq(7, 10, 0.25)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0)) +
  labs(title = "All 10Ks Pace over time at N Brown",
       y = "Pace",
       x = "Month and Year")

# 2017 was a good year for super quick 10ks. Never quite broke 7:30 mi/miles though

# so pace seems a bit all over. let's see if we can find some correlation between run volume and pace

# lets sort out some variables
run_summary$start_date <- lubridate::ymd_hms(run_summary$start_date)
run_summary$start_date_day <- as.Date(run_summary$start_date)
start_date <- min(run_summary$start_date_day)

# convert time to minutes
run_summary$elapsed_time <- run_summary$elapsed_time/60



# create a dataframe of just dates we use to join on
date_frame <- tibble(date = seq.Date(from = start_date,
                                     to = max(as.Date(rochdale_10k$start_date)),
                                     by = "days"))

# left join from run summary the data we need, replace NA with 0, and do a bunch of rolling sums
analysis <- date_frame %>%
  dplyr::left_join(run_summary[,c("start_date_day", "distance", "elapsed_time", "total_elevation_gain", "suffer_score")], 
                   by = c("date" = "start_date_day")) %>%
  dplyr::mutate_if(is.numeric, coalesce, 0) %>%
  dplyr::mutate(suffer_score = as.numeric(suffer_score)) %>%
  dplyr::mutate(run_flag = dplyr::case_when(distance == 0 ~ 0, TRUE ~ 1),
                pace = elapsed_time/distance) %>%
  dplyr::mutate(distance_roll_7 = zoo::rollapplyr(distance, width = 7, FUN = sum, partial = TRUE),
                distance_roll_14 = zoo::rollapplyr(distance, width = 14, FUN = sum, partial = TRUE),
                distance_roll_28 = zoo::rollapplyr(distance, width = 28, FUN = sum, partial = TRUE),
                distance_roll_365 = zoo::rollapplyr(distance, width = 365, FUN = sum, partial = TRUE),
                elapsed_time_roll_7 = zoo::rollapplyr(elapsed_time, width = 7, FUN = sum, partial = TRUE),
                elapsed_time_roll_14 = zoo::rollapplyr(elapsed_time, width = 14, FUN = sum, partial = TRUE),
                elapsed_time_roll_28 = zoo::rollapplyr(elapsed_time, width = 28, FUN = sum, partial = TRUE),
                elapsed_time_roll_365 = zoo::rollapplyr(elapsed_time, width = 365, FUN = sum, partial = TRUE),   
                total_elevation_gain_roll_7 = zoo::rollapplyr(total_elevation_gain, width = 7, FUN = sum, partial = TRUE),
                total_elevation_gain_roll_14 = zoo::rollapplyr(total_elevation_gain, width = 14, FUN = sum, partial = TRUE),
                total_elevation_gain_roll_28 = zoo::rollapplyr(total_elevation_gain, width = 28, FUN = sum, partial = TRUE),
                total_elevation_gain_roll_365 = zoo::rollapplyr(total_elevation_gain, width = 365, FUN = sum, partial = TRUE),
                run_count_roll_7 = zoo::rollapplyr(run_flag, width = 7, FUN = sum, partial = TRUE),
                run_count_roll_14 = zoo::rollapplyr(run_flag, width = 14, FUN = sum, partial = TRUE),
                run_count_roll_28 = zoo::rollapplyr(run_flag, width = 28, FUN = sum, partial = TRUE),
                run_count_roll_365 = zoo::rollapplyr(run_flag, width = 365, FUN = sum, partial = TRUE),
                suffer_score_roll_7 = zoo::rollapplyr(suffer_score, width = 7, FUN = mean, na.rm = TRUE, partial = TRUE),
                suffer_score_roll_14 = zoo::rollapplyr(suffer_score, width = 14, FUN = mean, na.rm = TRUE, partial = TRUE),
                suffer_score_roll_28 = zoo::rollapplyr(suffer_score, width = 28, FUN = mean, na.rm = TRUE, partial = TRUE),
                suffer_score_roll_365 = zoo::rollapplyr(suffer_score, width = 365, FUN = mean, na.rm = TRUE, partial = TRUE),
  )

# right let's filter the analysis down to just the time period of the 10ks
analysis_10k <- analysis %>%
  filter(date >= as.Date(min(rochdale_10k$start_date))) %>%
  filter(date <= as.Date(max(rochdale_10k$start_date))) %>%
  mutate(date = as.POSIXct(date))

analysis_10k_long <- analysis_10k %>%
  gather("variable", "value", 7:22) %>%
  select(date, variable, value)

# right let's see what we've got
analysis_10k_long %>%
  filter(variable %in% c("run_count_roll_28", "distance_roll_28")) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(aes(colour = variable)) +
  scale_x_datetime(date_breaks = "1 months", labels = date_format("%b %Y")) +
#  scale_y_continuous(limits = c(0, 14), 
#                     breaks = seq(0, 14, 2)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0),
          ) +
  labs(title = "Rolling Run and Distance Counts for 28 Days",
       y = "Rolling Count",
       x = "Month and Year")


# to run the correlation however i want to lag everything by a day, aka this is what i was before
# this day completed
roll_lag <- analysis_10k %>%
  select(8:27) %>%
  mutate_all(funs(lag))

rochdale_dates <- rochdale_10k %>%
  select(start_date) %>%
  mutate(start_date_day = as.Date(start_date))

analysis_10k_lag <- analysis_10k %>%
  select(1:7) %>%
  bind_cols(roll_lag) %>%
  tail(nrow(.)-1) %>%
  mutate(date = as.Date(date)) %>%
  semi_join(rochdale_dates, by = c("date" = "start_date_day"))

# let's have a look at correlations
corr <- cor(analysis_10k_lag[,7:27])

# visualise
corrplot(corr, method = "circle", type = "upper")
, order = "hclust")

# as elapsed_time seems a proxy for distance i'll take that out
# and i'll lost the 365 day rollers which i don't really believe
# and as this is exactly the same run each time we can ditch distance, run_flag
# elevation_gain and date

data_matrix <- analysis_10k_lag %>%
  select(-ends_with("365")) %>%
  select(-starts_with("elapsed_time")) %>%
  select(-c(1,2,3,4,5))

library(caret)
set.seed(1979)
train_index <- createDataPartition(data_matrix$pace, 
                                   p = .8,
                                   list = FALSE,
                                   times = 1)
train <- data_matrix[train_index,]
test <- data_matrix[-train_index,]

model <- lm(pace ~ ., 
            train)

summary(model)

pred_pace <- predict(model, test[,2:10])

test$pred_pace <- pred_pace
test$abs_error <- abs(test$pred_pace - test$pace)

ggplot(test, aes(x = pace, y = pred_pace)) + 
  geom_point() +
  scale_y_continuous(limits = c(7.6, 9.4),
                     breaks = seq(7.6, 9.4, 0.2)) +
  scale_x_continuous(limits = c(7.6, 9.4),
                     breaks = seq(7.6, 9.4, 0.2)) +
  labs(title = "Pace versus Predicted Pace of Test Set",
       y = "Predicted Pace in min/mile (decimalised)",
       x = "True Pace in min/mile (decimalised)")



# these are crap models
hi_2 <- analysis_10k[,c(1,24,25,26)]
hi_2_g %>% ggplot(aes(date, value, colour = variable)) + geom_line() +
  scale_x_datetime(date_breaks = "1 months", labels = date_format("%b %Y")) +
  scale_y_continuous(limits = c(0, 200), 
                     breaks = seq(0, 200, 50)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0)) +
  labs(title = "Mean Relative Effort over 7, 14 and 28 day rollers",
       y = "Mean Relative Effort",
       x = "Month and Year")




# time to get more into the detail
# i want the stream data for this, however by using dplyr filters I lose the actframe class
# now we should be able to pass this to get_activity_streams to get all the well activity streams
rochdale_10k_streams <- get_activity_streams(rochdale_10k, stoken)

# velocity seems to be in mph but I'd rather know pace, miles per min
rochdale_10k_streams <- rochdale_10k_streams %>%
  mutate(pace_smooth = 60 / velocity_smooth)

### use patchwork to put these two charts side by side

# heart rate through the course of a run
rochdale_10k_streams %>%
  filter(moving == TRUE) %>%
  filter(pace_smooth < 15) %>%
  ggplot(aes(distance, heartrate)) +
    geom_point(alpha = 0.01) +
    geom_smooth(span = 0.2, colour = "green") +
    geom_hline(yintercept = 183, linetype = "dashed", colour = "red") +
  annotate("text", label = "Max HR in BPM", x = 0.5, y = 188, size = 4, colour = "red") +
  scale_x_continuous(breaks = seq(0, 6, 1)) +
  labs(title = "Heart Rate through the course of all N Brown 10Ks",
       y = "HR in BPM",
       x = "Distance in Miles")

# what has my pace looked like generally over the course of a run
rochdale_10k_streams %>%
  filter(moving == TRUE) %>%
  filter(pace_smooth < 15) %>%
  ggplot(aes(distance, pace_smooth)) +
    geom_point(alpha = 0.01) + 
#    geom_smooth(span = 0.1, colour = "blue") +
    scale_y_continuous(labels = function(x) pretty_pace(x), limits = c(5, 12), breaks = seq(5, 12, 1)) +
  labs(title = "Pace through the course of all N Brown 10Ks",
       y = "Pace in Mins/Mile",
       x = "Distance in Miles")  


17# so i tend to set off a bit fast, slow down to steady, then speed up over time to the end. maybe with a lull
# just before the last dash, but then the last dash is legging it. interesting

library(tidytext)

words_10k <- rochdale_10k %>%
  select(start_date, id, average_pace, name) %>%
  mutate(start_date = as.Date(start_date)) %>%
  mutate(text_scrub = gsub("Rochdale Canal - 10[Kk] - ", "", name)) %>%
  mutate(text_scrub = gsub("T-[0-9]* Days", "", text_scrub)) %>%
  unnest_tokens(word, text_scrub) %>%
  anti_join(stop_words)

sent_10k <- words_10k %>%
  inner_join(get_sentiments("afinn"))

sent_10k %>%
  group_by(month = floor_date(start_date, "month")) %>%
  summarise(overall_sentiment = sum(score)) %>%
  ggplot(aes(month, overall_sentiment)) +
    geom_col() +
  scale_x_date(date_breaks = "1 months", labels = date_format("%b %Y")) +
  scale_y_continuous(breaks = seq(-15, 10, 5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0)) +
  labs(title = "Total Monthly Sentiment of 10K 'Free Text'",
       y = "Total Monthly Sentiment",
       x = "Month and Year")

sent_10k %>% 
  group_by(id, average_pace) %>% 
  summarise(net_score = sum(score)) %>% 
  ggplot(aes(as.factor(net_score), average_pace)) + 
  geom_violin() +
  scale_y_continuous(breaks = seq(7.5, 9.5, .25),
                     labels = function(x) pretty_pace(x)) +
  labs(title = "Pace Distribution by Sentiment",
       y = "Pace of Run",
       x = "Total Sentiment Score")

sent_10k %>%
  filter(start_date >= as.Date("2017-08-01")) %>%
  filter(start_date <= as.Date("2017-11-30")) %>%
  group_by(word) %>%
  summarise(net_score = sum(score)) %>%
  arrange(desc(net_score)) %>%
  head(10)
