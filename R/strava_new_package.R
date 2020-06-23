# devtools::install_github("marcusvolz/strava")
# devtools::install_github("marcusvolz/ggart")
# devtools::install_github("AtherEnergy/ggTimeSeries")

library(tidyverse)
library(strava)
library(ggart)
library(ggthemes)
library(ggTimeSeries)
library(lubridate)
library(viridis)

data <- process_data("activities/")

p1 <- plot_facets(data)

ggsave("plots/facets001.png", p1 , width = 20, height = 20, units = "cm")

p2 <- plot_map(data, lon_min = -2.5 , lon_max = -1.5, lat_min = 53, lat_max = 54)
ggsave("plots/map001.png", p2, width = 20, height = 15, units = "cm", dpi = 600)

p3 <- plot_elevations(data)
ggsave("plots/elevations001.png", p3, width = 20, height = 20, units = "cm")

summary <- data %>%
  mutate(time = as.Date(data$time),
         year = strftime(data$time, format = "%Y"),
         date_without_month = strftime(data$time, format = "%j"),
         month = strftime(data$time, format = "%m"),
         day_of_month = strftime(data$time, format = "%d"),
         year_month = strftime(data$time, format = "%Y-%m")) %>%
  group_by(time, year, date_without_month, month, day_of_month, year_month) %>%
  summarise(total_dist = sum(dist_to_prev), total_time = sum(time_diff_to_prev)) %>%
  mutate(speed = (total_dist) / (total_time /60^2)) %>%
  mutate(pace = (total_time / 60) / (total_dist)) %>%
  mutate(type = "day") %>%
  ungroup %>%
  mutate(id = as.numeric(row.names(.)))

# Generate plot data
time_min <- "2011-12-31"
time_max <- today()
max_dist <- 30

daily_data <- summary %>%
  group_by(time) %>%
  summarise(dist = sum(total_dist)) %>%
  ungroup() %>%
  mutate(time = as.Date(time)) %>%
  filter(complete.cases(.), time > time_min, time < time_max) %>%
  mutate(dist_scaled = ifelse(dist > max_dist, max_dist, dist))

# Create plot
p <- ggplot_calendar_heatmap(daily_data, "time", "dist_scaled",
                             dayBorderSize = 0.5, dayBorderColour = "white",
                             monthBorderSize = 0.75, monthBorderColour = "transparent",
                             monthBorderLineEnd = "round") +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_continuous(name = "km", low = "#DAE580", high = "#236327", na.value = "#EFEDE0") +
  facet_wrap(~Year, ncol = 1) +
  theme_tufte() +
  theme(strip.text = element_text(), axis.ticks = element_blank(), legend.position = "bottom")

# Save plot
ggsave("plots/calendar001.png", p, width = 30, height = 30, units = "cm", dpi = 300)