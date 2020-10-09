# process all the best efforts for a tidy dataframe
tidy_best_efforts <- function(id) {

# get the activity details
df <- get_activity(id, stoken)

# pluck out the relevant data we want
all_efforts <- tibble(
  id = id,
  name = map_chr(df$best_efforts, "name"),
  distance = map_int(df$best_efforts, "distance"),
  elapsed_time = map_int(df$best_efforts, "elapsed_time"),
  moving_time = map_int(df$best_efforts, "moving_time"),
  start_index = map_int(df$best_efforts, "start_index"),
  end_index = map_int(df$best_efforts, "end_index"),
  pr_rank = map_int(df$best_efforts, "pr_rank", .null = NA_integer_)
)

# wait 9 secs to not hit the rate limit
Sys.sleep(9)

return(all_efforts)
}


tidy_weather <- function(id) {
  id_test <- id
  
  lat <- all_stream %>% 
    filter(id == id, row_number() == 1) %>% 
    select(lat) %>% 
    as.numeric()
  
  lng <- all_stream %>% 
    filter(id == id, row_number() == 1) %>% 
    select(lng) %>% 
    as.numeric()
  
  start_datetime <- run_summary %>% 
    filter(id == id_test) %>% 
    select(start_date) %>% 
    as.character() %>%
    as_datetime() %>%
    round_date("hour")
  
  # pull the forecast at the nearest point to the start of the run
  forecast <- get_forecast_for(lat, lng, start_datetime, units = "uk2")
  
  # filter it for the start and add in the id
  weather_start <- forecast$hourly %>% 
    filter(time == start_datetime) %>%
    mutate(id = id)
  
  return(weather_start)
  
}

pretty_pace <- function(pace, unit = "imperial") {
  if(!is.numeric(pace) && !is.integer(pace)) stop('Pace must be either a numeric or integer variable')
  mins <- floor(pace)
  seconds <- formatC((pace - mins) * 60, width = 2, format = "d", flag = "0")
  pretty_pace <- paste0(as.character(mins), ":", seconds, ifelse(unit == "imperial", " min/mi", " min/km"))
  return(pretty_pace)
}

pretty_time <- function(pace) {
  if(!is.numeric(pace) && !is.integer(pace)) stop('Pace must be either a numeric or integer variable')
  mins <- floor(pace)
  seconds <- formatC((pace - mins) * 60, width = 2, format = "d", flag = "0")
  pretty_pace <- paste0(as.character(mins), ":", seconds)
  return(pretty_pace)
}