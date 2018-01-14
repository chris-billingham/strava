# process all the best efforts for a single activity
tidy_best_efforts <- function(id) {

  # get the activity details
df <- get_activity(id, stoken)
  
# ok how many best efforts do we have
efforts <- length(df$best_efforts)
  
# lets loop through all the efforts
for (number in 1:efforts) {

# pluck out the relevant data we want
name <- df$best_efforts[[number]]$name
distance <- df$best_efforts[[number]]$distance
elapsed_time <- df$best_efforts[[number]]$elapsed_time
moving_time <- df$best_efforts[[number]]$moving_time
start_index <- df$best_efforts[[number]]$start_index
end_index <- df$best_efforts[[number]]$end_index

# note pr_rank doesn't exist all the time hence this
pr_rank <- ifelse(is.numeric(df$best_efforts[[number]]$pr_rank),df$best_efforts[[number]]$pr_rank,0)

# merge into a tibble
effort <- tibble(id, name, distance, elapsed_time, moving_time, start_index, end_index, pr_rank)

# if we've got on add to a rolling list of efforts otherwise make it
if(exists("all_efforts")){
  all_efforts <- bind_rows(all_efforts, effort)
} else {
  all_efforts <- effort
}
}
return(all_efforts)
}

tidy_stream <- function(id) {
  # import the stream from strava
  stream <- get_streams(stoken, id, types = list('time','latlng','distance','altitude','velocity_smooth','heartrate','cadence','moving','grade_smooth'))
  
  # how many variables are we processing
  vars <- length(stream)
  
  # how many observations
  obs <- stream[[1]]$original_size
  
  # let's set up our stream_df
  stream_df <- as.tibble(rep(id, obs))
  colnames(stream_df) <- "id"
  
  # time to loop through all the variables
  for (lp in 1:vars) {
    
    # latlng is the only var that is wider than 1 dimension so we have to create a special bit
    if (stream[[lp]]$type == "latlng") {
    
      # in essence i flatten the data and then split through it in twos to two variables and retibble
      temp <- stream[[lp]]$data
      temp <- flatten(temp)
      l1 <- seq(1,length(temp)-1,2)
      l2 <- seq(2,length(temp),2)
      lat <- as.tibble(unlist(temp[l1]))
      lng <- as.tibble(unlist(temp[l2]))
      colnames(lat) <- "lat"
      colnames(lng) <- "lng"
      data <- bind_cols(lat, lng)
    } else {
      
      # if it's just the one variable it's a touch easier
      data <- as.tibble(unlist(stream[[lp]]$data))
      colnames(data) <- stream[[lp]]$type
    }
    
    # smash them all together
    stream_df <- bind_cols(stream_df, data)
  }
  
  # fin
  return(stream_df)
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
