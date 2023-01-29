strava_refresh_token <- function() {
# if we don't have an auth token, go get one, otherwise refresh what we have
if(!fs::file_exists(here::here(".httr-oauth"))){
  stoken <- httr::config(token = strava_oauth(app_name = Sys.getenv("strava_app_name"), 
                                              app_client_id = Sys.getenv("strava_app_client_id"), 
                                              app_secret = Sys.getenv("strava_app_secret"), 
                                              app_scope = "activity:read_all",
                                              cache = TRUE))
} else {
  stoken <- httr::config(token = readRDS(here::here(".httr-oauth"))[[1]])
  stoken$auth_token$refresh()
}
}

# process all the best efforts for a tidy dataframe
tidy_best_efforts <- function(id) {

# get the activity details
df <- rStrava::get_activity(id, stoken)

# pluck out the relevant data we want
all_efforts <- tibble::tibble(
  id = id,
  name = purrr::map_chr(df$best_efforts, "name"),
  distance = purrr::map_int(df$best_efforts, "distance"),
  elapsed_time = purrr::map_int(df$best_efforts, "elapsed_time"),
  moving_time = purrr::map_int(df$best_efforts, "moving_time"),
  start_index = purrr::map_int(df$best_efforts, "start_index"),
  end_index = purrr::map_int(df$best_efforts, "end_index"),
  pr_rank = purrr::map_int(df$best_efforts, "pr_rank", .null = NA_integer_)
)

if(nrow(all_efforts) == 0){
  all_efforts <- tibble::tibble(
    id = id,
    name = NA_character_,
    distanct = NA_integer_,
    elapsed_time = NA_integer_,
    moving_time = NA_integer_,
    start_index = NA_integer_,
    end_index = NA_integer_,
    pr_rank = NA_integer_
  )
}

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