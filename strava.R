library(devtools)
install_github("fawda123/rStrava")

library(RJSONIO)
library(rStrava)
library(plyr)
library(ggplot2)
library(zoo)

app_name <- 'R_Interaction' # chosen by user
app_client_id  <- '14839' # an integer, assigned by Strava
app_secret <- 'e73a3f650bbe1ea6a0b0ab9c40f983384d309b63' # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))

cat("google_key=AIzaSyDzUP0P47ODFAAdsHZJXqp9EwodO1TemKQ\n",
                                           file=file.path(normalizePath("~/"), ".Renviron"),
                                           append=TRUE)

# retrieve the key, restart R if not found

my_acts <- get_activity_list(stoken)

total_acts <- length(my_acts)

activity_list <- as.data.frame(my_acts[[1]]["id"])

# best efforts sort

for (i in 2:total_acts) {
  activity_list <- cbind(activity_list, as.data.frame(my_acts[[i]]["id"]))
}

activity_list <- as.data.frame(t(activity_list),stringsAsFactors = FALSE)

row.names(activity_list) <- NULL
activity_list$V1 <- as.numeric(activity_list$V1)

for (p in 1:total_acts) {

  current_activity <- activity_list[p,1]  

latest <- get_activity(current_activity, stoken)
if(latest[["type"]] == "Ride" || latest[["type"]] == "Hike")
{
  next
}

best_length <- length(latest[["best_efforts"]])

best_e <- as.data.frame(unlist(latest[["best_efforts"]][1]))
best_e <- as.data.frame(t(best_e), stringsAsFactors = FALSE)


for (i in 2:length(latest[["best_efforts"]]))
  {
  best_f <- as.data.frame(t(unlist(latest[["best_efforts"]][i])), stringsAsFactors = FALSE)
  best_e <- rbind.fill(best_e, best_f)
  }

row.names(best_e) <- NULL
best_e$moving_time <- as.numeric(best_e$moving_time)
best_e$moving_mins <- best_e$moving_time / 60

if(exists("all_best_efforts")) {
  all_best_efforts <- rbind.fill(all_best_efforts,best_e)
} else {
    all_best_efforts <- best_e
}

}

abe <- all_best_efforts

abe$elapsed_time <- as.numeric(abe$elapsed_time)
abe$moving_time <- as.numeric(abe$moving_time)
abe$start_date <- ymd_hms(abe$start_date)
abe$start_date_local <- ymd_hms(abe$start_date_local)
abe$distance <- as.numeric(abe$distance)
abe$start_index <- as.numeric(abe$start_index)
abe$end_index <- as.numeric(abe$end_index)
abe$year_mon <- as.yearmon(abe$start_date)


# read a single activity and translate all the data

read_stream <- function(act_id)
{
import_l <- get_streams(stoken, act_id, types = list('time','latlng','distance','altitude','velocity_smooth','heartrate','cadence','moving','grade_smooth'))
export2 <- toJSON(import_l)
write(export2, "test2.json")
iy <- RJSONIO::fromJSON("test2.json")

# process latlng

latlng <- iy[[1]]['data'][1]
latlng_df <- t(as.data.frame(latlng))
rownames(latlng_df) <- NULL
colnames(latlng_df) <- c("lat","lng")

activity_id <- as.data.frame(rep(act_id,length(latlng_df)))
colnames(activity_id) <- c("activity_id")

time <- iy[[2]]['data']
time_df <- as.data.frame(time)
rownames(time_df) <- NULL
colnames(time_df) <- c("time")

distance <- iy[[3]]['data']
distance_df <- as.data.frame(distance)
rownames(distance_df) <- NULL
colnames(distance_df) <- c("distance")

altitude <- iy[[4]]['data']
altitude_df <- as.data.frame(altitude)
rownames(altitude_df) <- NULL
colnames(altitude_df) <- c("altitude")

hr <- iy[[5]]['data']
hr_df <- as.data.frame(hr)
rownames(hr_df) <- NULL
colnames(hr_df) <- c("hr")

cadence <- iy[[6]]['data']
cadence_df <- as.data.frame(cadence)
rownames(cadence_df) <- NULL
colnames(cadence_df) <- c("cadence")

grade <- iy[[7]]['data']
grade_df <- as.data.frame(grade)
rownames(grade_df) <- NULL
colnames(grade_df) <- c("grade")

moving <- iy[[8]]['data']
moving_df <- as.data.frame(moving)
rownames(moving_df) <- NULL
colnames(moving_df) <- c("moving")

speed <- iy[[9]]['data']
speed_df <- as.data.frame(speed)
rownames(speed_df) <- NULL
colnames(speed_df) <- c("speed")

activity_df <- cbind(activity_id, latlng_df, time_df, distance_df, altitude_df, hr_df, cadence_df, grade_df, moving_df, speed_df)

return(activity_df)
}

