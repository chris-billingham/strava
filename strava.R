library(devtools)
install_github("fawda123/rStrava")

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

abe$elapsed_time <- as.numeric(abe$elapsed_time)
abe$moving_time <- as.numeric(abe$moving_time)
abe$start_date <- ymd_hms(abe$start_date)
abe$start_date_local <- ymd_hms(abe$start_date_local)
abe$distance <- as.numeric(abe$distance)
abe$start_index <- as.numeric(abe$start_index)
abe$end_index <- as.numeric(abe$end_index)
abe$year_mon <- as.yearmon(abe$start_date)

graphplot <- ggplot(abe, aes(distance_fact, moving_mins))
graphplot + geom_point(color = abe$year_mon)



