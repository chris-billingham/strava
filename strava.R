# library(devtools)
# install_github("fawda123/rStrava")

library(RJSONIO)
library(rStrava)
library(plyr)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(rlist)
library(tcltk)


app_name <- 'R_Interaction' # chosen by user
app_client_id  <- '14839' # an integer, assigned by Strava
app_secret <- 'e73a3f650bbe1ea6a0b0ab9c40f983384d309b63' # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))

# cat("google_key=AIzaSyDzUP0P47ODFAAdsHZJXqp9EwodO1TemKQ\n",    file=file.path(normalizePath("~/"), ".Renviron"),    append=TRUE)

# retrieve the key, restart R if not found

my_acts <- get_activity_list(stoken)

# create an activity summary

act_summary <- compile_activities(my_acts)

act_df <- as.data.frame(as.numeric(act_summary[,c("id")]))
colnames(act_df) <- c("activity_id")
total_acts <- length(my_acts)

# best efforts sort

all_act <- nrow(act_df)

pb <- tkProgressBar(title = "progress bar", min = 0, max = all_act, width = 300)

all_best <- lapply(act_df[,1],process_best_efforts)

all_best_df <- rbind.fill(all_best)

close(pb)

# create a DF with all data from all activity streams

pb <- tkProgressBar(title = "progress bar", min = 0, max = all_act, width = 300)

all_list <- lapply(act_df[,1],read_stream)

all_stream_df <- rbind.fill(all_list)

close(pb)

# Now to cross reference the best efforts with locations
# and look for commonalities
