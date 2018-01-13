# process all the best efforts for a single activity
best_efforts <- function(id) {

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


