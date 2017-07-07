# this function creates a list of all activities in a data frame
# however this is basically redundant as I can use a different function. Yay

get_activities <- function(list_name, list_length)
{
  for (i in 1:list_length) {
    if(exists("activity_list"))
    {
      activity_list <- cbind(activity_list, as.data.frame(list_name[[i]]["id"]))
    } else {
      activity_list <- as.data.frame(list_name[[1]]["id"])
    }
  }
  activity_list <- as.data.frame(t(activity_list),stringsAsFactors = FALSE)
  row.names(activity_list) <- NULL
  colnames(activity_list) <- c("activity_id")
  activity_list[,1] <- as.numeric(activity_list[,1])
  return(activity_list)
}

process_single_stream <- function(data_l)
{
  
  # this bit doesn't work correctly this is annoying
  # why won't it transpose the dataframe when i apply
  
  if(length(data_l[[1]][[1]])==2)
  {
    df <- as.data.frame(t(as.data.frame(data_l)))
    #rownames(df) <- NULL
    colnames(df) <- NULL
  } else
  {
    df <- as.data.frame(data_l)
    colnames(df) <- NULL
    
  }
  return(df)
}


read_stream <- function(act_id)
{
  import_l <- get_streams(stoken, act_id, types = list('time','latlng','distance','altitude','velocity_smooth','heartrate','cadence','moving','grade_smooth'))
  export2 <- toJSON(import_l, digits = 10)
  write(export2, "test2.json")
  iy <- RJSONIO::fromJSON("test2.json")
  
  # process the stream
  
  # print(act_id)
 
  curr_act <- which(act_df$activity_id == act_id)
  
  
  # dummy the activity_id
  act_size <- length(iy[[1]][[2]])
  activity_id <- as.data.frame(rep(act_id,act_size))
  colnames(activity_id) <- c("activity_id")
  
  # set up
  
  vars <- list.map(iy, type)
  data <- list.map(iy, data)
  
  vars <- unlist(vars)
  
  # latlng
  
  if(vars[1]=="latlng")
  {
  latlng <- as.data.frame(t(as.data.frame(data[1])))
  rownames(latlng) <- NULL
  colnames(latlng) <- c("lat","lng")
  }
  # process the rest function
  
  test <- lapply(data[2:length(data)], process_single_stream)
  stream_df <- rbind.fill(as.data.frame(test))
  colnames(stream_df) <- vars[2:length(vars)]
  if(exists("latlng"))
  {
    stream_df <- cbind(activity_id,stream_df)
  } else {
      stream_df <- cbind(activity_id, latlng, stream_df)
  }
  setTkProgressBar(pb, curr_act, label=paste(round(curr_act/all_act*100, 0),"% done"))
  
  return(stream_df)
}

process_best_efforts <- function(current_act)
{
  
    latest <- get_activity(current_act, stoken)
    curr_act <- which(act_df$activity_id == current_act)
    best_length <- as.numeric(length(latest[["best_efforts"]]))
    if (best_length == 0) {
      abe <- data.frame(id = character(),
                        resource_state = character(),
                        name = character(),
                        activity.id = character(),
                        activity.resource_state = character(),
                        athelete.id = character(),
                        athelete.resource_state = character(),
                        elapsed_time = double(),
                        moving_time = double(),
                        start_date = character(),
                        start_date_local = character(),
                        distance = double(),
                        start_index = double(),
                        end_index = double(),
                        moving_mins = double(),
                        year_mon = double(),
                        stringsAsFactors = FALSE)
      
    } else {
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
    
    if(exists("all_best_efforts")) 
      {
      all_best_efforts <- rbind.fill(all_best_efforts,best_e)
      } else {
      all_best_efforts <- best_e
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
    }

    
  setTkProgressBar(pb, curr_act, label=paste(round(curr_act/all_act*100, 0),"% done"))
  return(abe)
}



