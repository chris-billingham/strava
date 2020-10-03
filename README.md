# Strava Analysis, Investigation and Modelling

I opened this repo back in May 2017 to start looking at the functionality with the `rStrava` package. You can install this package:

`devtools::install_github("fawda123/rStrava")`

At the moment this consists of a number of scripts, and archive of some really old and not very good scripts, all sat in the `R` directory.

**Automation**: I have an Airflow server that schedules the below scripts to run and get any new data direct from Strava to store locally.

-   `01_update-stream.R` This pulls the activity stream, so per activity every datapoint available. As my GPS watch is set to update every second, this has a new update every second of every activity. Slowish and prone to triggering the rate limit.

-   `02_update-best-effort.R` On your Strava profile it records what it believes are your fastest times that happened within a Strava activity. In the backend, these "fastest efforts" are actually calculated on a per activity basis. This script pulls that data through via the API.

**Investigation:**

-   `strava_running_model.R` This was the first go at the modelling, you can see how I was already excited by rolling averages. Here using a randomForest for the modelling, oh what callow youth.

-   `strava_walkthrough.R` This was put together to support a presentation I made in Jan 2019. The idea was to review all the runs I did whilst I worked at N Brown on Rochdale Canal, see what interesting things I could find. Ultimately I tried a (rubbish) linear regression to predict pace and then did some sentiment analysis on the text to look for correlations. The modelling was not very successful 😐

-   `strava_animation.R` This again supported the presentation above, ultimately just cool visualations of the same runs on the canal, split by year. Some of the outputs are in the `/plots` directory.

-   `strava_art.R` This was using Marcus Volz' `strava` package after seeing it on twitter. Package is available at: <https://github.com/marcusvolz/strava> . This ended up trying to make a lot of very samey boring runs look cool. /plots directory will give you the answer to that.

**Functions:**

`xx_strava-functions.R` A few things in here:

-   There isn't actually a function in RStrava to extract the best efforts in a tidy fashion, so `tidy_best_efforts()` is your friend.

-   Ah `tidy_weather()`, I tried to be smart and combine Strava with using the DarkSky API to pull what the weather was for the time and location of a run (courtesy of Bob Rudis' `darksky` package, on CRAN). I can't remember if I ended up getting it working but left as an exercise for the reader. Unfortunately the DarkSky API will be deprecated soon as Apple bought them. 😔

-   `pretty_pace()`and `pretty_time()`were more helper functions so that when I'm trying to visualise a run, the data is presented in a recognisably "Strava" format. TBH was quite pleased with these.
