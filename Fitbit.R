### Fitbit.R - Olof Hoverfält - 2020

# Playground for manipulating and plotting Fitbit data
# Input: personal Fitbit profile data
# Using the fitbitr package: https://github.com/teramonagi/fitbitr

####################################################
################ SET UP ENVIRONMENT ################
####################################################

# Remove all objects from workspace
rm(list = ls())

# Load required packages
library("fitbitr")
library("ggplot2")
library("dplyr")

# Source required files
source("Fitbit-API-Key.R")

# Set Fitbit key and secret
FITBIT_KEY    <- get_fitbit_key()
FITBIT_SECRET <- get_fitbit_secret()
FITBIT_CALLBACK <- "http://localhost:1410/" 

# Authenticate and get token
token <- fitbitr::oauth_token()


##########################################
################ ACTIVITY ################
##########################################

# Fitbit API: Time series options are 1d, 7d, 30d, 1w, 1m, 3m, 6m, 1y
# Fitbit API: Intraday time series number of data points to include either 1min or 15min.
# Fitbit API: https://dev.fitbit.com/build/reference/web-api/activity/


# Set test date
date <- "2020-04-25"

# Get daily activity summary
activity_summary <- get_activity_summary(token, date)
activity_summary$activities

# Get daily step data for entire item data period and remove duplicates
steps_2020 <- get_activity_time_series(token, "steps", date=date, period="1y")
steps_2019 <- get_activity_time_series(token, "steps", date="2019-12-31", period="1y")
steps_2018 <- get_activity_time_series(token, "steps", date="2018-12-31", period="1y")
steps <- rbind(steps_2020, rbind(steps_2018, steps_2019))
steps <- steps[!duplicated(steps$dateTime),]

# Convert variables to correct type and arrange by date
steps <- steps %>%
  mutate(date = as.POSIXct(strptime(steps$dateTime, "%Y-%m-%d"))) %>%
  mutate(steps = as.numeric(value)) %>%
  select(-dateTime, -value) %>%
  arrange(date)

# Plot steps
ggplot2::ggplot(steps, aes(x=date, y=steps)) + geom_col()




# Get activity intraday time series
steps <- get_activity_intraday_time_series(token, "steps", date, detail_level="1min")
steps$time <- as.POSIXct(strptime(paste0(steps$dateTime, " ", steps$dataset_time), "%Y-%m-%d %H:%M:%S"))
ggplot2::ggplot(steps, aes(x=time, y=dataset_value)) + geom_col()


# Get Activity Types (complicated nested list)
get_activity_types(token)

# Get Activity Type (Walk=90013)
get_activity_type(token, 90013)

# Get Frequent Activities
get_frequent_activities(token)

# Get Recent Activities
get_recent_activity_types(token)

# Get Lifetime Stats
str(get_lifetime_stats(token))



############################################
################ HEART RATE ################
############################################

# Fitbit API: https://dev.fitbit.com/build/reference/web-api/heart-rate/


# Set test date
date <- "2020-04-19"

# Get heart rate time series
heart_rate <- get_heart_rate_time_series(token, date=date, period="7d")
heart_rate$value$restingHeartRate



# Get intraday heart rate time series
heart_rate <- get_heart_rate_intraday_time_series(token, date=date, detail_level="1min")

# Add date
heart_rate$date = date

# Convert variables to correct type
heart_rate <- heart_rate %>%
  mutate(dateTime = as.POSIXct(strptime(paste0(date, " ", time), "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(heart_rate = as.numeric(value)) %>%
  select(-date, -time, -value)

# Plot heart rate
ggplot2::ggplot(heart_rate, aes(x=dateTime, y=heart_rate)) + geom_col()


ggplot(heart_rate, aes(x=time, y=value)) + geom_line()




#######################################
################ SLEEP ################
#######################################

# Fitbit API: https://dev.fitbit.com/build/reference/web-api/sleep/


# Get Sleep Logs(date is character or Date)
x <- get_sleep_logs(token, date)

#Get the current sleep goal.
get_sleep_goal(token)

#Get Sleep Time Series
get_sleep_time_series(token, "timeInBed", date, period="7d")
get_sleep_time_series(token, "efficiency", date, period="7d")



########################################
################ WEIGHT ################
########################################

# Fitbit API: xxx


get_body_fat_logs(token, date)

get_weight_logs(token, date)

get_body_time_series(token, "weight", date = date, period="7d")





#################################################################
################ Fitbit management and resources ################
#################################################################

# https://dev.fitbit.com/apps
# https://dev.fitbit.com/build/reference/web-api/


### Resources for the use of fitibitr 

# Alternative for setting key
# Sys.setenv(FITBIT_KEY = "<your-fitbit-key>", FITBIT_SECRET = "<your-firbit-secret>")

# OAuth 2.0: Authorization URI: https://www.fitbit.com/oauth2/authorize
# OAuth 2.0: Access/Refresh Token Request URI: https://api.fitbit.com/oauth2/token

## Installing the fitbitr package, not available on CRAN
# fitbitr package: https://github.com/teramonagi/fitbitr
# install.packages("devtools")
# devtools::install_github("teramonagi/fitbitr")



