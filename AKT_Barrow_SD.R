# Buoy Data from Barrow
# Date: May 24, 2017
# Author: Kalyan

#################################################################
## Don't run in production code:
## remove (almost) everything in the working environment.
## Will generate no warning, so don't do this unless you are really sure.
rm(list = ls())
##################################################################

# load packages
library(dplyr)
library(ggplot2)
library(plotly)
library(tibble)
library(readr)
library(tidyr)
library(lubridate)
library(latex2exp)

#Read and save raw data from beyond66.com website
#####

raw_data <-
        read.table(
                #setwd() to the correct directory
                file = "./Barrow_Data/SD_Data/sd_data.csv",
                header = FALSE,
                sep = ",",
                dec = ".",
                as.is = TRUE,
                fill = TRUE,
                comment.char = "",
                skip = 0 # no skip
        )

#####
# save specific raw data to a timestamped csv files
# # if you don't specify the path, the cwd is assumed
# 
# saveCSV <- function(dataf, path = getwd()) {
#         # dataf is a df that gets saved as .csv file
#         # file name will have timestamp prepended to it.
#         #       Example: 2017_05_22_160445_raw_data.csv
#         # If no path is specified, file will be saved
#         # in the current working directory
#         # WARNING: function might fail on Windows
#         
#         now <- Sys.time()
#         path <- paste0(path,
#                        "/data/",
#                        format(now, "%Y_%m_%d_%H%M%S_"),
#                        deparse(substitute(dataf)),
#                        ".csv")
#         write_csv(dataf, path)
# }
# 
# deleteCSV <- function(file_name, several = FALSE) {
#         # deletes single or multiple csv files
#         # NEEDS MORE WORK
#         if (several == TRUE) {
#                 system(paste0("rm ", file_name, "*"))
#         }
#         else {
#                 system(paste0("rm ", file_name))
#         }
# }
# 
# 
# # save raw data to disk as a csv
# saveCSV(raw_data)
# 
# #####
#Clean and transform raw data
tidy_data <- tbl_df(raw_data)

#NOTE:  date & time are in Alaska time zone,
#       unknown1 through unknown3 are ambigous field variables
#       tilt_flag is a binary variable: not upright=0, upright=1
#               currently tilt_flag is not populated
#       solar1 is upward pointing solar radiation sensor
#       solar2 is downward pointing solar radiation sensor
#       currently solar3 & solar4 are not populated
#       temp_bs1 through temp_bs8: temperatures below the buoy surface
#       temp_bs1 is the closest to buoy, sensors separated by 1 ft.
#               currently, temp_bs8 is not avaialable

colnames(tidy_data) <-
        c(
                "file.number",
                "date", #dmy
                "time", #HMS: 00:00:00
                "lat_n",
                "north",
                "lon_w",
                "west",
                "tilt_flag",
                "battery_voltage",
                "solar1",
                "solar2",
                "solar3",
                "solar4",
                "ambient_temp",
                "surface_temp",
                "temp_bs1",
                "temp_bs2",
                "temp_bs3",
                "temp_bs4",
                "temp_bs5",
                "temp_bs6",
                "unknown", # always stays equal to 20
                "eol" # "*" used for end of line

        )


# # # Cleanup
# # Use grep("US",OlsonNames(),value=TRUE) to find all US timezones
tidy_data <-
        tidy_data %>%
        mutate(
                # formatting date,time vectors
                date = as.integer(date),
                time = as.integer(time),
                date_time_alaska = sprintf("%06d %06d", date, time),
                date_time_alaska = parse_date_time(date_time_alaska,
                                                   orders = "dmy HMS",
                                                   tz = "US/Alaska"),
                #create US/Pacific date timestamps
                date_time_pacific = with_tz(date_time_alaska,
                                            tz = "US/Pacific"),
                #create UTC date timestamps
                date_time_utc = with_tz(date_time_alaska,
                                        tz = "UTC")
        ) %>%
        # remove redundant and ambigous field variables
        select(
                -c(
                        file.number,
                        date,
                        time,
                        north,
                        west,
                        solar3,
                        solar4,
                        unknown,
                        eol
                )
        )

######
# unit conversions and scaling
scaled_SDdata <- tidy_data %>%
        mutate(battery_voltage = as.numeric(battery_voltage) * 0.01596)
# NEED Solar and Temperature conversions

##### 
#Plots
#ts <- seq.POSIXt(as.POSIXlt(mi), as.POSIXlt("2001-09-01 0:07"), by="min")
ts <- seq.POSIXt(min(scaled_SDdata$date_time_alaska), 
                 max(scaled_SDdata$date_time_alaska), by="hour")
df <- data.frame(date_time_alaska=ts)
scaled_SDdata <- full_join(df,scaled_SDdata)

#save scaled_SDdata as RDS to use it in app
saveRDS(scaled_SDdata, file="scaled_SDdata.Rds")

p1 <- scaled_SDdata %>% 
        gather(key,value, solar1, solar2) %>%
        ggplot(aes(x = date_time_alaska)) +
        geom_line(aes(y = value, col = key)) +
        scale_x_datetime(name = "Date, US/Alaska",
                         date_minor_breaks = "1 day") +
        scale_y_continuous(name = "Solar Radiation") +
        theme_bw()


p2 <- scaled_SDdata %>%
        gather(key,value, ambient_temp, surface_temp, temp_bs1,
               temp_bs2, temp_bs3, temp_bs4, temp_bs5, temp_bs6) %>%
        ggplot(aes(x = date_time_alaska)) +
        geom_line(aes(y = value, col = key)) +
        scale_x_datetime(name = "Date, US/Alaska",
                         date_minor_breaks = "1 day") +
        scale_y_continuous(name = "Temperature, degreeC") + 
        #TeX('Temperature, $^\\degree$C')
        theme_bw()

p3 <- scaled_SDdata %>% 
        ggplot(aes(x = date_time_alaska)) +
        geom_line(aes(y = battery_voltage)) +
        geom_point(aes(y = battery_voltage), alpha = 0.4, col = "red") +
        scale_x_datetime(name = "Date, US/Alaska",
                         date_minor_breaks = "1 day") +
        scale_y_continuous(name = "Battery Voltage, volts") +
        theme_bw()


