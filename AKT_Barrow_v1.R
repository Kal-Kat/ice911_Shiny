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
library(lubridate)

#Read and save raw data from beyond66.com website
#####
raw_data <-
        read.table(
                url("http://beyond66.com/swims/root/AKT_Barrow_JELLY_2017.csv"),
                header = FALSE,
                sep = ",",
                dec = ".",
                as.is = TRUE,
                fill = TRUE,
                comment.char = "",
                skip = 0 # no skip
        )
# save specific raw data to a timestamped csv files
# if you don't specify the path, the cwd is assumed

saveCSV <- function(dataf, path = getwd()) {
        # dataf is a df that gets saved as .csv file
        # file name will have timestamp prepended to it.
        #       Example: 2017_05_22_160445_raw_data.csv
        # If no path is specified, file will be saved
        # in the current working directory
        # WARNING: function might fail on Windows
        
        now <- Sys.time()
        path <- paste0(path,
                       "/data/",
                       format(now, "%Y_%m_%d_%H%M%S_"),
                       deparse(substitute(dataf)),
                       ".csv")
        write_csv(dataf, path)
}

deleteCSV <- function(file_name, several = FALSE) {
        # deletes single or multiple csv files
        # NEEDS MORE WORK
        if (several == TRUE) {
                system(paste0("rm ", file_name, "*"))
        }
        else {
                system(paste0("rm ", file_name))
        }
}


# save raw data to disk as a csv
saveCSV(raw_data)

#####
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
                "date",
                "time",
                "time_zone",
                "buoy",
                "unknown1",
                "unknown2",
                "unknown3",
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
                "temp_bs7",
                "temp_bs8"
        )



#####
# print("Dimensions of tidy_data:")
# print(dim(tidy_data))
# print(sapply(tidy_data, function(x) sum(is.na(x))))
# missing_values <- sapply(tidy_data, function(x)
#         sum(is.na(x)))

# # # Cleanup
# # Use grep("US",OlsonNames(),value=TRUE) to find all US timezones
tidy_data <-
        tidy_data %>%
        filter(solar1 <= 2000) %>% 
        mutate(
                # formatting date,time vectors
                date_time_alaska = sprintf("%s %s", date, time),
                date_time_alaska = parse_date_time(date_time_alaska,
                                                   orders = "mdy HMS",
                                                   tz = "US/Alaska"),
                #create US/Pacific date timestamps
                date_time_pacific = with_tz(date_time_alaska,
                                            tz = "US/Pacific"),
                #create UTC date timestamps
                date_time_utc = with_tz(date_time_alaska,
                                        tz = "UTC"),
                #strip spaces from buoy character vector
                buoy = gsub("\\.|/|\\-|\"|\\s", "" , buoy)
        ) %>%
        # remove redundant and ambigous field variables
        select(
                -c(
                        date,
                        time,
                        time_zone,
                        unknown1,
                        unknown2,
                        unknown3,
                        north,
                        west,
                        solar3,
                        solar4,
                        temp_bs8
                )
        ) %>%
        # convert buoy to a factor variable
        filter(buoy %in% c("A", "B", "C", "D")) %>%
        mutate(buoy = factor(buoy))

######
# unit conversions and scaling
scaled_data <- tidy_data %>%
        mutate(battery_voltage = as.numeric(battery_voltage) * 0.01596,
               solar1 = solar1 )
# NEED Solar and Temperature conversions

p1 <- ggplot(scaled_data, aes(x = date_time_alaska,
                              y = solar1,
                              col = buoy)) +
        geom_point(alpha = 0.8) +  
        scale_x_datetime(name = "Date, US/Alaska",
                         date_minor_breaks = "1 day") +
        scale_y_continuous(name = "Incoming Solar Radiation") + 
        theme_bw()

p2 <- ggplot(scaled_data, aes(x = date_time_alaska,
                              y = solar2,
                              col = buoy)) +
        geom_point(alpha = 0.8) + facet_wrap( ~ buoy)
p3 <- ggplot(scaled_data,
             aes(x = date_time_alaska,
                 y = surface_temp,
                 col = buoy)) +
        geom_point(alpha = 0.8, size = 0.4) + facet_wrap( ~ buoy)

p4 <- ggplot(scaled_data, aes(x = date_time_alaska,
                              y = temp_bs4,
                              col = buoy)) +
        geom_point(alpha = 0.8) + facet_wrap( ~ buoy)
p1_ly <- ggplotly(p1)
p2_ly <- ggplotly(p2)
p3_ly <- ggplotly(p3)


##########
#sanity checks
summary(tidy_data$solar1[!is.na(tidy_data$solar1)])

