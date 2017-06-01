# Shiny app for displaying AKT_Barrow Buoy Temperatures
# Created on: May 31, 2017
# Author: Kalyan, Version: 1.0

########
# Objectives: 
# 1. Display temperatures of Barrow SD card data
# 2. Display weather station temps along with buoy temps
# 3. Use Sunset, Sunrise info and wind info

########
# Issues:
#         1. Understand the missing data in timeseries plot
#         2. colnames of data and unit conversion
#         3. Save the data in sqlite db or a flat file and call it from app
#         4. Add reactivity and interactivity
#         5. Understand the data trends


#libraries
#####
library(shiny)
library(openintro) # Data to test new app features
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(latex2exp)
library(tibble)
library(readr)
library(lubridate)
library(shinydashboard)

#load central buoy data
scaled_SDdata <- readRDS("scaled_SDdata.Rds")


# Define UI for application that draws a histogram
#####
header <- dashboardHeader(
        title = "Ice911: Barrow SD
        Card Data"
        
#####

        # # Dropdown menu for messages
        # dropdownMenu(
        #         type = "messages",
        #         badgeStatus = "success",
        #         messageItem("Support Team",
        #                     "This is the content of a message.",
        #                     time = "5 mins"),
        #         messageItem("Support Team",
        #                     "This is the content of another message.",
        #                     time = "2 hours"),
        #         messageItem("New User",
        #                     "Can I get some help?",
        #                     time = "Today")
        # ),
        # 
        # # Dropdown menu for notifications
        # dropdownMenu(
        #         type = "notifications",
        #         badgeStatus = "warning",
        #         notificationItem(
        #                 icon = icon("users"),
        #                 status = "info",
        #                 "5 new members joined today"
        #         ),
        #         notificationItem(
        #                 icon = icon("warning"),
        #                 status = "danger",
        #                 "Resource usage near limit."
        #         ),
        #         notificationItem(
        #                 icon = icon("shopping-cart", lib = "glyphicon"),
        #                 status = "success",
        #                 "25 sales made"
        #         ),
        #         notificationItem(
        #                 icon = icon("user", lib = "glyphicon"),
        #                 status = "danger",
        #                 "You changed your username"
        #         )
        # ),
        # 
        # # Dropdown menu for tasks, with progress bar
        # dropdownMenu(
        #         type = "tasks",
        #         badgeStatus = "danger",
        #         taskItem(value = 20, color = "aqua",
        #                  "Refactor code"),
        #         taskItem(value = 40, color = "green",
        #                  "Design new layout"),
        #         taskItem(value = 60, color = "yellow",
        #                  "Another task"),
        #         taskItem(value = 80, color = "red",
        #                  "Write documentation")
        # )
        
)
#####
sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
        navbarPage("Barrow  SD Card Data",
                   tabPanel("Solar Radiation",
                            fluidPage(
                                    
                                    box(plotlyOutput(outputId = "solar"), 
                                        width = 12)
                            )),
                   tabPanel("Buoy String Temperatures",
                            fluidPage(
                                    
                                    box(plotlyOutput(outputId = "temps"), 
                                        width = 12)
                            )),
                   tabPanel("Datalogger Battery",
                            fluidPage(
                                    box(plotlyOutput(outputId = "battery"), 
                                        width = 12)
                            ))
        )
)
       
#####
        # fluidRow(
        #         box(
        #                 title = "Title 1",
        #                 width = 4,
        #                 solidHeader = TRUE,
        #                 status = "primary",
        #                 "Box content"
        #         ),
        #         box(
        #                 title = "Title 2",
        #                 width = 4,
        #                 solidHeader = TRUE,
        #                 "Box content"
        #         ),
        #         box(
        #                 title = "Title 1",
        #                 width = 4,
        #                 solidHeader = TRUE,
        #                 status = "warning",
        #                 "Box content"
        #         )
        # )
#####
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
        ##solar radiation
        output$solar <- renderPlotly({
                p1 <- scaled_SDdata %>% 
                        gather(key,value, solar1, solar2) %>%
                        ggplot(aes(x = date_time_alaska)) +
                        geom_line(aes(y = value, col = key)) +
                        geom_point(aes(y = value, col = key), alpha = 0.4) +
                        scale_x_datetime(name = "Date, US/Alaska",
                                         date_minor_breaks = "1 day") +
                        scale_y_continuous(name = "Solar Radiation") +
                        theme_bw()
                ggplotly(p1)
        })

        ##temperatures
        output$temps <- renderPlotly({
                p2 <- scaled_SDdata %>%
                        gather(key,value, ambient_temp, surface_temp,
                               temp_bs1, temp_bs2, temp_bs3, temp_bs4, 
                               temp_bs5, temp_bs6) %>%
                        ggplot(aes(x = date_time_alaska)) +
                        geom_line(aes(y = value, col = key)) +
                        # geom_point(aes(y = value, col = key), 
                        #            alpha = 0.1,
                        #            shape = ".") +
                        scale_x_datetime(name = "Date, US/Alaska",
                                         date_minor_breaks = "1 day") +
                        scale_y_continuous(name = "Temperature, degreeC") + 
                        #TeX('Temperature, $^\\degree$C')
                        theme_bw()
                ggplotly(p2)
        })
        
        ##battery voltages
        output$battery <- renderPlotly({
                p3 <- scaled_SDdata %>% 
                        ggplot(aes(x = date_time_alaska)) +
                        geom_line(aes(y = battery_voltage)) +
                        geom_point(aes(y = battery_voltage), alpha = 0.4, col = "red") +
                        scale_x_datetime(name = "Date, US/Alaska",
                                         date_minor_breaks = "1 day") +
                        scale_y_continuous(name = "Battery Voltage, volts") +
                        theme_bw()
                ggplotly(p3)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

