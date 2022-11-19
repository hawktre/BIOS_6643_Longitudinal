## ---------------------------
## Script name: Avalanche EDA
##
## Purpose of script: EDA for Longitudinal Data Analysis Project
##
## Author: Trent Hawkins
##
## Date Created: 2022-10-17
## ---------------------------


# Load Packages -----------------------------------------------------------
require(tidyverse)
require(here)
library(lubridate)


# Read in and clean avalanche data --------------------------------------
avalanche <- read_csv(here("DataRaw/CAIC_avalanches_2008-10-01_2022-10-17.csv")) %>% 
  janitor::clean_names()%>% 
  drop_na(rsize, dsize, bc_zone) %>% 
  filter(!rsize == "U",
         !dsize == "U")

#get date column
avalanche <- avalanche %>% 
  mutate(date = as_date(date),
         year = year(date),
         month = month(date),
         day = day(date),
         rsize = factor(rsize, levels = unique(avalanche$rsize), ordered = T))

# Read in and clean weather data --------------------------------------
setwd(here("DataRaw/Project"))

for (file in list.files()){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("weather_dat")){
    weather_dat <- read.csv(file, header=TRUE) %>% 
      mutate(bc_zone = gsub("\\_.*", "", file))
  }
  
  # if the merged dataset does exist, append to it
  if (exists("weather_dat")){
    temp_dataset <-read.csv(file, header=TRUE) %>% 
      mutate(bc_zone = gsub("\\_.*", "", file))
    weather_dat<-rbind(weather_dat, temp_dataset)
    rm(temp_dataset)
  }
  
}

## Make the zones match avalanche
weather_dat <- weather_dat %>% 
  mutate(bc_zone = case_when(bc_zone == "aspen" ~ "Aspen",
                          bc_zone == "frontrange" ~ "Front Range",
                          bc_zone == "grandmesa" ~ "Grand Mesa",
                          bc_zone == "gunnison" ~ "Gunnison",
                          bc_zone == "north" ~ "Northern San Juan",
                          bc_zone == "sangre" ~ "Sangre de Cristo",
                          bc_zone == "sawatch" ~ "Sawatch Range",
                          bc_zone == "south" ~ "Southern San Juan",
                          bc_zone == "steamboat" ~ "Steamboat & Flat Tops",
                          bc_zone == "vail" ~ "Vail & Summit County",),
         date = as_date(Date),
         year = year(date),
         month = month(date),
         day = day(date)) %>% 
  rename("day_avg_temp" = Air.Temperature.Average..degF.,
         "day_max_temp" = Air.Temperature.Maximum..degF.,
         "day_min_temp" = Air.Temperature.Minimum..degF.,
         "day_start_precip" = Precipitation.Accumulation..in..Start.of.Day.Values,
         "day_start_snow" = Snow.Water.Equivalent..in..Start.of.Day.Values,
         "day_precip" = Precipitation.Increment..in.)




# Avalanche Data Visualizations -------------------------------------------
#Total avalanches over time by zone
avalanche %>%
  filter(trigger == "N") %>% 
  group_by(year, bc_zone) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x = year, y = N, col = bc_zone)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(2010, 2022, 1))

#Rsize by zone
avalanche %>%
  filter(trigger == "N") %>%
  group_by(rsize, bc_zone) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x = bc_zone, y = N, fill = rsize)) +
  geom_bar(position = "dodge", stat = "identity")

#Dsize by zone
avalanche %>%
  filter(trigger == "N") %>%
  group_by(dsize, bc_zone) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x = bc_zone, y = N, fill = dsize)) +
  geom_bar(position = "dodge", stat = "identity")

#Rsize 2 avalanches over time by zone
avalanche %>%
  filter(trigger == "N", rsize >= "R2", year < 2022) %>% 
  group_by(year, bc_zone) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x = year, y = N, col = bc_zone)) +
  geom_smooth(method = "loess", se = F) +
  labs(x = "Year",
       y = "Avalanches (Natural Trigger)",
       title = "Naturally Triggered Avalanches Annually (Since 2010)",
       color = "Avalanche Zone")+
  scale_x_continuous(breaks = seq(2010, 2021, 1)) +
  theme(legend.position = "bottom")

#Rsize 2 avalanches over time by zone (artificial)    
avalanche %>%
  filter(trigger != "N", rsize >= "R2") %>% 
  group_by(year, bc_zone) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x = year, y = N, col = bc_zone)) +
  geom_smooth(method = "loess", se = F) +
  labs(x = "Year",
       y = "Avalanches (Artificial Trigger)",
       title = "Artificially Triggered Avalanches Annually (Since 2010)",
       color = "Avalanche Zone")+
  scale_x_continuous(breaks = seq(2010, 2022, 1)) +
  theme(legend.position = "bottom")

#Rsize 2 avalanches over time by zone
avalanche %>%
  filter(trigger == "N", 
         rsize >= "R2") %>% 
  group_by(year, bc_zone) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x = year, y = N, col = bc_zone)) +
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = seq(2010, 2022, 1))

# Weather Data Visualization ----------------------------------------------
#Mean temperature over all times collected
weather_dat %>% 
  group_by(year, bc_zone) %>% 
  summarise(mean_temp = mean(day_avg_temp)) %>% 
  ggplot(aes(x = year, y = mean_temp, color = bc_zone)) +
  geom_smooth(method = "loess", se = F)+
  scale_x_continuous(breaks = seq(min(weather_dat$year), max(weather_dat$year), 5))

#Mean Temperature over times contained in avalanche data
weather_dat %>% 
  filter(year >= min(avalanche$year)) %>% 
  group_by(date, bc_zone) %>% 
  summarise(mean_temp = mean(day_avg_temp)) %>% 
  ggplot(aes(x = date, y = mean_temp, color = bc_zone)) +
  geom_smooth(method = "loess", se = F) +
  labs(x = "Year",
       y = "Mean Daily Temperature",
       color = "Avalanche Zone",
       title = "Mean Daily Temperature by Zone (2010-Present)") +
  theme(legend.position = "bottom")

weather_dat %>% 
  filter(year >= min(avalanche$year)) %>% 
  group_by(date, bc_zone) %>% 
  summarise(swe = max(day_start_snow)) %>% 
  ggplot(aes(x = date, y = swe, color = bc_zone)) +
  geom_smooth(method = "loess", se = F) +
  labs(x = "Year",
       y = "Total Snow-Water Equivilent (Daily)",
       color = "Avalanche Zone",
       title = "Total Snow-Water Equivilent by Zone (2010-Present)") +
  theme(legend.position = "bottom")
