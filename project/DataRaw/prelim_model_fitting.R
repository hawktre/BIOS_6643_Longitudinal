## ---------------------------
## Script name: Preliminary Model Fitting for Longitudinal Project
##
## Purpose of script: Fit the preliminary models for Phase 3 update
##
## Author: Trent Hawkins
##
## Date Created: 2022-11-18
## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(here)
require(naniar)

# Read in the data --------------------------------------------------------

avalanche <- read_csv(here("project/DataProcessed/avalanche_dat_cleaned.csv"))

weather <- read_csv(here("project/DataProcessed/weather_dat_merged.csv"))

avi_full <- left_join(avalanche, weather, by = c("date" = "Date"))


# Drop anything with >40% Missingness -------------------------------------
avi_colnames <- names(avi_full)
avi_hi_missingness <- NULL

for (i in avi_colnames) {
  if (sum(is.na(avi_full[[i]]))/length(avi_full[[i]]) > 0.2){
    avi_hi_missingness <- append(avi_hi_missingness, T)
  }
  else {
    avi_hi_missingness <- append(avi_hi_missingness, F)
  }
}

avi_full <- avi_full[,!avi_hi_missingness] 

avi_full <- avi_full %>% 
  select(-c(bc_zone.y, date.y, year.y, month.y, day.y))


# Filter to include only the data we want to model  -----------------------

avi_full <- avi_full %>% 
  filter(trigger == "N", 
         date_known == "Known" | date_known == "Estimated") %>% 
  select(rsize, year.x, day_start_snow, day_precip, day_avg_temp, elev) %>% 
  mutate(rsize_fact = as.numeric(gsub("R", "", rsize)))

write_csv(avi_full, here("project/DataProcessed/avalanche_dat_cleaned.csv"))