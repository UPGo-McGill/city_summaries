#################### BASIC FACTS ######################################

source("R/01_helper_functions.R")

Start_date <- as.POSIXlt(End_date)
Start_date$year <- End_date$year - 1

# Number of active listings on end date and a year before

daily %>% 
  filter(Date == End_date) %>% 
  group_by(Property_ID) %>% 
  nrow()

daily %>% 
  filter(Date == Start_date) %>% 
  group_by(Property_ID) %>% 
  nrow()

#  Average number of active listings over the past year
daily %>% 
  filter(Date <= End_date | Date >= Start_date) %>% 
  group_by(Date) %>% 
  summarize(Listings = n()) %>% 
  select("Listings") %>% 
  mean()

