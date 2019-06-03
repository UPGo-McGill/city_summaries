#################### BASIC FACTS ######################################

source("R/01_helper_functions.R")


# Update USD to CAD exchange rate
exchange_rate <- 1.34

# Set up timeframes
year_prior <- as.POSIXlt(End_date)
year_prior$year <- year_prior$year - 1
year_prior_prior <- as.POSIXlt(year_prior)
year_prior_prior$year <- year_prior$year - 1

# Number of active listings on end date and a year before

daily %>% 
  filter(Date == End_date) %>% 
  group_by(Property_ID) %>% 
  nrow()

daily %>% 
  filter(Date == year_prior) %>% 
  group_by(Property_ID) %>% 
  nrow()

# Percent of listings which are entire homes on end date and a year before

nrow(daily %>% 
       filter(Date == End_date) %>% 
       group_by(Property_ID) %>% 
       filter(Listing_Type == "Entire home/apt"))/
  nrow(daily %>% 
         filter(Date == End_date))

nrow(daily %>% 
       filter(Date == year_prior) %>% 
       group_by(Property_ID) %>% 
       filter(Listing_Type == "Entire home/apt"))/
  nrow(daily %>% 
         filter(Date == year_prior))


#  Average number of active listings over the past year and previous year

daily %>% 
  filter(Date <= End_date & Date >= year_prior) %>% 
  group_by(Date) %>% 
  summarize(Listings = n()) %>%
  summarise(mean_Listings = mean(Listings))

daily %>% 
  filter(Date <= year_prior & Date >= year_prior_prior) %>% 
  group_by(Date) %>% 
  summarize(Listings = n()) %>%
  summarise(mean_Listings = mean(Listings))


# Revenue over past twelve months and twelve months prior to that

daily %>% 
  filter(Date <= End_date & 
           Date >= year_prior &
           Status == "R" &
           !is.na(Price)) %>%
 summarise(sum_revenue = sum(Price)*exchange_rate)

daily %>% 
  filter(Date <= year_prior 
         & Date >= year_prior_prior 
         & Status == "R" &
        !is.na(Price)) %>%
summarise(sum_revenue = sum(Price)*exchange_rate)


# Housing loss on the end date and a year prior

strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, year_prior,
             End_date, listing_type = Listing_Type) %>% 
  filter(date == End_date) %>% 
  group_by(ghost_ID) %>% 
  summarize(n = sum(housing_units)) %>% 
  ungroup() %>% 
  summarize(GH_housing_loss = sum(n)) +
nrow(daily %>% 
  filter(Date == End_date) %>% 
  inner_join(property, .) %>% 
  filter(FREH == TRUE))


strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, year_prior_prior,
             year_prior, listing_type = Listing_Type) %>% 
  filter(date == year_prior) %>% 
  group_by(ghost_ID) %>% 
  summarize(n = sum(housing_units)) %>% 
  ungroup() %>% 
  summarize(GH_housing_loss = sum (n)) +
  nrow(daily %>% 
         filter(Date == year_prior) %>% 
         inner_join(property, .) %>% 
         filter(FREH == TRUE))
