#################### BASIC FACTS ######################################

source("R/01_helper_functions.R")

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
# how to do this without assignment?

listings_past_year <- daily %>% 
  filter(Date <= End_date & Date >= year_prior) %>% 
  group_by(Date) %>% 
  summarize(Listings = n())

mean(listings_past_year$Listings)

listings_past_year <- daily %>% 
  filter(Date <= year_prior & Date >= year_prior_prior) %>% 
  group_by(Date) %>% 
  summarize(Listings = n())

mean(listings_past_year$Listings)
rm(listings_past_year)


# Revenue over past twelve months and twelve months prior to that
# how to do this without assignment?

revenue <- daily %>% 
  filter(Date <= End_date & 
           Date >= year_prior &
           Status == "R")

sum(revenue$Price, na.rm = TRUE)

revenue <- daily %>% 
  filter(Date <= year_prior 
         & Date >= year_prior_prior 
         & Status == "R")

sum(revenue$Price, na.rm = TRUE)
rm(revenue)

# Housing loss on the end date and a year prior
GH_list <-
  strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, year_prior,
             End_date, listing_type = Listing_Type) %>% 
  filter(date == End_date) %>% 
  group_by(ghost_ID) %>% 
  summarize(n = sum(housing_units)) 

sum(GH_list$n) +
nrow(daily %>% 
  filter(Date == End_date) %>% 
  inner_join(property, .) %>% 
  filter(FREH == TRUE))


GH_list <-
  strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, year_prior_prior,
             year_prior, listing_type = Listing_Type) %>% 
  filter(date == year_prior) %>% 
  group_by(ghost_ID) %>% 
  summarize(n = sum(housing_units)) 

sum(GH_list$n) +
  nrow(daily %>% 
         filter(Date == year_prior) %>% 
         inner_join(property, .) %>% 
         filter(FREH == TRUE))
