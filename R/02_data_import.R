########################### DATA IMPORT ##########################################
## Load helpers

source("R/01_helper_functions.R")

# See CMA_Codes or CD_Codes for list
CMA_Codes <- filter(list_census_regions(dataset = "CA16"), level == "CMA") 
CD_Codes <- filter(list_census_regions(dataset = "CA16"), level == "CD")

citycode = "59935"
cityname = "Victoria, British Columbia"

## Specify dates (start date should be at least two years before the 
# end date in order to allow for year to year comparisons)
End_date <- as.Date("2019-04-30")
Start_date <- as.Date("2016-07-01")

## Import city geometries
# For a regional municipality/area not in a city, replace instances of "CMA" with "CD"

city <- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = citycode),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(32618)

## Import private Airbnb files

property <-
  read_csv("data/property.csv", col_types = cols_only(
    `Property_ID` = col_character(),
    `Listing_Title` = col_character(),
    `Property_Type` = col_character(),
    `Listing_Type` = col_character(),
    `Created` = col_date(format = ""),
    `Scraped` = col_date(format = ""),
    Latitude = col_double(),
    Longitude = col_double(),
    `City` = col_skip(),
    `Airbnb_PID` = col_double(),
    `Airbnb_HID` = col_double(),
    `HomeAway_PID` = col_character(),
    `HomeAway_HID` = col_character())) %>% 
  set_names(c("Property_ID", "Listing_Title", "Property_Type", "Listing_Type",
              "Created", "Scraped", "Latitude", "Longitude", "Airbnb_PID",
              "Airbnb_HID", "HomeAway_PID", "HomeAway_HID")) %>% 
  arrange(Property_ID) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32618) %>% 
  filter(Property_Type %in% c(
    "House", "Private room in house", "Apartment", "Cabin",
    "Entire condominium", "Townhouse", "Condominium", "Entire apartment",
    "Private room", "Loft", "Place", "Entire house", "Villa", "Guesthouse",
    "Private room in apartment", "Guest suite", "Shared room in dorm",
    "Chalet", "Dorm", "Entire chalet", "Shared room in loft", "Cottage",
    "Resort", "Serviced apartment", "Other", "Bungalow", "Farm stay",
    "Private room in villa", "Entire loft", "Entire villa",
    "Private room in guesthouse", "Island", "Entire cabin", "Vacation home",
    "Entire bungalow", "Earth house", "Nature lodge", "In-law",
    "Entire guest suite", "Shared room in apartment", "Private room in loft",
    "Tiny house", "Castle", "Earth House", "Private room in condominium",
    "Entire place", "Shared room", "Hut", "Private room in guest suite",
    "Private room in townhouse", "Timeshare", "Entire townhouse",
    "Shared room in house", "Entire guesthouse", "Shared room in condominium",
    "Cave", "Private room in cabin", "Dome house",
    "Private room in vacation home", "Private room in dorm",
    "Entire serviced apartment", "Private room in bungalow",
    "Private room in serviced apartment", "Entire Floor", "Entire earth house",
    "Entire castle", "Shared room in chalet", "Shared room in bungalow",
    "Shared room in townhouse", "Entire cottage", "Private room in castle",
    "Private room in chalet", "Private room in nature lodge", "Entire in-law",
    "Shared room in guesthouse", "Casa particular", "Serviced flat", "Minsu",
    "Entire timeshare", "Shared room in timeshare", "Entire vacation home",
    "Entire nature lodge", "Entire island", "Private room in in-law",
    "Shared room in serviced apartment", "Shared room in cabin", "Entire dorm",
    "Entire cave", "Private room in timeshare", "Shared room in guest suite",
    "Private room in cave", "Entire tiny house",
    "Private room in casa particular (cuba)", "Casa particular (cuba)",
    "Private room in cottage", "Private room in tiny house",
    "Entire casa particular", "")) %>% 
  select(-Property_Type)

daily <- 
  read_csv("data/daily.csv", col_types = cols(
    `Property_ID` = col_character(),
    Date = col_date(format = ""),
    Status = col_factor(levels = c("U", "B", "A", "R")),
   # `Booked Date` = col_skip(),
    `Price` = col_double(),
    #`Price (Native)` = col_skip(),
    #`Currency Native` = col_skip(),
    #`Reservation ID` = col_skip(),
    `Airbnb_PID` = col_double(),
    `HomeAway_PID` = col_character())) %>% 
  set_names(c("Property_ID", "Date", "Status", "Price", "Airbnb_PID", 
              "HomeAway_PID")) %>% 
  filter(!is.na(Status)) %>%
  arrange(Property_ID, Date)


## Trim listings to city geometry and inputted dates
property <-
  property %>% 
  filter(Property_ID %in% daily$Property_ID,
         Scraped >= Start_date,
         Created <= End_date) %>% 
  st_join(st_buffer(city["geometry"], 200),
          join = st_within, left = FALSE)

daily <- 
  daily %>% 
  filter(Property_ID %in% property$Property_ID,
         Date >= Start_date,
         Date <= End_date)


## Join property and daily file

daily <- inner_join(daily, st_drop_geometry(property)) %>% 
  select(Property_ID, Date, Status, Price, Airbnb_PID, HomeAway_PID, Airbnb_HID,
         HomeAway_HID, Listing_Type)


## Find FREH listings and revenue

property <- 
  daily %>%
  group_by(Property_ID) %>% 
  summarize(
    n_reserved = sum(Status == "R"),
    n_available = sum(Status == "A" | Status == "R"),
    revenue = sum((Status == "R") * Price),
    FREH = if_else(
      first(Listing_Type) == "Entire home/apt" & n_reserved >= 90 &
        n_available >= 183, TRUE, FALSE)) %>% 
  inner_join(property, .)


## Find multi-listings

daily <- strr_multilistings(daily, listing_type = Listing_Type,
                            host_ID = Airbnb_HID, date = Date)

property <- 
  daily %>%
  group_by(Property_ID) %>% 
  summarize(ML = as.logical(ceiling(mean(ML)))) %>% 
  inner_join(property, .)

# Identify ghost hotels
GH_list <-
  strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, Start_date,
             End_date, listing_type = Listing_Type) %>% 
  pull(property_IDs) %>%
  unlist() %>%
  unique()

property <-
  property %>% 
  mutate(GH = if_else(Property_ID %in% GH_list, TRUE, FALSE))

rm(GH_list)
