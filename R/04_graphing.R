############## GRAPHING #################################

source("R/01_helper_functions.R")

# Active airbnb listings over time

ggplot(daily %>% 
         group_by(Date) %>% 
         summarize(Listings = n())) +
  geom_smooth(aes(Date, Listings)) 