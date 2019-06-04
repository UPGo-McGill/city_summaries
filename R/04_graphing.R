############## GRAPHING #################################

source("R/01_helper_functions.R")

# Active airbnb listings over time

figure2 <- ggplot(daily %>% 
         group_by(Date) %>% 
         summarize(Listings = n())) +
  geom_line(aes(Date, Listings)) 

ggsave("output/figure2.jpg")

