############## GRAPHING #################################

source("R/01_helper_functions.R")

# Set up timeframes
year_prior <- as.POSIXlt(End_date)
year_prior$year <- year_prior$year - 1
year_prior_prior <- as.POSIXlt(year_prior)
year_prior_prior$year <- year_prior$year - 1

# Active airbnb listings over time

figure2 <- ggplot(daily %>% 
         group_by(Date) %>% 
         summarize(Listings = n())) +
  geom_line(aes(Date, Listings)) 

#ggsave("output/figure2.jpg")

# host revenue
host_revenue<-
  daily %>%
  filter(Date >= year_prior, Date <= End_date, Status == "R") %>%
  group_by(Airbnb_HID) %>%
  summarize(rev = sum(Price)) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev))) %>% 
  gather(`Top 1%`, `Top 5%`, `Top 10%`, key = "percentile", value = "value") %>% 
  mutate(percentile = factor(percentile, levels = c('Top 1%', 'Top 5%', 'Top 10%')))

figure3 <- ggplot(host_revenue)+
  geom_bar(mapping = aes(x = percentile, y = value, fill = percentile), stat = "identity")+
  theme_minimal()+
  scale_fill_manual("Percentile", values = alpha(c("lightblue", "blue", "darkblue"), .6))

#ggsave("output/figure3.jpg")


