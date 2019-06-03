##################### MAPPING ##########################################

source("R/01_helper_functions.R")

city <- "plateau-mont-royal montreal"

streets <- 
  getbb(city) %>% 
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

streets <- 
  rbind(streets$osm_polygons %>% st_cast("LINESTRING"),streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32618) %>%
  select(osm_id, name, geometry)


##
figure1 <- 
#  tm_shape(st_buffer(city_border, 200)) +
#  tm_borders(lwd = 1) + 
  tm_shape(streets)+
  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(property)+
  tm_dots(col = "Listing_Type",
          scale = 4/3, 
          palette = get_brewer_pal("-Dark2", n = 3), 
          alpha = 0.6, 
          legend.show = FALSE, 
          size = "revenue", 
          title.size = "Revenu", 
          size.lim = c(0, 100000)) +
  tm_add_legend(type="symbol",
                col= get_brewer_pal("-Dark2", n = 3),
                labels=c("Entire Home", "Private Room", "Shared Room"),
                border.lwd = NA,
                alpha = 0.6,
                title="Listing Type")
