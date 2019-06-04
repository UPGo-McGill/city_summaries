##################### MAPPING ##########################################

source("R/01_helper_functions.R")

## Import street basemap
# Road level based on city size: value = secondary, tertiary or all (no value field)

streets <- 
  getbb(cityname) %>% 
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

streets <- 
  rbind(streets$osm_polygons %>% st_cast("LINESTRING"),streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32618) %>%
  select(osm_id, name, geometry)

## Map of Listing Type (Entire Home, Private Room or Shared Room) and Revenue
# For smaller cities or neighbourhoods may need to replace tm_shape(city) for:
##tm_shape(st_buffer(city, 200)) 

figure1 <- 
tm_shape(property)+
  tm_dots(scale = 0)+
tm_shape(streets)+
  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(city) +
  tm_borders(lwd = 1) + 
  tm_shape(property)+
  tm_dots(col = "Listing_Type",
          scale = 4/3, 
          palette = get_brewer_pal("-Dark2", n = 3), 
          alpha = 0.6, 
          size = "revenue", 
          title.size = "Revenue", 
          size.lim = c(0, 100000),
          legend.show = FALSE,
          legend.size.show = TRUE) +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE, legend.bg.alpha = 0.6, legend.bg.color = "white") +
  tm_add_legend(type="symbol",
                col= get_brewer_pal("-Dark2", n = 3),
                labels=c("Entire Home", "Private Room", "Shared Room"),
                border.lwd = NA,
                alpha = 1,
                title="Listing Type")+
  tm_compass()
   
tmap_save(figure1, "output/figure1.png", width = 2400, height = 2400 )
