library(raster)
library(sf)
library(tidyverse)

# Read data ---------------------------------------------------------------

# climate raster just to see the right projection
# climate_raster <- raster('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio1_1981-2010_V.2.1.tif')
roads <- raster::raster("data-raw/spatial/GRIP4_density_total/grip4_total_dens_m_km2.asc")
ukr_shape <- sf::st_read("data-raw/spatial/ukraine_shape/ukr_adm0.shp")

# check the projection
projection(roads)
projection(ukr_shape)

# Read header data -------------------------------------------------------
headers <- read_csv("data-raw/headers.csv") |>
  dplyr::select(series, lat, lon) |>
  # calculate mean per series
  summarize(
    lat = mean(lat, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE),
    .by = series
  )

# Extract location of our data points
location_roads <- st_as_sf(headers, coords = c("lon", "lat"), crs = crs(roads))

# Extract information from the raster
location_roads$roads <- raster::extract(roads, location_roads)

# Plot road data ----------------------------------------------------
# Crop the raster to the extent of the shapefile
ukr_roads <- raster::crop(roads, ukr_shape)
ukr_roads_tibble <- as.data.frame(ukr_roads, xy = TRUE) |> as_tibble()

plot_roads <- ggplot() +
  geom_raster(
    data = ukr_roads_tibble,
    aes(fill = grip4_total_dens_m_km2, x = x, y = y)
  ) +
  geom_sf(
    data = location_roads, aes(color = roads),
    size = .5
  ) +
  scale_fill_gradient(low = "grey", high = "black") +
  scale_color_viridis_c(option = "C", na.value = "green") +
  geom_sf(data = ukr_shape, fill = NA, color = "purple") +
  labs(fill = "Road density (map)", color = "Road density (extracted)") +
  labs(title = "Road density (map + extracted)") +
  theme(axis.title = element_blank())

ggsave("img/roads_map.png",
  plot = plot_roads,
  width = 16, height = 10, units = "cm", scale = 1.3)

# Add the new variables to the header data -------------------------------
headers_raw <- read_csv("data-raw/headers.csv")

new_vars <- headers_raw |>
  left_join(as.data.frame(location_roads) |> dplyr::select(series, roads),
    by = "series")
# we set the road density to 0 where it is NA because we checked the data points and
# they are in the middle of nowhere
new_vars$roads[is.na(new_vars$roads)] <- 0

headers <- headers_raw |>
  left_join(new_vars, by = "series")

# Save the new header data -----------------------------------------------
write_csv(headers, "data-raw/headers.csv")
