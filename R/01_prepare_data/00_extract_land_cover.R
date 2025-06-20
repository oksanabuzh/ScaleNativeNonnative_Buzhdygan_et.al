library(tidyverse)
library(terra)
library(sf)
library(exactextractr)

hea <- read_csv("data-raw/headers.csv")
hea |> str()

# Spatial points
hea_sf <- st_as_sf(hea, coords = c("lon", "lat"), crs = 4326)

# Raster data is downloaded from https://land.copernicus.eu/en/products/global-dynamic-land-cover/copernicus-global-land-service-land-cover-100m-collection-3-epoch-2019-globe
# Loading raster data
builtup_rast <- rast("data/LandCover2019_raster_100m_global_yearly_version3/PROBAV_LC100_global_v3.0.1_2019-nrt_BuiltUp-CoverFraction-layer_EPSG-4326.tif")
cropland_rast <- rast("data/LandCover2019_raster_100m_global_yearly_version3/PROBAV_LC100_global_v3.0.1_2019-nrt_Crops-CoverFraction-layer_EPSG-4326.tif")

crs_raster <- crs(builtup_rast)

buffer_sizes <- c(250, 500, 1000, 2000)

# spatial reprojection
hea_proj <- st_transform(hea_sf, crs = crs_raster)

# Extracting values from rasters in the header

result_hea <- hea

for (buf in buffer_sizes) {
  buffer <- st_buffer(hea_proj, dist = buf)
  builtup_mean <- exact_extract(builtup_rast, buffer, "mean")
  cropland_mean <- exact_extract(cropland_rast, buffer, "mean")
  result_hea[[paste0("builtup_", buf, "m")]] <- builtup_mean
  result_hea[[paste0("cropland_", buf, "m")]] <- cropland_mean
}

result_hea |> names()
result_hea |>
  select(builtup_250m:cropland_2000m) |>
  str()

write.csv(result_hea, "header_landcover_buffers.csv", row.names = FALSE)
