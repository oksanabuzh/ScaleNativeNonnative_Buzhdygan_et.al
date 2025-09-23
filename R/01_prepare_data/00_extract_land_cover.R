# Purpose: Extract land cover data from Copernicus raster files (100m resolution)
# and calculate mean built-up and cropland cover fractions in buffers around
# sampling points (250-2000m).

library(readr)
library(terra)
library(sf)
library(exactextractr)

headers <- read_csv("data-raw/headers.csv")

# Convert latitude and longitude to spatial points
headers_sf <- st_as_sf(headers, coords = c("lon", "lat"), crs = 4326)

# Raster data is downloaded from https://land.copernicus.eu/en/products/global-dynamic-land-cover/copernicus-global-land-service-land-cover-100m-collection-3-epoch-2019-globe
# Loading raster data
builtup_rast <- rast(
  "data-raw/spatial/LandCover2019_raster_100m_global_yearly_version3/PROBAV_LC100_global_v3.0.1_2019-nrt_BuiltUp-CoverFraction-layer_EPSG-4326.tif"
)
cropland_rast <- rast(
  "data-raw/spatial/LandCover2019_raster_100m_global_yearly_version3/PROBAV_LC100_global_v3.0.1_2019-nrt_Crops-CoverFraction-layer_EPSG-4326.tif"
)

crs_raster <- crs(builtup_rast)

buffer_sizes <- c(250, 500, 1000, 2000)

# spatial reprojection
headers_proj <- st_transform(headers_sf, crs = crs_raster)

# Extracting values from rasters in the headers

result_headers <- headers

for (buf in buffer_sizes) {
  buffer <- st_buffer(headers_proj, dist = buf)
  builtup_mean <- exact_extract(builtup_rast, buffer, "mean")
  cropland_mean <- exact_extract(cropland_rast, buffer, "mean")
  result_headers[[paste0("builtup_", buf, "m")]] <- builtup_mean
  result_headers[[paste0("cropland_", buf, "m")]] <- cropland_mean
}

result_headers |> names()
result_headers |>
  select(builtup_250m:cropland_2000m) |>
  str()

write_csv(
  result_headers,
  "data-raw/headers_landcover_buffers.csv"
)
