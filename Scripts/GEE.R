# Load required packages
library(rgee)
library(sf)
library(dplyr)
library(geojsonio)
library(purrr)
library(ggplot2)
library(leaflet)
library(googledrive)

# === Step 1: Initialize rgee ===
ee_Initialize()  # Run this once per R session

# === Step 2: Load and prepare the polygon data ===
# Read a GeoPackage file containing polygons (e.g., ecosystems)
poly_sf <- st_read("Data/ecosystem-spatial-data/RLE_final_layers_5m.gpkg")

# Drop problematic date columns (EE does not support Date fields)
poly_sf_clean <- poly_sf[, !sapply(poly_sf, inherits, "Date")]

# Convert to Earth Engine FeatureCollection (limit to first 100 features for upload limits)
poly_ee <- sf_as_ee(poly_sf_clean[1:200, ])

# Center the map on the polygon
Map$centerObject(poly_ee, zoom = 7)

# Optional: visualize the polygon boundary
Map$addLayer(
  eeObject = poly_ee,
  visParams = list(
    color = "red",
    fillColor = "00000000",  # Transparent fill
    width = 2
  ),
  name = "Boundary"
)

# === Step 3: Load CHIRPS precipitation and MODIS ET ===

# CHIRPS daily precipitation — aggregate to monthly mean
chirps_img <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$
  filterDate("2022-01-01", "2022-01-31")$
  filterBounds(poly_ee)$
  mean()

# MODIS evapotranspiration (MOD16A2, 8-day composite, units: kg/m²/8day)
modis_et_img <- ee$ImageCollection("MODIS/061/MOD16A2")$
  filterDate("2022-01-01", "2022-01-31")$
  filterBounds(poly_ee)$
  mean()$
  multiply(0.1)$           # Apply scale factor
  select("ET")             # Select ET band

# Optional: add MODIS ET layer to Map for visualization
Map$addLayer(
  eeObject = modis_et_img,
  visParams = list(min = 0, max = 5, palette = c("white", "green")),
  name = "MODIS ET"
  )

# === Step 4: Zonal statistics using reduceRegions ===

# CHIRPS monthly precipitation mean per polygon
chirps_stat <- chirps_img$reduceRegions(
  collection = poly_ee,
  reducer = ee$Reducer$mean(),
  scale = 5000
)

# MODIS ET zonal mean per polygon (native resolution ≈463.3 m)
et_stat <- modis_et_img$reduceRegions(
  collection = poly_ee,
  reducer = ee$Reducer$mean(),
  scale = 500
)

# === Step 6: Export to Google Drive (optional) ===
task <- ee_table_to_drive(
  collection = et_stat,
  description = "et_stat_export",
  folder = "rgee_exports",
  fileFormat = "CSV"
)
task$start()

# === Step 7: Authenticate and download from Google Drive ===
# Ensure proper permissions
drive_auth()

# Find the file on Google Drive
file_info <- drive_find(pattern = "et_stat", type = "csv")[1, ]

# Define the local path you want to save to
local_dir <- "./Output"  # your target folder (change if needed)
# dir.create(local_dir, showWarnings = FALSE)  # create if it doesn't exist

# Download the file
drive_download(
  file = file_info,
  path = file.path(local_dir, file_info$name),
  overwrite = TRUE
)

# === Step 8: Read CSV and convert back to sf ===
# Read CSV
et_df <- read.csv(paste0(local_dir, "/", file_info$name))

# Convert .geo (GeoJSON strings) column to sf geometries
et_sf_list <- map(et_df$.geo, geojson_sf)
et_sf <- do.call(rbind, et_sf_list)

# Reattach attribute columns
et_sf <- cbind(et_sf, et_df[ , !(names(et_df) %in% ".geo")])

# === Step 9: Data summary and visualization ===
et_sf$mean <- as.numeric(et_sf$mean)

# Quick summary and histogram
summary(et_sf$mean)
hist(et_sf$mean)

# Static map using ggplot2
ggplot(et_sf) +
  geom_sf(aes(fill = mean)) +
  coord_sf(xlim = c(146, 146.5), ylim = c(-42.1, -41.6)) +  # Adjust to your region
  scale_fill_viridis_c(
    limits = c(summary(et_sf$mean)[2], summary(et_sf$mean)[5]),
    oob = scales::squish  # You can try `rescale_none`, `squish` or `censor` instead
  ) +
  theme_minimal()

# Alternative: interactive map using leaflet
pal <- colorNumeric("YlGnBu", domain = et_sf$mean)

leaflet(et_sf) %>%
  addTiles() %>%
  addPolygons(
    color = "black",
    weight = 1,
    fillOpacity = 0.7,
    fillColor = ~pal(mean),
    label = ~paste0("ET: ", round(mean, 2))
  ) %>%
  addLegend("bottomright", pal = pal, values = ~mean, title = "Mean ET")
