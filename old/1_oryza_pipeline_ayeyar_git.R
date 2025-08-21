## ORYZA preparation pipeline for Ayeyarwady Region (Myanmar)
## Creates look up files for daily weather and soil for oryza simulation
## Data downloaded : Jan 1 2015 to Dec 31 2024 : Daily weather (min temp, max temp, dew points, wind speeds, irradiance) from NASA, and daily rainfalls from CHIRPS
## Data downloaded : Soil data from 6 depths including pH, SOC, total nitrogen, sand, silt, clay, and build density at 5, 15, 30, 60, 100, 200 cm
## Data are already downloaded and stored locally 
## The code covers processing of downloaded data to producing lookup files for weather and soil, acting as intermediate for making of oryza files (sol. and cli.)
## final outputs : two files : oryza_weather_final.csv/ mm_ayeyar_soil_lookup_out.csv
## Please Ignore the unit conversion for now. I will go over each and every one of them and correct them after checking the literature.


this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data"
} else {
	path <- "C:/1_Research/yield_gap"
}
setwd(path)

#library(sf)          # Spatial vector data
#library(terra)       # Raster data processing
#library(data.table)  # Fast data manipulation
#library(dplyr)       # Data wrangling
#library(lubridate)   # Date handling

## 1. Define area of interest (Ayeyarwady Region) and rice mask
## MIMU hosts admin boundaries of Myanmar in WGS84
## SPAM provides rice raster files (check if total rice or split irrigated/rainfed)
admin_boundary_path <- "data_myanmar/admin_boundaries/state_regions/mmr_polbnda2_adm1_250k_mimu_1.shp"
rice_mask_path     <- "data_myanmar/rice_mask_mm/spam2020_ha_rice_mmr.tif"

## Load Ayeyarwady boundary (EPSG:4326) and overlay with SPAM rice mask
all_regions   <- sf::st_read(admin_boundary_path)
pattern       <- paste(c("Ayeyawady", "Ayeyarwady", "Ayeyarwady Region"), collapse = "|")
ayeyar_region <- all_regions |>
  dplyr::filter(dplyr::if_any(where(is.character), ~ grepl(pattern, ., ignore.case = TRUE)))
rice_rast     <- terra::rast(rice_mask_path)

# Ensure rice raster matches boundary projection
rice_rast     <- terra::project(rice_rast, ayeyar_region)

## Crop rice raster to region first for efficiency (avoids processing entire country)
rice_rast_crop <- terra::crop(rice_rast, ayeyar_region)

## 2. Create 0.5° grid and retain only cells containing rice
## Extract mean SPAM rice value within each grid cell
## Keep cells with mean rice area > 0
bbox <- sf::st_bbox(ayeyar_region)
cell_size <- 0.5

## Create regular grid and assign unique IDs
grid_sfc <- sf::st_make_grid(ayeyar_region, cellsize = cell_size, square = TRUE)
grid_05  <- sf::st_as_sf(grid_sfc)
grid_05$grid_id <- paste0("GRID_", sprintf("%03d", seq_len(nrow(grid_05))))

## Calculate grid cell centroids and coordinates
centroids <- sf::st_centroid(grid_05)
coord_mat <- sf::st_coordinates(centroids)
grid_05$lon <- coord_mat[,1]
grid_05$lat <- coord_mat[,2]

## Extract mean rice area per grid cell and filter for rice presence
grid_vect <- terra::vect(grid_05)
rice_vals <- terra::extract(rice_rast_crop, grid_vect, fun = mean, na.rm = TRUE)
has_rice  <- rice_vals[, 2] > 0  # Column 2 contains mean values
grid_rice <- grid_05[has_rice, ]

## 3. Read NASA converted data (2015–2024) and compute derived variables
## Radiation in MJ·m⁻²·day⁻¹ converted to kJ
## Vapor pressure calculated from dew point using FAO formula
## Temperatures in °C, wind speed at 10m in m/s

start_date <- as.Date("2015-01-01")
end_date   <- as.Date("2024-12-31")

# Directory with converted NASA files (srad and vap already in ORYZA units)
nasa_dir <- "data_myanmar/nasa_mm/converted"

# Solar radiation already converted to kJ/m²/day
nasa_srad <- data.table::fread(file.path(nasa_dir, "nasa_irradiance.csv"))
nasa_srad <- nasa_srad[, .(LON, LAT, YEAR, DOY, SRAD = srad)]

# Vapor pressure (kPa) calculated from dew point using FAO method
nasa_dew <- data.table::fread(file.path(nasa_dir, "nasa_dew_point.csv"))
nasa_vap <- nasa_dew[, .(LON, LAT, YEAR, DOY, VAP = vap)]

# Min/max air temperatures (°C) - already processed
nasa_tmin <- data.table::fread(file.path(nasa_dir, "nasa_temp_min.csv"))[, .(LON, LAT, YEAR, DOY, TMIN = T2M_MIN)]
nasa_tmax <- data.table::fread(file.path(nasa_dir, "nasa_temp_max.csv"))[, .(LON, LAT, YEAR, DOY, TMAX = T2M_MAX)]

## Use 10m wind speeds (and conversion to 2m)
nasa_wind <- data.table::fread(file.path(nasa_dir, "nasa_wind_speed.csv"))[, .(LON, LAT, YEAR, DOY, WIND = WS10M)]
nasa_wind$WIND <- nasa_wind$WIND * 0.78

## Merge all NASA variables by coordinates and date
nasa_all <- Reduce(function(x, y) merge(x, y, by = c("LON", "LAT", "YEAR", "DOY"), all = TRUE),
                   list(nasa_srad, nasa_vap, nasa_tmin, nasa_tmax, nasa_wind))

## Assign NASA records to nearest 0.5° rice grid cells
nasa_sf <- sf::st_as_sf(nasa_all, coords = c("LON", "LAT"), crs = 4326)
nasa_joined <- sf::st_join(nasa_sf, grid_rice[, c("grid_id", "lon", "lat")], left = FALSE) |>
  sf::st_drop_geometry()

## Average weather variables by grid cell, year, and day
weather_base <- nasa_joined |>
  dplyr::group_by(grid_id, YEAR, DOY) |>
  dplyr::summarise(
    SRAD = mean(SRAD, na.rm = TRUE),
    TMIN = mean(TMIN, na.rm = TRUE),
    TMAX = mean(TMAX, na.rm = TRUE),
    VAP  = mean(VAP,  na.rm = TRUE),
    WIND = mean(WIND, na.rm = TRUE),
    lon  = dplyr::first(lon),
    lat  = dplyr::first(lat),
    .groups = "drop"
  )

## 4. Retrieve daily CHIRPS precipitation and aggregate to 0.5° grid
## Use locally downloaded CHIRPS data with lon/lat/date/rainfall(mm)
## Filter to Ayeyarwady bbox, assign to grid cells, average per cell/day
chirps_file <- "data_myanmar/chrips_mm/raw_data/chirps_myanmar_raw_2015_2024_combined.csv"
chirps_raw  <- data.table::fread(chirps_file)

## Convert dates and extract year/day-of-year
chirps_raw[, date := as.Date(date)]
chirps_raw[, YEAR := lubridate::year(date)]
chirps_raw[, DOY  := lubridate::yday(date)]

## Filter to Ayeyarwady bounding box (with 1° buffer)
lon_range <- range(sf::st_coordinates(ayeyar_region)[,1])
lat_range <- range(sf::st_coordinates(ayeyar_region)[,2])
chirps_clip <- chirps_raw[dplyr::between(lon, lon_range[1] - 1, lon_range[2] + 1) &
                            dplyr::between(lat, lat_range[1] - 1, lat_range[2] + 1)]

#chirps_clip <- chirps_raw[(lon >= lon_range[1] - 1) & (lon <= lon_range[2] + 1) &
#                            (lat >= lat_range[1] - 1) & (lat <= lat_range[2] + 1)]


## Remove negative rainfall values (CHIRPS missing codes) - set to NA
chirps_clip[chirps < 0, chirps := NA_real_]

## Assign CHIRPS points to 0.5° grid cells by rounding coordinates
chirps_clip[, grid_lon := round(lon / 0.5) * 0.5]
chirps_clip[, grid_lat := round(lat / 0.5) * 0.5]
chirps_clip[, grid_key := paste0(grid_lon, "_", grid_lat)]

## Map rounded coordinates to grid_id using grid centroids
grid_key_map <- grid_rice |> sf::st_drop_geometry() |>
  dplyr::mutate(grid_key = paste0(round(lon / 0.5) * 0.5, "_", round(lat / 0.5) * 0.5)) |>
  dplyr::select(grid_id, grid_key)
chirps_joined <- merge(chirps_clip, grid_key_map, by = "grid_key", all.x = TRUE, allow.cartesian = TRUE)

## Aggregate rainfall by grid_id, year, day
prec_agg <- chirps_joined[!is.na(grid_id), .(RAIN = mean(chirps, na.rm = TRUE)), by = .(grid_id, YEAR, DOY)]

## 5. Merge meteorology and precipitation, assign station numbers
weather_full <- weather_base |>
  dplyr::left_join(prec_agg, by = c("grid_id", "YEAR", "DOY")) |>
  dplyr::arrange(grid_id, YEAR, DOY)

## Replace missing rainfall with zero (no rain assumption)
weather_full$RAIN[is.na(weather_full$RAIN)] <- 0

## Create station IDs and format for ORYZA
weather_full <- weather_full |>
  dplyr::mutate(STN = as.integer(factor(grid_id, levels = sort(unique(grid_id))))) |>
  dplyr::select(STN, YEAR, DAY = DOY, SRAD, TMIN, TMAX, VAP, WIND, RAIN, lon, lat)



## Create output directory and write files
out_dir <- "data_myanmar/ayeyar_only/oryza_ready_files"
dir.create(out_dir, FALSE, recursive = TRUE)

## Write weather file and station coordinates
write.csv(weather_full, file.path(out_dir, "oryza_weather_final.csv"), row.names = FALSE)
station_coords <- weather_full |> dplyr::select(STN, lon, lat) |> dplyr::distinct()
#station_coords <- unique(weather_full[, c("STN", "lon", "lat")])

write.csv(station_coords, file.path(out_dir, "station_coordinates.csv"), row.names = FALSE)

## 6. Extract SoilGrids properties and aggregate to 0.5° using local GeoTIFFs
## Pattern: cm_mean.tif
## Loop over variables/depths, crop to region, extract mean per grid cell
soil_dir <- "data_myanmar/soilgrid_tiff_mm"
soil_vars  <- c("bdod", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")
soil_depths <- c("5cm", "15cm", "30cm", "60cm", "100cm", "200cm")

## Initialize soil dataframe with grid info
soil_df <- grid_rice |> sf::st_drop_geometry() |> dplyr::select(grid_id, lon, lat)

## Process each soil variable and depth
for (var in soil_vars) {
  for (depth in soil_depths) {
    tif_name <- paste0("Myanmar_", var, "_", depth, "_mean.tif")
    tif_path <- file.path(soil_dir, tif_name)
    if (!file.exists(tif_path)) next
    
    ## Load, project, and crop raster
    rast_var <- terra::rast(tif_path)
    rast_var <- terra::project(rast_var, ayeyar_region)
    rast_crop <- terra::crop(rast_var, ayeyar_region)
    
    ## Extract mean value per grid cell
    vals <- terra::extract(rast_crop, terra::vect(grid_rice), fun = mean, na.rm = TRUE)
    col_name <- paste0(var, "_", depth)
    
    ## Apply SoilGrids unit conversions
    conv <- 1
    if (var == "bdod") conv <- 1 / 100    # cg/cm3 → g/cm3 (kg/dm3)
    if (var %in% c("clay", "sand", "silt")) conv <- 1 / 10   # g/kg → %
    if (var == "nitrogen") conv <- 1 / 100  # cg/kg → g/kg (approx)
    if (var == "soc") conv <- 1 / 100       # dg/kg → %
    if (var == "phh2o") conv <- 1 / 10      # pH*10 → pH
    
    soil_df[[col_name]] <- vals[, 2] * conv
  }
}

## Write soil lookup table
soil_out_dir <- "data_myanmar/ayeyar_only/oryza_ready_files"
dir.create(soil_out_dir, FALSE, recursive = TRUE)
write.csv(soil_df, file.path(soil_out_dir, "mm_ayeyar_soil_lookup_out.csv"), row.names = FALSE)

