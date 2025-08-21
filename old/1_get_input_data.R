## ORYZA preparation pipeline for Myanmar
## Creates look up files for daily weather and soil for oryza simulation
## Data downloaded : Jan 1 2015 to Dec 31 2024 : Daily weather (min temp, max temp, dew points, wind speeds, irradiance) from NASA, and daily rainfalls from CHIRPS
## Data downloaded : Soil data from 6 depths including pH, SOC, total nitrogen, sand, silt, clay, and build density at 5, 15, 30, 60, 100, 200 cm
## Data are already downloaded and stored locally 
## The code covers processing of downloaded data to producing lookup files for weather and soil, acting as intermediate for making of oryza files (sol. and cli.)
## final outputs : two files : oryza_weather_final.csv/ mm_ayeyar_soil_lookup_out.csv
## Please Ignore the unit conversion for now. I will go over each and every one of them and correct them after checking the literature.

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else {
	path <- "C:/1_Research/yield_gap"
}
dir.create(path, FALSE, TRUE)
setwd(path)
dir.create("data/raw", FALSE, TRUE)
dir.create("data/intermediate", FALSE, TRUE)

## 1. Define area of interest (Ayeyarwady Region) and rice mask
## MIMU hosts admin boundaries of Myanmar in WGS84
## SPAM provides rice raster files (check if total rice or split irrigated/rainfed)
#admin_boundary_path <- "C:/1_Research/data_myanmar/admin_boundaries/state_regions/mmr_polbnda2_adm1_250k_mimu_1.shp"
#rice_mask_path     <- "C:/1_Research/data_myanmar/rice_mask_mm/spam2020_ha_rice_mmr.tif"

## Load Ayeyarwady boundary (EPSG:4326) and overlay with SPAM rice mask
#all_regions   <- st_read(admin_boundary_path)

aoi <- geodata::gadm("Myanmar", level=1, path="data/raw")
rice_rast <- geodata::crop_spam(crop="rice", var="area", "data/raw") |> terra::crop(my_region, mask=TRUE)
rice_rast <- terra::classify(rice_rast[[1]], cbind(NA, 0))



## 2. Create 0.5° grid and retain only cells containing rice
## Extract mean SPAM rice value within each grid cell
## Keep cells with mean rice area > 0

g <- terra::init(terra::rast(terra::ext(rice_rast), res=0.5), "cell")
grid_05 <- as.data.frame(g, xy=TRUE)
names(grid_05)[1:2] <- c("lon", "lat")


start_date <- as.Date("2015-01-01")
end_date   <- as.Date("2024-12-31")


wth1 <- geodata:::.worldclim_day(terra::ext(rice_rast), "data/raw")
wth2 <- geodata:::.worldclim_day(cbind(95.8, 17.3), "data/raw")



## Extract mean rice area per grid cell and filter for rice presence
grid_05$rice_area <- terra::extract(rice_rast[["rice_area_all"]], terra::as.polygons(g), fun = sum, na.rm = TRUE, ID=FALSE)[,1]
grid_rice <- grid_05[grid_05$rice_area > 0, ]

## 3. Read NASA converted data (2015–2024) and compute derived variables
## Radiation in MJ·m⁻²·day⁻¹ converted to kJ
## Vapor pressure calculated from dew point using FAO formula
## Temperatures in °C, wind speed at 10m in m/s


## Create output directory and write files
out_dir <- "ayeyar_only/oryza_ready_files"
dir.create(out_dir, FALSE, recursive = TRUE)

## Write weather file and station coordinates
write.csv(weather_full, file.path(out_dir, "oryza_weather_final.csv"), row.names = FALSE)
station_coords <- weather_full %>% select(STN, lon, lat) %>% distinct()
write.csv(station_coords, file.path(out_dir, "station_coordinates.csv"), row.names = FALSE)

## 6. Extract SoilGrids properties and aggregate to 0.5° using local GeoTIFFs
## Pattern: cm_mean.tif
## Loop over variables/depths, crop to region, extract mean per grid cell
#soil_dir <- "C:/1_Research/data_myanmar/soilgrid_tiff_mm"

vars <- c("bdod", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")
soil_5  <- lapply(vars, \(v) geodata::soil_world(v, 5, stat="mean"))


c("bdod", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")
soil_depths <- c("5cm", "15cm", "30cm", "60cm", "100cm", "200cm")

## Initialize soil dataframe with grid info
soil_df <- grid_rice %>% st_drop_geometry() %>% select(grid_id, lon, lat)

## Process each soil variable and depth
for (var in soil_vars) {
  for (depth in soil_depths) {
    tif_name <- paste0("Myanmar_", var, "_", depth, "_mean.tif")
    tif_path <- file.path(soil_dir, tif_name)
    if (!file.exists(tif_path)) next
    
    ## Load, project, and crop raster
    rast_var <- terra::rast(tif_path)
    rast_var <- terra::project(rast_var, crs(ayeyar_region))
    rast_crop <- terra::crop(rast_var, vect(ayeyar_region))
    
    ## Extract mean value per grid cell
    vals <- terra::extract(rast_crop, terra::vect(grid_rice), fun = mean, na.rm = TRUE)
    col_name <- paste0(var, "_", depth)
    
    ## Apply SoilGrids unit conversions. SON and SOC units : kg C/ha / kg N/ha
    conv <- 1
    if (var == "bdod") conv <- 1 / 100    # cg/cm3 → g/cm3 (kg/dm3)
    if (var %in% c("clay", "sand", "silt")) conv <- 1 / 1000   # g/kg → %
    if (var == "nitrogen") conv <- 1 / 100  # not correct : cg/kg → g/kg (approx)
    if (var == "soc") conv <- 1 / 10       # not correct : dg/kg → %
    if (var == "phh2o") conv <- 1 / 10      # pH*10 → pH
    
    soil_df[[col_name]] <- vals[, 2] * conv
  }
}

## Write soil lookup table
soil_out_dir <- "C:/1_Research/data_myanmar/ayeyar_only/oryza_ready_files"
if (!dir.exists(soil_out_dir)) dir.create(soil_out_dir, recursive = TRUE)
write.csv(soil_df, file.path(soil_out_dir, "mm_ayeyar_soil_lookup_out.csv"), row.names = FALSE)

