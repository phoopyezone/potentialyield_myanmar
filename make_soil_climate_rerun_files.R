## make_oryza_file_pipeline.R
##
## Automates generation of multi-year weather (*.cli) and soil (*.sol) files for ORYZA
## Reads processed weather and soil data, creates individual files per station/grid
## Generates reruns.rer file for batch simulation runs

library(data.table)  # Fast data processing
library(dplyr)       # Data manipulation

## ============================================================================
## 1. Input and output paths
## ============================================================================

# Define simulation directory structure
root_dir       <- "C:/1_Research/simulation_main"
make_dir       <- file.path(root_dir, "make_files")
weather_file   <- file.path(make_dir, "oryza_weather_final.csv")      # Daily weather data
soil_lookup    <- file.path(make_dir, "mm_ayeyar_soil_lookup_wide.csv") # Soil properties by depth

weather_dir    <- file.path(root_dir, "weather")  # Output .cli files
soil_dir       <- file.path(root_dir, "soils")    # Output .sol files  
rerun_file     <- file.path(root_dir, "reruns.rer") # Batch run configuration

## Create output directories if they don't exist
if (!dir.exists(weather_dir)) dir.create(weather_dir, recursive = TRUE)
if (!dir.exists(soil_dir))    dir.create(soil_dir, recursive = TRUE)

## ============================================================================
## 2. Load lookup tables
## ============================================================================

# Load daily weather data (STN, YEAR, DAY, SRAD, TMIN, TMAX, VAP, WIND, RAIN, lon, lat)
weather <- fread(weather_file)

## Clean rainfall data - remove negative values (missing codes) and set to zero
if ("RAIN" %in% names(weather)) {
  weather$RAIN <- ifelse(weather$RAIN < 0, 0, weather$RAIN)
}

# Load soil properties by depth (grid_id, lon, lat, bdod_5cm, clay_5cm, etc.)
# Values already converted to conventional units (bulk density g/cm3, texture %, SOC %, pH)
soil <- fread(soil_lookup)

## Create station coordinate mapping
# Sort soil by coordinates and assign station numbers (matches weather STN assignment)
soil_match <- soil %>% arrange(lon, lat) %>% mutate(STN = row_number())

## Station coordinates for climate file headers (altitude set to 0m)
station_coords <- soil_match %>% select(STN, lon, lat) %>% mutate(alt = 0)

## Soil data with station IDs for soil file generation
soil_station <- soil_match

## ============================================================================
## 3. Helper functions for writing climate and soil files
## ============================================================================

## Format single weather record as comma-separated line for .cli file
format_weather_line <- function(stn, year, day, srad, tmin, tmax, vap, wind, rain) {
  sprintf("%d,%d,%d,%.0f,%.1f,%.1f,%.2f,%.2f,%.1f", stn, year, day, srad, tmin, tmax, vap, wind, rain)
}

## Create ORYZA climate file header with station info and column descriptions
cli_header <- function(stn_id, lon, lat, alt) {
  paste0(c(
    "*-----------------------------------------------------------",
    sprintf("*  Station Name: GRID_%03d", stn_id),
    "*  Author      : Automated pipeline", 
    "*  Source      : ORYZA climate generator",
    "*",
    "*  Comments    : Automatically generated weather file",
    sprintf("*  Longitude:  %7.2f E   Latitude:  %6.2f N   Altitude: %.1f m", lon, lat, alt),
    "*",
    "*  Column    Daily Value",
    "*     1      Station number",
    "*     2      Year",
    "*     3      Day",
    "*     4      irradiance         kJ m-2 d-1",
    "*     5      min temperature    oC",
    "*     6      max temperature    oC",
    "*     7      vapor pressure     kPa",
    "*     8      mean wind speed    m s-1",
    "*     9      precipitation      mm d-1",
    "*-----------------------------------------------------------"
  ), collapse = "\n")
}

## Write complete climate file (.cli) for one station
write_cli_file <- function(stn_id, wdata, coord_row) {
  lon <- coord_row$lon
  lat <- coord_row$lat
  alt <- coord_row$alt
  
  # Create file header and coordinate line
  header <- cli_header(stn_id, lon, lat, alt)
  coord_line <- sprintf("%.5f,%.5f,%.1f,0.00,0.00", lon, lat, alt)
  
  # Format all daily weather records
  daily_lines <- mapply(format_weather_line,
                        stn = wdata$STN,
                        year = wdata$YEAR,
                        day  = wdata$DAY,
                        srad = wdata$SRAD,
                        tmin = wdata$TMIN,
                        tmax = wdata$TMAX,
                        vap  = wdata$VAP,
                        wind = wdata$WIND,
                        rain = wdata$RAIN,
                        USE.NAMES = FALSE)
  
  # Write complete climate file
  out_path <- file.path(weather_dir, sprintf("STN%d.cli", stn_id))
  writeLines(c(header, coord_line, daily_lines), out_path)
}

## Replace soil properties in template with site-specific values
replace_soil_properties <- function(template_lines, nl, tkl, clay, sand, bd, ph) {
  ## Number of soil layers
  template_lines <- sub("NL = [0-9]+", sprintf("NL = %d", nl), template_lines)
  
  ## Layer thickness (m)
  tkl_str <- paste0(sprintf("%.2f", tkl), collapse = ",")
  template_lines <- sub("TKL = [^!]+", sprintf("TKL = %s      ! Thickness of each soil layer (m)", tkl_str), template_lines)
  
  ## Clay content (fraction)
  clay_str <- paste0(sprintf("%.2f", clay), collapse = ",")
  template_lines <- sub("CLAYX = [^!]+", sprintf("CLAYX = %s      !soil clay content, fraction", clay_str), template_lines)
  
  ## Sand content (fraction)
  sand_str <- paste0(sprintf("%.2f", sand), collapse = ",")
  template_lines <- sub("SANDX = [^!]+", sprintf("SANDX = %s      !soil sand content, fraction", sand_str), template_lines)
  
  ## Bulk density (g/cm3)
  bd_str   <- paste0(sprintf("%.3f", bd), collapse = ",")
  template_lines <- sub("BD = [^!]+", sprintf("BD = %s     !soil bulk density (g/cm3)", bd_str), template_lines)
  
  ## Soil pH (if available)
  if (!is.null(ph)) {
    ph_str <- paste0(sprintf("%.1f", ph), collapse = ",")
    # Find and replace existing pH line or insert new one
    ph_idx <- grep("PHX", template_lines)
    if (length(ph_idx) > 0) {
      template_lines[ph_idx[1]] <- sprintf("PHX = %s      !soil pH", ph_str)
    } else {
      # Insert pH after bulk density line
      insert_idx <- grep("BD =", template_lines)
      if (length(insert_idx) > 0) {
        template_lines <- append(template_lines, sprintf("PHX = %s      !soil pH", ph_str), after = insert_idx)
      }
    }
  }
  return(template_lines)
}

## Write complete soil file (.sol) for one station
write_sol_file <- function(stn_id, soil_row, template_lines) {
  # Define 6 soil layers with cumulative depths: 5, 15, 30, 60, 100, 200cm
  nl  <- 6
  tkl <- c(0.05, 0.10, 0.15, 0.30, 0.40, 1.00)  # Layer thickness (m)
  
  ## Extract and convert soil properties to ORYZA units
  # Clay/sand: % to fraction (divide by 100 - but data already in %, so divide by 1000 from g/kg)
  clay_vals <- as.numeric(soil_row[paste0("clay_", c("5cm","15cm","30cm","60cm","100cm","200cm"))]) / 1000
  sand_vals <- as.numeric(soil_row[paste0("sand_", c("5cm","15cm","30cm","60cm","100cm","200cm"))]) / 1000
  
  # Bulk density: cg/cm3 to g/cm3 (divide by 100)
  bd_vals   <- as.numeric(soil_row[paste0("bdod_", c("5cm","15cm","30cm","60cm","100cm","200cm"))]) / 100
  
  # pH: pH*10 to pH (divide by 10) if available
  ph_vals   <- NULL
  if (any(grepl("phh2o_5cm", names(soil_row)))) {
    ph_vals <- as.numeric(soil_row[paste0("phh2o_", c("5cm","15cm","30cm","60cm","100cm","200cm"))]) / 10
  }
  
  # Replace template values with site-specific data
  new_lines <- replace_soil_properties(template_lines, nl, tkl, clay_vals, sand_vals, bd_vals, ph_vals)
  
  # Update soil code identifier
  new_lines <- sub("SCODE = '[^']+'", sprintf("SCODE = '%s'", sprintf("GRID_%03d", stn_id)), new_lines)
  
  # Write soil file
  out_path <- file.path(soil_dir, sprintf("GRID_%03d.sol", stn_id))
  writeLines(new_lines, out_path)
}

## ============================================================================
## 4. Generate climate and soil files
## ============================================================================

# Load soil template file (standard.sol)
template_sol_path <- file.path(root_dir, "irri_files", "standard.sol")
if (!file.exists(template_sol_path)) {
  # Fallback location if not found
  template_sol_path <- "/home/oai/share/standard.sol"
}
template_sol_lines <- readLines(template_sol_path)

## Process each station: create both .cli and .sol files
unique_stations <- sort(unique(weather$STN))
for (stn in unique_stations) {
  # Extract weather data for this station
  wdata <- weather[STN == stn]
  coord_row <- station_coords %>% filter(STN == stn) %>% slice(1)
  
  # Generate climate file
  write_cli_file(stn, wdata, coord_row)
  
  # Generate soil file using corresponding soil data
  srow <- soil_station %>% filter(STN == stn) %>% slice(1)
  if (nrow(srow) > 0) {
    write_sol_file(stn, srow, template_sol_lines)
  } else {
    warning(sprintf("No soil lookup entry matched station %d; soil file not created", stn))
  }
}

## ============================================================================
## 5. Create reruns file for batch processing
## ============================================================================

# Initialize reruns.rer file with header
rer_lines <- c(
  "*==============================================*",
  "* RERUNS – one run per grid/station combination *",
  "*==============================================*",
  ""
)

# Add one rerun configuration per station
set_idx <- 1
for (stn in unique_stations) {
  # Windows-style path for soil file
  soil_path_win <- sprintf("C:\\1_Research\\simulation_main\\soils\\GRID_%03d.sol", stn)
  
  # Add rerun configuration
  rer_lines <- c(rer_lines,
                 sprintf("*Rerun set %d", set_idx),
                 sprintf("FILEI2 = '%s'", soil_path_win),  # Soil file path
                 "CNTR   = 'STN'",                         # Control variable
                 sprintf("ISTN   = %d", stn),              # Station number
                 "")
  set_idx <- set_idx + 1
}

# Write reruns file
writeLines(rer_lines, rerun_file)

# Summary message
cat(sprintf("Generated %d climate files and %d soil files. Rerun file saved to %s\n",
            length(unique_stations), length(unique_stations), rerun_file))