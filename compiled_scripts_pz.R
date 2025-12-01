##### START OF: 1_get_wth_soil_data.R #####

# NASA POWER-based implementation that reproduces the same


suppressPackageStartupMessages({
  library(nasapower)
  library(terra)
  library(dplyr)
  library(lubridate)
})

# --- PATHS (fixed for local environment) ---
path <- "G:/My Drive/simulation_data/v2"

dir.create(path, FALSE, TRUE)
setwd(path)
raw_dir <- file.path("data", "raw")
power_dir <- file.path(raw_dir, "weather", "power")
dir.create(power_dir, recursive = TRUE, showWarnings = FALSE)

# --- CONFIG ---
vars <- c("ALLSKY_SFC_SW_DWN", "T2M_MIN", "T2M_MAX", "WS2M", "T2MDEW", "PRECTOTCORR")
years <- 1995:2024
ext <- terra::ext(91.5, 101.5, 8, 29)  # xmin, xmax, ymin, ymax
bbox <- c(ymin(ext), xmin(ext), ymax(ext), xmax(ext))  # lat_min, lon_min, lat_max, lon_max
# NOTE: nasapower::get_power(bbox=) expects c(lat_min, lon_min, lat_max, lon_max)

# Build the target grid (POWER native grid is 0.5-degree)
lon_seq <- seq(xmin(ext), xmax(ext), by = 0.5)
lat_seq <- seq(ymin(ext), ymax(ext), by = 0.5)
lat_seq_desc <- sort(lat_seq, decreasing = TRUE)

# Helper: turn a POWER dataframe for a single day into a raster layer
.make_layer <- function(df, value_col){
  r <- terra::rast(ncols = length(lon_seq), nrows = length(lat_seq_desc),
                   xmin = xmin(ext), xmax = xmax(ext), ymin = ymin(ext), ymax = ymax(ext),
                   crs = "EPSG:4326")
  df_ord <- df |>
    dplyr::arrange(dplyr::desc(LAT), LON)
  terra::values(r) <- df_ord[[value_col]]
  r
}

# Helper: fetch a single year's worth for one parameter and return a SpatRaster time series
.fetch_year <- function(parm, yr){
  d1 <- paste0(yr, "-01-01"); d2 <- paste0(yr, "-12-31")
  dat <- nasapower::get_power(
    community = "AG",
    temporal_average = "DAILY",
    pars = parm,
    bbox = bbox,
    dates = c(d1, d2)
  )
  dat <- dplyr::rename(dat, DATE = YYYYMMDD)
  dat$DATE <- as.Date(dat$DATE)
  days <- sort(unique(dat$DATE))
  layers <- vector("list", length(days))
  for (i in seq_along(days)){
    dd <- dat[dat$DATE == days[i], c("LAT", "LON", parm)]
    layers[[i]] <- .make_layer(dd, parm)
  }
  r <- terra::rast(layers)
  terra::time(r) <- days
  names(r) <- paste0(parm, "_", format(days, "%Y%j"))
  r
}

# Helper: load if exists; otherwise fetch all years and write a single NetCDF per variable
.get_or_load_var <- function(parm){
  fn_raw <- file.path(power_dir, sprintf("%s-1995_2024-91.5x101.5x8x29.nc", parm))
  if (file.exists(fn_raw)){
    message("Found existing ", basename(fn_raw), "; loading…")
    return(terra::rast(fn_raw))
  }
  message("Downloading ", parm)
  rs <- lapply(years, function(yy) .fetch_year(parm, yy))
  R <- terra::rast(rs)
  terra::writeCDF(R, fn_raw, varname = parm, longname = parm, overwrite = TRUE)
  R
}

# -----------------------------------------------------------------------------
# 1) Download (or load existing) raw POWER variables
# -----------------------------------------------------------------------------
R_ALLSKY   <- .get_or_load_var("ALLSKY_SFC_SW_DWN")  # kWh m-2 day-1 (POWER Daily)
R_TMIN     <- .get_or_load_var("T2M_MIN")            # °C
R_TMAX     <- .get_or_load_var("T2M_MAX")            # °C
R_WIND     <- .get_or_load_var("WS2M")               # m s-1
R_TDEW     <- .get_or_load_var("T2MDEW")             # °C
R_PREC     <- .get_or_load_var("PRECTOTCORR")        # mm day-1

# -----------------------------------------------------------------------------
# 2) Create (or load) derived products with the same names as original pipeline
# -----------------------------------------------------------------------------
fn_rad <- file.path(power_dir, "radiation-1995_2024-91.5x101.5x8x29.nc")
if (!file.exists(fn_rad)){
  rrad <- R_ALLSKY * 3600  # kWh m-2 d-1 -> kJ m-2 d-1
  terra::writeCDF(rrad, fn_rad, varname = "radiation", longname = "radiation", overwrite = TRUE)
} else {
  rrad <- terra::rast(fn_rad)
}

fn_vap <- file.path(power_dir, "vapr-1995_2024-91.5x101.5x8x29.nc")
if (!file.exists(fn_vap)){
  vapr <- 6.112 * exp((17.67 * R_TDEW) / (R_TDEW + 243.5)) / 10  # hPa->kPa
  terra::writeCDF(vapr, fn_vap, varname = "VAPR", longname = "vapor pressure", overwrite = TRUE)
} else {
  vapr <- terra::rast(fn_vap)
}

fn_rain <- file.path(power_dir, "rain-1995_2024-91.5x101.5x8x29.nc")
if (!file.exists(fn_rain)){
  rain <- R_PREC  # already mm d-1
  terra::writeCDF(rain, fn_rain, varname = "RAIN", longname = "precipitation", unit = "mm", overwrite = TRUE)
} else {
  rain <- terra::rast(fn_rain)
}


### elevation (skip if exists)
elv_tif <- file.path(raw_dir, "elevation.tif")
if (!file.exists(elv_tif)){
  elv <- geodata::elevation_30s("Myanmar", path = raw_dir)
  elv <- terra::resample(elv, vapr, "average", filename = elv_tif)
} else {
  elv <- terra::rast(elv_tif)
}


### soil (skip if exists)
soil_tif <- file.path(raw_dir, "soil.tif")
soil_agg_tif <- file.path(raw_dir, "soil_agg.tif")
if (!file.exists(soil_agg_tif)){
  if (!file.exists(soil_tif)){
    ext <- terra::ext(91.5, 101.5,  8, 29)
    vars <- c("bdod", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")
    depths <- c(5, 15, 30)
    soil <- geodata::soil_world(vars, depths, stat = "mean", vsi = TRUE)
    soil <- terra::crop(soil, ext, filename = soil_tif)
  } else {
    soil <- terra::rast(soil_tif)
  }
  soil[["clay_0-5cm"]] <- 100 - (soil[["sand_0-5cm"]] + soil[["silt_0-5cm"]])
  soil2 <- terra::resample(soil, elv, "average", filename = soil_agg_tif, overwrite = TRUE)
} else {
  soil2 <- terra::rast(soil_agg_tif)
}


## cells (skip if exists)
cells_rds <- file.path("data", "cells.rds")
if (!file.exists(cells_rds)){
  aoi <- geodata::gadm("Myanmar", level = 1, path = raw_dir)
  r <- rast(elv)
  r <- mask(init(r, "cell"), aoi, touches = TRUE)
  rice_rast <- geodata::crop_spam(crop = "rice", var = "area", raw_dir) |> terra::crop(aoi, mask = TRUE)
  rice_rast <- resample(rice_rast[[1]], r, "sum") > 0
  r <- mask(r, rice_rast, maskvalue = FALSE)
  cells <- data.frame(r)[,1]
  
  xy <- data.frame(xyFromCell(r, cells))
  relv <- rast(elv_tif)
  xy$elevation <- round(relv[cells])
  xy$cell <- cells
  
  saveRDS(xy, cells_rds)
}

##### END OF: 1_get_wth_soil_data.R #####


##### START OF: 2_make_soil_files.R #####

## make_oryza_file_pipeline.R
##
## Automates generation of multi-year weather (*.cli) and soil (*.sol) files for ORYZA
## Reads processed weather and soil data, creates individual files per station/grid
## Generates reruns.rer file for batch simulation runs

# --- PATH (fixed for local environment) ---
path <- "G:/My Drive/simulation_data/v2"
setwd(path)

## ============================================================================
## 1. Input and output paths

soil_dir  <- file.path(path, "oryza", "input", "soils")    # Output .sol files (match existing layout)
dir.create(soil_dir, FALSE, recursive = TRUE)


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
    ph_idx <- grep("PHX", template_lines)
    if (length(ph_idx) > 0) {
      template_lines[ph_idx[1]] <- sprintf("PHX = %s      !soil pH", ph_str)
    } else {
      insert_idx <- grep("BD =", template_lines)
      if (length(insert_idx) > 0) {
        template_lines <- append(template_lines, sprintf("PHX = %s      !soil pH", ph_str), after = insert_idx)
      }
    }
  }
  return(template_lines)
}

## Write complete soil file (.sol) for one station
write_sol_file <- function(stn_id, soil_row, template_lines, tkl=c(0.05, 0.10, 0.15, 0.30, 0.40, 1.00), outpath) {
  nl <- length(tkl)
  depths <- c("0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm")[1:nl]
  clay_vals <- soil_row[paste0("clay_", depths)] / 1000
  sand_vals <- soil_row[paste0("sand_", depths)] / 1000
  bd_vals  <- soil_row[paste0("bdod_", depths)] / 100
  ph_vals  <- soil_row[paste0("phh2o_", depths)]
  
  new_lines <- replace_soil_properties(template_lines, nl, tkl, clay_vals, sand_vals, bd_vals, ph_vals)
  
  fname <- file.path(outpath, paste0("cell_", stn_id, ".sol"))
  if (file.exists(fname)) return(invisible(NULL))
  writeLines(new_lines, fname)
}

# Load soil template file (standard.sol)
template_sol_path <- file.path(path, "oryza", "control", "templates", "standard.sol")
template_sol_lines <- readLines(template_sol_path)

cells <- readRDS("data/cells.rds")
soil <- terra::rast("data/raw/soil_agg.tif")

for (i in 1:nrow(cells)) {
  print(cells$cell[i]); flush.console()
  x <- terra::extract(soil, cells$cell[i])
  #repair for now (left untouched)
  x$`clay_0-5cm` = 100 - (x$`silt_0-5cm` + x$`silt_0-5cm`)
  write_sol_file(cells$cell[i], x, template_sol_lines, tkl=c(0.05, .1, .15), soil_dir)
}


##### END OF: 2_make_soil_files.R #####


##### START OF: 2_make_wth_files.R #####

## make_oryza_file_pipeline.R — ROBUST (avoids NetCDF chunk/HDF read errors)
##
## Creates ORYZA *.cli files from precomputed POWER NetCDFs.
## Key improvement: loads all NetCDF data upfront, then extracts per-cell
## to avoid repeated file I/O which can cause HDF5/NetCDF errors.

# --- PATH (fixed for local environment) ---
path <- "G:/My Drive/simulation_data/v2"
setwd(path)

weather_dir <- file.path(path, "oryza", "input", "weather")  # Output .cli files
dir.create(weather_dir, FALSE, TRUE)

## ============================================================================
## 3. Helper functions for writing climate files
## ============================================================================

## Format single weather record as comma-separated line for .cli file
format_weather_line <- function(stn, year, day, srad, tmin, tmax, vap, wind, rain) {
  sprintf("%d,%d,%d,%.0f,%.1f,%.1f,%.2f,%.2f,%.1f", stn, year, day, srad, tmin, tmax, vap, wind, rain)
}

## Create ORYZA climate file header with station info and column descriptions
cli_header <- function(stn_id, lon, lat, alt) {
  paste0(c(
    "*-----------------------------------------------------------",
    sprintf("*  Station Name: STN%03d", stn_id),
    "*  Author      : Phoo", 
    "*  Source      : NASA Power",
    "*",
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
write_cli_file <- function(id, wdata, out_path) {
  # Create file header and coordinate line
  header <- cli_header(id$cell, id$x, id$y, id$elevation)
  coord_line <- sprintf("%.5f,%.5f,%.1f,0.00,0.00", id$x, id$y, id$elevation)
  
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
  out_path <- file.path(weather_dir, sprintf("STN%d.cli", id$cell))
  writeLines(c(header, coord_line, daily_lines), out_path)
}

## ============================================================================
## 4. Load all NetCDF weather data upfront
## ============================================================================

# Load the spatial dataset (SpatRasterDataset with all 6 variables)
vars <- c("radiation", "T2M_MIN", "T2M_MAX", "vapr", "WS2M", "rain")
fnames <- paste0("data/raw/weather/power/", vars, "-1995_2024-91.5x101.5x8x29.nc")
s <- terra::sds(fnames)

# Load cells
cells <- readRDS("data/cells.rds")

# Extract all weather data for all cells at once
# This returns a list of 6 matrices (one per variable), 
# where rows = time, columns = cells
message("Extracting weather data for all cells...")
x <- terra::extract(s, cells$cell)
x <- lapply(x, t)  # Transpose so columns = cells, rows = time

## ============================================================================
## 5. Write individual .cli files per cell
## ============================================================================

dates <- terra::time(s[[1]])  # Get date sequence from first raster

for (i in 1:nrow(cells)) {
  print(paste("Processing cell", cells$cell[i], "-", i, "of", nrow(cells))); flush.console()
  
  # Extract data for this cell from all variables
  y <- do.call(data.frame, lapply(x, \(v) v[,i]))
  names(y) <- c("SRAD", "TMIN", "TMAX", "VAP", "WIND", "RAIN")
  
  # Add metadata columns
  y$STN <- cells$cell[i]
  y$YEAR <- as.integer(format(dates, "%Y"))
  y$DAY <- as.integer(format(dates, "%j"))
  
  # Write .cli file for this cell
  write_cli_file(cells[i,], y, weather_dir)
}

message("Weather file creation complete!")

##### END OF: 2_make_wth_files.R #####


##### START OF: 3_make_rerun_files.R #####

## make_oryza_file_pipeline.R
##
## Generates reruns.rer file for batch simulation runs

# --- PATH (fixed for local environment) ---
path <- "G:/My Drive/simulation_data/v2"
setwd(path)

dir.create("oryza/reruns", FALSE, FALSE)

STTIME = seq(3, 365, 7)

rer <- expand.grid(STTIME=STTIME, IYEAR=2014:2023)

rer$EMYR <- rer$IYEAR 

rer <- data.frame(rerun=1:nrow(rer), rer)

#soil_path <- "..\\oryza\\soils\\cell_%03d.sol"
cells <- readRDS("data/cells.rds")
stations <- cells$cell

# Add one rerun configuration per station
for (i in 1:length(stations)) {
  rerun_file <- paste0("oryza/reruns/run_", i, ".rer")
  #	rer$fILEI2 <- sprintf("'%s'", sprintf(soil_path, stations[i]))
  rer$ISTN <- stations[i]
  x <- lapply(1:nrow(rer), \(i) paste0(names(rer), "=", rer[i,])) |> unlist()
  x <- gsub("^rerun", "\n*rerun", x)
  writeLines(x, rerun_file)
}

##### END OF: 3_make_rerun_files.R #####



##### START OF: 4_runmodel.R #####

##
# --- PATH (fixed for local environment) ---
path <- "G:/My Drive/simulation_data/v2"
setwd(file.path(path, "oryza"))
dir.create("output", FALSE, FALSE)

cells <- readRDS("../data/cells.rds")

# Read from control/templates subdirectory (relative to oryza folder)
x <- readLines("control/templates/CONTROL_template.dat")

i=1 
for (i in 1:nrow(cells)) {
  print(paste(i, "-", cells$cell[i])); flush.console()
  y <- gsub("_xxx", paste0("_", i), x)
  y <- gsub("_yyy", paste0("_", cells$cell[i]), y)
  
  # Write to working directory (oryza folder)
  writeLines(y, "CONTROL.DAT")
  
  s <- system("ORYZA3.exe", intern=TRUE)
}

##### END OF: 4_runmodel.R #####


##### START OF: 5_readoutput.R #####

# --- PATH (fixed for local environment) ---
path <- "G:/My Drive/simulation_data/v2"
setwd(path)

dir.create("data/output", FALSE, FALSE)

library(dplyr)
library(readr)
library(stringr)

# Extract ORYZA yields from res.dat file
extract_yields <- function(file_path) {
  
  lines <- readLines(file_path, warn = FALSE)
  rerun_lines <- grep("OUTPUT FROM RERUN SET:", lines)
  all_data <- list()
  
  for (i in seq_along(rerun_lines)) {
    start_idx <- rerun_lines[i]
    end_idx <- if(i < length(rerun_lines)) rerun_lines[i+1] - 1 else length(lines)
    section <- lines[start_idx:end_idx]
    
    # Get run ID
    run_line <- section[grep("OUTPUT FROM RERUN SET:", section)]
    run_id <- str_split(run_line, "\\s+")[[1]]
    run_id <- as.numeric(run_id[length(run_id)])
    
    # Find data
    header_idx <- grep("^TIME\\s+", section)
    if(length(header_idx) == 0) next
    
    headers <- str_split(str_squish(section[header_idx[1]]), "\\s+")[[1]]
    data_lines <- section[(header_idx[1] + 1):length(section)]
    data_lines <- data_lines[str_detect(data_lines, "^\\s*\\d")]
    
    if(length(data_lines) == 0) next
    
    # Parse data
    parsed_data <- list()
    for(j in seq_along(data_lines)) {
      vals <- str_split(str_squish(data_lines[j]), "\\s+")[[1]]
      if(length(vals) >= length(headers)) {
        parsed_data[[j]] <- setNames(vals[1:length(headers)], headers)
      }
    }
    
    if(length(parsed_data) == 0) next
    
    df <- data.frame(do.call(rbind, parsed_data), stringsAsFactors = FALSE)
    df$Run_ID <- run_id
    
    # Convert to numeric
    numeric_cols <- c("TIME", "YEAR", "DOY", "WRR14", "WAGT", "WSO", "WST", 
                      "LAI","TMIN", "TMAX", "RAIN")
    for(col in numeric_cols) {
      if(col %in% names(df)) {
        df[[col]][df[[col]] == "-"] <- NA
        df[[col]] <- as.numeric(df[[col]])
      }
    }
    
    all_data[[i]] <- df[nrow(df), ]
  }
  
  x <- dplyr::bind_rows(all_data)
  x$id <- gsub("output_|\\.dat", "", basename(file_path))
  return(x)
}

ff <- list.files("oryza/output/test/dat", pattern="\\.dat$", full=TRUE)

output <- lapply(ff, extract_yields)
x <- dplyr::bind_rows(output)

y <- x[, c("id", "YEAR", "WRR14")]
# Robust ID parse: keep digits only
y$id <- as.integer(gsub("\\D", "", y$id))

# Map to cells
cells <- readRDS("data/cells.rds")
y$cell <- cells$cell[y$id]

# Clean and de-dup
y <- dplyr::filter(y, !is.na(cell), !is.na(YEAR), !is.na(WRR14))
y <- dplyr::distinct(y)
y$YEAR <- as.integer(y$YEAR)

# Wide table: one col per YEAR
w <- tidyr::pivot_wider(y, id_cols = cell, names_from = YEAR, values_from = WRR14,
                        values_fn = max, values_fill = NA_real_)

# Build raster stack using exactly the present year columns
year_cols <- setdiff(names(w), "cell")
years <- as.integer(year_cols)

r <- terra::rast(terra::rast("data/raw/soil_agg.tif"), nlyr = length(years))
r[w$cell] <- as.matrix(w[, year_cols])
terra::time(r) <- years


##### END OF: 5_readoutput.R #####