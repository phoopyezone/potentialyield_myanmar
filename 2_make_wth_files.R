## make_oryza_file_pipeline.R
##
## Automates generation of multi-year weather (*.cli) and soil (*.sol) files for ORYZA
## Reads processed weather and soil data, creates individual files per station/grid
## Generates reruns.rer file for batch simulation runs


this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else {
	path <- "C:/1_Research/yield_gap/v2"
}
setwd(path)

#library(data.table)  # Fast data processing
#library(dplyr)       # Data manipulation

## ============================================================================
## 1. Input and output paths
## ============================================================================

weather_dir <- file.path(path, "oryza", "weather")  # Output .cli files
dir.create(weather_dir, FALSE, TRUE)

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
write_cli_file <- function(stn_id, wdata, xye, out_path) {
  lon <- xye$x
  lat <- xye$y
  alt <- xye$elevation
  
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



aoi <- geodata::gadm("Myanmar", level=1, path="data/raw")

vars <- c("radiation", "T2M_MIN", "T2M_MAX", "vapr", "WS2M", "rain")
fnames <- paste0("data/raw/weather/power/", vars, "-1995_2024-91.5x101.5x8x29.nc")
s <- sds(fnames)
cells <- readRDS("data/cells.rds")


for (i in 1:nrow(cells)) {
	print(cells$cell[i]); flush.console()
	x <- extract(s, cells$cell[i])
	x <- lapply(x, t)
	x <- data.frame(do.call(cbind, x))
	names(x) <- c("SRAD", "TMIN", "TMAX", "VAP", "WIND", "RAIN")
	dates <- time(s[[1]])

	x$STN = cells$cell[i]
	x$YEAR=as.integer(format(dates, "%Y"))
	x$DAY=as.integer(format(dates, "%j"))
	
	write_cli_file(cells[i], x, cells[i,], weather_dir)
}


