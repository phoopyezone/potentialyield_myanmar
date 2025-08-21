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



aoi <- geodata::gadm("Myanmar", level=1, path="data/raw")

vars <- c("radiation", "T2M_MIN", "T2M_MAX", "vapr", "WS2M", "rain")
fnames <- paste0("data/raw/weather/power/", vars, "-1995_2024-91.5x101.5x8x29.nc")
s <- terra::sds(fnames)
cells <- readRDS("data/cells.rds")

x <- terra::extract(s, cells$cell)
x <- lapply(x, t)


for (i in 1:nrow(cells)) {
	print(cells$cell[i]); flush.console()
	y <- do.call(data.frame, lapply(x, \(v) v[,i]))
	names(y) <- c("SRAD", "TMIN", "TMAX", "VAP", "WIND", "RAIN")
	dates <- terra::time(s[[1]])
	y$STN=cells$cell[i]
	y$YEAR=as.integer(format(dates, "%Y"))
	y$DAY=as.integer(format(dates, "%j"))
	write_cli_file(cells[i,], y, weather_dir)
}


