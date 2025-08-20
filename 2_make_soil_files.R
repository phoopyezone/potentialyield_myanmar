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

## ============================================================================
## 1. Input and output paths

soil_dir  <- file.path(path, "oryza", "soils")    # Output .sol files  
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
write_sol_file <- function(stn_id, soil_row, template_lines, tkl=c(0.05, 0.10, 0.15, 0.30, 0.40, 1.00)) {
  # Define 6 soil layers with cumulative depths: 5, 15, 30, 60, 100, 200cm
  nl <- length(tkl)
    # Layer thickness (m)
  
  ## Extract and convert soil properties to ORYZA units
  # Clay/sand: % to fraction (divide by 100 - but data already in %, so divide by 1000 from g/kg)
  depths <- c("0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm")[1:nl]
  clay_vals <- as.numeric(soil_row[paste0("clay_", depths)]) / 1000
  sand_vals <- as.numeric(soil_row[paste0("sand_", depths)]) / 1000
  
  # Bulk density: cg/cm3 to g/cm3 (divide by 100)
  bd_vals   <- as.numeric(soil_row[paste0("bdod_", depths)]) / 100
  
  # pH: pH*10 to pH (divide by 10) if available
  ph_vals   <- NULL
  if (any(grepl("phh2o_5cm", names(soil_row)))) {
    ph_vals <- as.numeric(soil_row[paste0("phh2o_", depths)]) / 10
  }
  
  # Replace template values with site-specific data
  new_lines <- replace_soil_properties(template_lines, nl, tkl, clay_vals, sand_vals, bd_vals, ph_vals)
  
  # Update soil code identifier
  new_lines <- sub("SCODE = '[^']+'", sprintf("SCODE = '%s'", sprintf("GRID_%03d", stn_id)), new_lines)
  
  # Write soil file
  out_path <- file.path(soil_dir, sprintf("GRID_%03d.sol", stn_id))
  writeLines(new_lines, out_path)
}



# Load soil template file (standard.sol)
template_sol_path <- file.path(path, "data/oryza", "standard.sol")
template_sol_lines <- readLines(template_sol_path)

cells <- readRDS("data/cells.rds")
soil <- terra::rast("data/raw/soil_agg.tif")

for (i in 1:nrow(cells) {
	print(cells$cell[i]); flush.console()
	x <- extract(soil, cells$cell[i])
	write_sol_file(cells$cell[i], x, template_sol_lines)
}
