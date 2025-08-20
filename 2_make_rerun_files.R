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

rerun_file <- "oryza/reruns.rer"

# Initialize reruns.rer file with header
rer_lines <- c(
  "*==============================================*",
  "* RERUNS – one run per grid/station combination *",
  "*==============================================*",
  ""
)


soil_path <- ".\\oryza\\soils\\cell_%03d.sol"

cells <- readRDS("data/cells.rds")
stations <- cells$cell

# Add one rerun configuration per station
for (i in 1:length(stations)) {
  fname <- sprintf(soil_path, stations[i])
  
  # Add rerun configuration
  rer_lines <- c(rer_lines,
                 sprintf("*Rerun set %d", i),
                 sprintf("FILEI2 = '%s'", fname),  # Soil file path
                 #"CNTR   = 'STN'",                 # Control variable
                 sprintf("ISTN   = %d", stations[i]),      # Station number
                 "")
}
writeLines(rer_lines, rerun_file)

			