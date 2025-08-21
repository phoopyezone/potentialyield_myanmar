## make_oryza_file_pipeline.R
##
## Generates reruns.rer file for batch simulation runs


this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else {
	path <- "C:/1_Research/yield_gap/v2"
}
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

			