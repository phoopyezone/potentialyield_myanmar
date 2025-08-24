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

STTIME = round(seq(15.4, 365, 30.4167))

rer <- expand.grid(STTIME=STTIME, IYEAR=2014:2023)
#rer <- expand.grid(STTIME=STTIME, IYEAR=2014)
rer <- sapply(rer, as.character)
rer <- data.frame(rerun=1:nrow(rer), rer)

rer$EMD <- as.integer(rer$STTIME) + 7
rer$EMYR <- rer$IYEAR 
rer$STTIME = paste0(rer$STTIME, ".")
#rer$PRODENV = 'POTENTIAL'

rerun_file <- "oryza/reruns.rer"
x <- lapply(1:nrow(rer), \(i) paste0(names(rer), "=", rer[i,])) |> unlist()
x <- gsub("^rerun", "\n*rerun", x)
writeLines(x, rerun_file)



#cells <- readRDS("data/cells.rds")
#for (i in 1:length(stations)) {
#	rerun_file <- paste0("oryza/reruns/run_", i, ".rer")
#	rer$ISTN <- stations[i]
#	x <- lapply(1:nrow(rer), \(i) paste0(names(rer), "=", rer[i,])) |> unlist()
#	x <- gsub("^rerun", "\n*rerun", x)
#	writeLines(x, rerun_file)
#}

			