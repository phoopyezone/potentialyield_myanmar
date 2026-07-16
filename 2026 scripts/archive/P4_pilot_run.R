##
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else if (this == "phoopyezone") {
	path <- "G:/My Drive/Research/simulation/main_oryza/v2"
} else {
	path <- "."
}

#### change
eenv    <- "watbal"      # "potential" or "watbal"
ccvr    <- "mestizo"     # "standard" or "mestizo"
runname <- paste0("pilot_", eenv, "_", ccvr)


#### do not change
setwd(file.path(path, "simulate"))
dir.create("output", FALSE, FALSE)
dir.create(file.path("output", runname))
s <- sapply(c("logs", "dat"), \(d) dir.create(file.path("output", runname, d), FALSE))
fcntr <- "control/templates/pilot_CONTROL_template.txt"
frer  <- "control/reruns.rer"
cntr  <- readLines(fcntr)
rrun  <- readLines(frer)
cells <- readRDS("../data/pilot_selected_cells.rds")

nrun  <- max(12, sum(grepl("rerun", rrun)))
fX    <- paste0("control/crops/", ccvr, ".crpX")
fsolX <- "input/soils/pilot/cell_yyy.solX"
####

for (i in 1:nrow(cells)) {
	fout <- paste0(file.path("output", runname, "dat"), "/output_", i, ".dat")
	if ((!file.exists(fout)) || (file.info(fout)$size == 0)) {
		print(paste0("run ", i, ", cell ", cells$cell[i])); flush.console()
		y <- gsub("_xxx", paste0("_", i), cntr)
		y <- gsub("yyy",  cells$cell[i], y)
		y <- gsub("_zzz", runname, y)
		y <- gsub("_rrr", nrun,    y)
		y <- gsub("_env", eenv,    y)
		y <- gsub("_cvr", ccvr,    y)
		writeLines(y, "CONTROL.DAT")

		fsx <- gsub("yyy", cells$cell[i], fsolX)
		for (f in c(fX, fsx)) { if (file.exists(f)) {Sys.sleep(1); file.remove(f)} }

		try(system(paste0('"', getwd(), '/ORYZA3.exe"'), intern=TRUE))
		Sys.sleep(1)
	}
}
