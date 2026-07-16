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
combos <- list(
	list(eenv = "potential", ccvr = "standard"),
	list(eenv = "potential", ccvr = "mestizo"),
	list(eenv = "potential", ccvr = "local"),
	list(eenv = "potential", ccvr = "RC222"),
	list(eenv = "watbal",    ccvr = "standard"),
	list(eenv = "watbal",    ccvr = "mestizo"),
	list(eenv = "watbal",    ccvr = "local"),
	list(eenv = "watbal",    ccvr = "RC222")
)
#### do not change
setwd(file.path(path, "simulate"))
dir.create("output", FALSE, FALSE)
fcntr <- "control/templates/pilot_CONTROL_template.txt"
frer  <- "control/reruns.rer"
cntr  <- readLines(fcntr)
rrun  <- readLines(frer)
cells <- readRDS("../data/pilot_selected_cells.rds")
nrun  <- max(12, sum(grepl("rerun", rrun)))
fsolX <- "input/soils/pilot/cell_yyy.solX"
####

for (combo in combos) {
	eenv    <- combo$eenv
	ccvr    <- combo$ccvr
	runname <- paste0("pilot_", eenv, "_", ccvr)

	dir.create(file.path("output", runname), FALSE, FALSE)
	s <- sapply(c("logs", "dat"), \(d) dir.create(file.path("output", runname, d), FALSE))

	fX <- paste0("control/crops/", ccvr, ".crpX")

	cat("====", runname, "====\n"); flush.console()

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
	cat("====", runname, "DONE ====\n"); flush.console()
}
cat("ALL COMBOS COMPLETE\n")
