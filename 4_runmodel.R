##
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else {
	path <- "C:/1_Research/yield_gap/v2"
}

#### change
runname <- "test"


#### do not change
setwd(file.path(path, "oryza"))
dir.create("output", FALSE, FALSE)
dir.create(file.path("output", runname))
s <- sapply(c("control", "logs", "op", "dat"), \(d) dir.create(file.path("output", runname, d), FALSE))
fexp <- "control/templates/experiment_template.txt"
fcntr <- "control/templates/CONTROL_template.txt"
frer <- "control/reruns.rer"
cntr <- readLines(fcntr)
expr <- readLines(fexp)
rrun <- readLines(frer)
cells <- readRDS("../data/cells.rds")

ff <- c(fcntr, fexp, frer)
file.copy(ff, file.path("output", runname, "control", basename(ff)), overwrite=FALSE)
nrun <- max(12, sum(grepl("rerun", rrun)))
fX <- c("control/crops/standard.crpX", "control/experiment.expX")
fsolX <- "input/soils/cell_yyy.solX"
#### 

#### change
#cntr <- gsub("standard.crp", "Mestizo.crp", cntr)


options(warn=2)
for (i in 1:nrow(cells)) {
	fout <- paste0(file.path("output", runname, "dat"), "/output_", i, ".dat")
	if ((!file.exists(fout)) || (file.info(fout)$size == 0)) {	
		print(paste0("run ", i, ", cell ", cells$cell[i])); flush.console()
		y <- gsub("_xxx", paste0("_", i), cntr)
		y <- gsub("yyy", cells$cell[i], y)
		y <- gsub("_zzz", runname, y)
		y <- gsub("_rrr", nrun, y)
		writeLines(y, "CONTROL.DAT")

		z <- gsub("_yyy", cells$cell[i], expr)
		writeLines(z, "experiment.exp")
		
		fsx <- gsub("yyy", cells$cell[i], fsolX)
		for (f in c(fX, fsx)) { if (file.exists(f)) {Sys.sleep(1); file.remove(f)} }

		try(system("ORYZA3.exe", intern=TRUE))
		Sys.sleep(1)
		fop <- file.path("output", runname, "op", paste0("op_", i, ".val"))
		if (file.exists(fop)) file.remove(fop); 
		file.copy("op.dat", fop)
	}
}
options(warn=0)


