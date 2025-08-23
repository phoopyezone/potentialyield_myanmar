##
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else {
	path <- "C:/1_Research/yield_gap/v2"
}
setwd(file.path(path, "oryza"))
dir.create("output", FALSE, FALSE)
dir.create("logs", FALSE, FALSE)

cells <- readRDS("../data/cells.rds")

runname <- "potential"

dir.create(file.path("output", runname))

fexp <- "templates/experiment_template.txt"
fcntr <- "templates/CONTROL_template.txt"

cntr <- readLines(fcntr)
#cntr <- gsub("standard.crp", "Mestizo.crp", cntr)

expr <- readLines(fexp)
rrun <- readLines("reruns.rer")

ff <- c(fcntr, fexp, "reruns.rer")

file.copy(ff, file.path("output", runname, basename(ff)), overwrite=FALSE)

nrun <- max(30, sum(grepl("rerun", rrun)))

fcrpX <- "crops/standard.crpX"
fexpX <- "experiment.expX"
fsolX <- "soils/cell_yyy.solX"
 
options(warn=2)
for (i in 1:nrow(cells)) {
	fout <- paste0(file.path("output", runname), "/output_", i, ".dat")
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
		for (f in c(fcrpX, fexpX, fsx)) { if (file.exists(f)) {Sys.sleep(1); file.remove(f)} }

		try(system("ORYZA3.exe", intern=TRUE))
		Sys.sleep(1)
		fop <- paste0(file.path("output", runname), "/op_", i, ".val")
		file.copy("op.dat", fop)
	}
}
options(warn=0)


