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

runname <- "test"
dir.create(file.path("output", runname))


cntr <- readLines("templates/CONTROL_template.DAT")
expr <- readLines("templates/experiment_template.exp")
rrun <- readLines("reruns.rer")
nrun <- max(30, sum(grepl("rerun", rrun)))

fcrpX <- "crops/standard.crpX"
 
options(warn=2)
for (i in 1:nrow(cells)) {
	fout <- paste0(file.path("output", runname), "/output_", i, ".dat")
	if (!file.exists(fout)) {	
		print(paste0("run ", i, ", cell ", cells$cell[i])); flush.console()
		y <- gsub("_xxx", paste0("_", i), cntr)
		y <- gsub("yyy", cells$cell[i], y)
		y <- gsub("_zzz", runname, y)
		y <- gsub("_rrr", nrun, y)
		writeLines(y, "CONTROL.DAT")
		z <- gsub("_yyy", cells$cell[i], expr)
		writeLines(z, "experiment.exp")

		if (file.exists(fcrpX)) {
			Sys.sleep(1)
			file.remove(fcrpX)
		}
		system("ORYZA3.exe", intern=TRUE)
	}
}
options(warn=0)


