##
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else {
	path <- "C:/1_Research/yield_gap/v2"
}
setwd(file.path(path, "oryza"))
dir.create("output", FALSE, FALSE)

cells <- readRDS("../data/cells.rds")

x <- readLines("CONTROL_template.DAT")
 
i=1 
for (i in 1:nrow(cells)) {
	print(paste(i, "-", cells$cell[i])); flush.console()
	y <- gsub("_xxx", paste0("_", i), x)
	y <- gsub("_yyy", paste0("_", cells$cell[i]), y)
	writeLines(y, "CONTROL.DAT")
	s <- system("ORYZA3.exe", intern=TRUE)
}



