

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else {
	path <- "C:/1_Research/yield_gap/v2"
}

dir.create(path, FALSE, TRUE)
setwd(path)
outpath <- "data/raw/NASA"
dir.create(outpath, FALSE, TRUE)


#https://power.larc.nasa.gov/api/temporal/daily/regional?latitude-min=8&latitude-max=18&longitude-min=92&longitude-max=102&parameters=T2M_MIN&community=SB&start=20000101&end=20001231&format=NetCDF

#T2M_MIN,T2M_MAX, WS2M,T2MDEW,PRECTOTCORR_SUM

get_NASA <- function(year, parameter, e, path) {
	ee <- paste(as.vector(e), collapse="x")
	f <- file.path(path, paste0(parameter, "-", year, "-", ee, ".nc"))
	if (!file.exists(f)) {
	print(f); flush.console()
		request <- paste0("https://power.larc.nasa.gov/api/temporal/daily/regional?latitude-min=", e$ymin, "&latitude-max=", e$ymax, "&longitude-min=", e$xmin, "&longitude-max=", e$xmax, "&parameters=PPPP&community=SB&start=YYYY0101&end=YYYY1231&format=NetCDF")
		url <- gsub("YYYY", year, request)
		url <- gsub("PPPP", parameter, url)
		g <- httr::GET(url)
		content <- httr::content(g, "raw")	
		writeBin(content, f)
	}
	f
}

vars <- c("ALLSKY_SFC_SW_DWN", "T2M_MIN", "T2M_MAX", "WS2M", "T2MDEW", "PRECTOTCORR")
for (var in vars) {
	for (year in 1995:2024) {
		fnc1 <- get_NASA(year, var, terra::ext(92, 102, 8, 18), outpath)
		fnc2 <- get_NASA(year, var, terra::ext(92, 102, 18, 28), outpath)
		fnc3 <- get_NASA(year, var, terra::ext(92, 102, 28, 30), outpath)
	}
}


outpath2 <- "data/intermediate/NASA"
dir.create(outpath2, F, T)

for (var in vars) {
	ff <- list.files(outpath, var, full=TRUE)
	r <- lapply(1995:2024, \(year) {
		fy <- grep(year, ff, value=TRUE)
		merge(sprc(lapply(fy, rast)))
	}) |> rast()
	terra::writeCDF(r, file.path(outpath2, paste0(var, ".nc")), overwrite=TRUE) 
}
	

# resample radiation 
tmp <- terra::rast(file.path(outpath2, "T2M_MIN.nc"))
rad <- terra::rast(file.path(outpath2, "ALLSKY_SFC_SW_DWN.nc"))
rrad <- terra::resample(rad, tmp)
terra::writeCDF(rrad, file.path(outpath2, "radiation.nc"))

# vapor pressure
dew <- terra::rast(file.path(outpath2, "T2MDEW.nc"))
vap <- 6.112 * exp((17.67 * dew) / (dew + 243.5))
terra::writeCDF(vap, file.path(outpath2, "vapr.nc"))



#r1 <- rast(fnc1)
#r2 <- rast(fnc2)
#r3 <- rast(fnc3)
#m = merge(r1, r2, r3)
