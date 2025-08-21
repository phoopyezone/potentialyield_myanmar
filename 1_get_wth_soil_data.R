
# using geodata 0.6-4
# remotes::install_github("rspatial/geodata")

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else {
	path <- "C:/1_Research/yield_gap/v2"
}

dir.create(path, FALSE, TRUE)
setwd(path)
outpath <- "data/raw/"
dir.create(outpath, FALSE, TRUE)

vars <- c("ALLSKY_SFC_SW_DWN", "T2M_MIN", "T2M_MAX", "WS2M", "T2MDEW", "PRECTOTCORR")
years <- 1995:2024
ext <- terra::ext(91.5, 101.5,  8, 29)

for (var in vars) {
	geodata:::powerWeather(years, var, ext, outpath)
}

outpath <- "data/raw/weather/power"
# resample radiation 
tmp <- terra::rast(file.path(outpath, "T2M_MIN-1995_2024-91.5x101.5x8x29.nc"))
rad <- terra::rast(file.path(outpath, "ALLSKY_SFC_SW_DWN-1995_2024-91.5x101.5x8x29.nc"))
rrad <- terra::resample(rad, tmp, "average")
# from (W/m^2) to (kJ/m^2)/day 
rrad <- round((24 * 3.6) * rrad)
terra::writeCDF(rrad, file.path(outpath, "radiation-1995_2024-91.5x101.5x8x29.nc"), varname="radiation", longname="radiation", overwrite=TRUE)

# vapor pressure
dew <- terra::rast(file.path(outpath, "T2MDEW-1995_2024-91.5x101.5x8x29.nc"))
vap <- 6.112 * exp((17.67 * dew) / (dew + 243.5))
#from  hPa to kPa
vap <- vap / 10
terra::writeCDF(vap, file.path(outpath, "vapr-1995_2024-91.5x101.5x8x29.nc"), varname="VAPR", longname="vapor pressure", overwrite=TRUE)


rain <- terra::rast(file.path(outpath, "PRECTOTCORR-1995_2024-91.5x101.5x8x29.nc")) * 86400
terra::writeCDF(rain, file.path(outpath, "rain-1995_2024-91.5x101.5x8x29.nc"), varname="RAIN", longname="precipitation", unit="mm", overwrite=TRUE)

### elevation 
elv <- geodata::elevation_30s("Myanmar", path="data/raw")
elv <- terra::resample(elv, dew, "average", filename="data/raw/elevation.tif")



### soil
elv <- terra::rast("data/raw/elevation.tif")
ext <- terra::ext(91.5, 101.5,  8, 29)
vars <- c("bdod", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")
depths <- c(5, 15, 30)
soil <- geodata::soil_world(vars, depths, stat="mean", vsi=TRUE)
soil <- terra::crop(soil, ext, filename="data/raw/soil.tif")

soil <- terra::rast("data/raw/soil.tif")
soil[["clay_0-5cm"]] <- 100 - (soil[["sand_0-5cm"]] + soil[["silt_0-5cm"]])

soil2 <- terra::resample(soil, elv, "average", filename="data/raw/soil_agg.tif", overwrite=TRUE)


## cells
aoi <- geodata::gadm("Myanmar", level=1, path="data/raw")
r <- rast(elv)
r <- mask(init(r, "cell"), aoi, touches=TRUE)
rice_rast <- geodata::crop_spam(crop="rice", var="area", "data/raw") |> terra::crop(aoi, mask=TRUE)
rice_rast <- resample(rice_rast[[1]], r, "sum") > 0
r <- mask(r, rice_rast, maskvalue=FALSE)
cells <- data.frame(r)[,1]

xy <- data.frame(xyFromCell(r, cells))
relv <- rast("data/raw/elevation.tif")
xy$elevation <- round(relv[cells])
xy$cell <- cells

saveRDS(xy, "data/cells.rds")

