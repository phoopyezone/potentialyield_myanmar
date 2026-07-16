this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else if (this == "phoopyezone") {
	path <- "G:/My Drive/Research/simulation/main_oryza/v2"
} else {
	path <- "."
}
setwd(path)

library(terra)
library(dplyr)
library(openxlsx)

admin_shp <- "G:/My Drive/Research/simulation/data_myanmar/admin_boundaries/state_regions/mmr_polbnda2_adm1_250k_mimu_1.shp"
out_dir   <- "data/output"
dir.create(file.path(out_dir, "maps"), FALSE, TRUE)

# ── Load data ─────────────────────────────────────────────────────────────────
x     <- read.csv(file.path(out_dir, "test.csv"))
cells <- readRDS("data/cells.rds")
admin <- vect(admin_shp)

# x has columns: cell, start_date, WRR14, WAGT, WSO, WST, LAI, sYEAR, sDOY
# Average yield per cell across all planting dates and years
cell_avg <- x |>
  filter(WRR14 > 0) |>
  group_by(cell) |>
  summarise(
    mean_yield = mean(WRR14, na.rm=TRUE),
    sd_yield   = sd(WRR14,   na.rm=TRUE),
    max_yield  = max(WRR14,  na.rm=TRUE),
    min_yield  = min(WRR14,  na.rm=TRUE),
    n          = n(),
    .groups = "drop"
  )

# Yearly average per cell (for year map)
cell_year <- x |>
  filter(WRR14 > 0) |>
  mutate(year = as.integer(substr(start_date, 1, 4))) |>
  group_by(cell, year) |>
  summarise(mean_yield = mean(WRR14, na.rm=TRUE), .groups="drop")

# ── Spatial join: assign state to each cell ───────────────────────────────────
cell_xy  <- cells[, c("cell","x","y")]
pts      <- vect(cell_xy, geom=c("x","y"), crs=crs(admin))
st_join  <- terra::extract(admin, pts)
cell_xy$state   <- st_join$ST
cell_xy$st_type <- st_join$ST_RG

cell_avg <- merge(cell_avg, cell_xy, by="cell")

# ── Raster maps ───────────────────────────────────────────────────────────────
template <- rast("data/raw/soil_agg.tif")

make_raster <- function(df, value_col) {
  r <- rast(template, nlyr=1)
  r[df$cell] <- df[[value_col]]
  r
}

r_mean <- make_raster(cell_avg, "mean_yield")
r_max  <- make_raster(cell_avg, "max_yield")

# Annual max yield per year raster stack
years  <- sort(unique(cell_year$year))
r_yrs  <- rast(lapply(years, function(yr) {
  dy <- cell_year[cell_year$year == yr, ]
  make_raster(dy, "mean_yield")
}))
names(r_yrs) <- years

# ── Plot and save maps ────────────────────────────────────────────────────────
pal <- hcl.colors(100, "Viridis")

save_map <- function(r, fname, title, leg_title="Yield (kg/ha)") {
  png(file.path(out_dir, "maps", fname), width=900, height=1100, res=120)
  plot(admin, border="grey60", col=NA, main=title, cex.main=1.1)
  plot(r, col=pal, legend=TRUE, add=TRUE, plg=list(title=leg_title))
  plot(admin, border="grey40", col=NA, add=TRUE, lwd=0.8)
  dev.off()
}

save_map(r_mean, "yield_mean_2014_2023.png", "Mean Rice Yield 2014-2023 (kg/ha)")
save_map(r_max,  "yield_max_2014_2023.png",  "Max Rice Yield 2014-2023 (kg/ha)")

# Annual maps (small multiples)
png(file.path(out_dir, "maps", "yield_by_year_2014_2023.png"),
    width=1600, height=1200, res=120)
par(mfrow=c(2, 5), mar=c(1,1,2,3))
for (yr in years) {
  plot(admin, border="grey60", col="grey95", main=yr, cex.main=1)
  plot(r_yrs[[as.character(yr)]], col=pal, add=TRUE, legend=FALSE)
  plot(admin, border="grey40", col=NA, add=TRUE, lwd=0.5)
}
dev.off()

terra::writeRaster(r_mean, file.path(out_dir, "maps", "yield_mean_2014_2023.tif"), overwrite=TRUE)
terra::writeRaster(r_yrs,  file.path(out_dir, "maps", "yield_by_year_2014_2023.tif"), overwrite=TRUE)
cat("Maps saved.\n")

# ── Descriptive statistics by state ──────────────────────────────────────────
state_stats <- cell_avg |>
  group_by(State=state, Type=st_type) |>
  summarise(
    `N Cells`            = n(),
    `Mean Yield (kg/ha)` = round(mean(mean_yield), 0),
    `SD (kg/ha)`         = round(mean(sd_yield, na.rm=TRUE), 0),
    `Min (kg/ha)`        = round(min(min_yield), 0),
    `Max (kg/ha)`        = round(max(max_yield), 0),
    .groups = "drop"
  ) |> arrange(desc(`Mean Yield (kg/ha)`))

state_year_stats <- cell_year |>
  mutate(year=as.character(year)) |>
  left_join(cell_xy[, c("cell","state")], by="cell") |>
  group_by(State=state, Year=year) |>
  summarise(`Mean Yield (kg/ha)` = round(mean(mean_yield), 0), .groups="drop") |>
  tidyr::pivot_wider(names_from=Year, values_from=`Mean Yield (kg/ha)`)

cell_out <- cell_avg |>
  select(Cell=cell, Lon=x, Lat=y, State=state, Type=st_type,
         `Mean (kg/ha)`=mean_yield, `SD (kg/ha)`=sd_yield,
         `Min (kg/ha)`=min_yield, `Max (kg/ha)`=max_yield, N=n) |>
  mutate(across(where(is.numeric), \(v) round(v, 0))) |>
  arrange(desc(`Mean (kg/ha)`))

# ── Write Excel ───────────────────────────────────────────────────────────────
hdr <- createStyle(textDecoration="bold", fgFill="#2E75B6",
                   fontColour="white", halign="center")
wb  <- createWorkbook()

for (nm in c("State Summary","By State and Year","Cell Level")) addWorksheet(wb, nm)

writeDataTable(wb, "State Summary",      state_stats,  tableStyle="TableStyleMedium9")
writeDataTable(wb, "By State and Year",  state_year_stats, tableStyle="TableStyleMedium9")
writeDataTable(wb, "Cell Level",         cell_out,     tableStyle="TableStyleLight1")

for (sh in c("State Summary","By State and Year","Cell Level")) {
  addStyle(wb, sh, hdr, rows=1, cols=1:20, gridExpand=TRUE)
  setColWidths(wb, sh, cols=1:20, widths="auto")
}

saveWorkbook(wb, file.path(out_dir, "myanmar_rice_yield_by_state_2014_2023.xlsx"), overwrite=TRUE)
cat("Excel saved.\n")
