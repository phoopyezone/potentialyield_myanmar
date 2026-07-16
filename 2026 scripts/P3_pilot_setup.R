## P3_pilot_setup.R
## Defines pilot cell selection, copies input files, archives old ones,
## clears previous output, and saves pilot_selected_cells.rds.
## Re-run this script whenever the cell selection changes.

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
    path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else if (this == "phoopyezone") {
    path <- "G:/My Drive/Research/simulation/main_oryza/v2"
} else { path <- "." }
setwd(path)

library(terra)

#### change — update cell IDs and state labels here
new_cells <- data.frame(
    cell  = c( 406,          422,          408,      342,           361,
               296,            232,          281,            215,       183),
    state = c("Ayeyarwady", "Ayeyarwady", "Yangon", "Bago (West)", "Bago (East)",
              "Nay Pyi Taw", "Mandalay",  "Shan (South)", "Sagaing", "Sagaing"),
    stringsAsFactors = FALSE
)
#### do not change below

# ── 1. Build pilot_selected_cells.rds ─────────────────────────────────────────
cells_all <- readRDS("data/cells.rds")
names(cells_all)[names(cells_all) == "MMR_elv"] <- "elevation"

selected <- merge(cells_all, new_cells, by = "cell")
selected <- selected[match(new_cells$cell, selected$cell), ]   # preserve order
selected$pilot_idx  <- seq_len(nrow(selected))
selected$mean_WRR14 <- NA_real_

saveRDS(selected, "data/pilot_selected_cells.rds")
cat("pilot_selected_cells.rds saved —", nrow(selected), "cells\n")
print(selected[, c("pilot_idx", "cell", "x", "y", "state")])

# ── 2. Copy weather and soil files to pilot subfolders ─────────────────────────
wth_src  <- "simulate/input/weather"
wth_dst  <- "simulate/input/weather/pilot"
sol_src  <- "simulate/input/soils"
sol_dst  <- "simulate/input/soils/pilot"
wth_arch <- file.path(wth_dst, "archive")
sol_arch <- file.path(sol_dst, "archive")

dir.create(wth_dst,  recursive = TRUE, showWarnings = FALSE)
dir.create(sol_dst,  recursive = TRUE, showWarnings = FALSE)
dir.create(wth_arch, recursive = TRUE, showWarnings = FALSE)
dir.create(sol_arch, recursive = TRUE, showWarnings = FALSE)

keep_ids <- selected$cell

## Archive any existing pilot files not in the new selection
for (f in list.files(wth_dst, pattern = "^STN[0-9]+\\.cli$", full.names = TRUE)) {
    cid <- as.integer(sub(".*STN([0-9]+)\\.cli", "\\1", basename(f)))
    if (!cid %in% keep_ids) {
        file.rename(f, file.path(wth_arch, basename(f)))
        cat("Archived weather:", basename(f), "\n")
    }
}
for (f in list.files(sol_dst, pattern = "^cell_[0-9]+\\.sol$", full.names = TRUE)) {
    cid <- as.integer(sub(".*cell_([0-9]+)\\.sol", "\\1", basename(f)))
    if (!cid %in% keep_ids) {
        file.rename(f, file.path(sol_arch, basename(f)))
        cat("Archived soil:   ", basename(f), "\n")
    }
}

## Copy new files (skip if already present)
for (cid in keep_ids) {
    wf <- file.path(wth_dst, paste0("STN",   cid, ".cli"))
    sf <- file.path(sol_dst, paste0("cell_", cid, ".sol"))
    if (!file.exists(wf))
        file.copy(file.path(wth_src, paste0("STN",   cid, ".cli")), wf)
    if (!file.exists(sf))
        file.copy(file.path(sol_src, paste0("cell_", cid, ".sol")), sf)
}
cat("\nWeather files in pilot/:", length(list.files(wth_dst, "*.cli")),
    "| Soil files:", length(list.files(sol_dst, "*.sol")), "\n")

# ── 3. Clear old dat and log output files ──────────────────────────────────────
runs <- c("pilot_potential_standard", "pilot_potential_mestizo",
          "pilot_potential_local",    "pilot_potential_RC222",
          "pilot_watbal_standard",    "pilot_watbal_mestizo",
          "pilot_watbal_local",       "pilot_watbal_RC222")

for (r in runs) {
    for (d in c("dat", "logs")) {
        folder <- file.path("simulate/output", r, d)
        dir.create(folder, recursive = TRUE, showWarnings = FALSE)
        old <- list.files(folder, full.names = TRUE)
        if (length(old) > 0) {
            file.remove(old)
            cat("Cleared", length(old), "files from", file.path(r, d), "\n")
        }
    }
}

cat("\nP3 complete. Ready to run P4 for all 4 combos.\n")
