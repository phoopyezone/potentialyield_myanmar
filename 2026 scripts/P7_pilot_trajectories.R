## P7_pilot_trajectories.R
## Reads full daily trajectories from res.dat (all time steps, not just peak).
## Plots grain yield (WRR14) and total biomass (WAGT) accumulation curves for
## each planting month, averaged across cells and years.
## Run AFTER P5 (requires the dat files; does NOT need pilot_results.csv).

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
    path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else if (this == "phoopyezone") {
    path <- "G:/My Drive/Research/simulation/main_oryza/v2"
} else { path <- "." }
setwd(path)

library(dplyr)

out_dir <- "data/output/pilot"
dir.create(file.path(out_dir, "maps"), FALSE, TRUE)

# ── Full-trajectory extractor ──────────────────────────────────────────────────
# Returns ALL time steps (P5's extract_5 returns only the peak row per rerun)
extract_full <- function(file_path) {
    lines     <- readLines(file_path, warn=FALSE)
    start_idx <- grep("OUTPUT FROM RERUN SET:", lines)
    if (length(start_idx)==0) return(NULL)
    end_idx   <- c(start_idx[-1], length(lines))

    all_data <- vector("list", length(start_idx))
    for (i in seq_along(start_idx)) {
        section    <- lines[start_idx[i]:end_idx[i]]
        header_idx <- grep("^TIME\\s+", section)
        if (length(header_idx)==0) next
        headers    <- stringr::str_split(
            stringr::str_squish(section[header_idx[1]]), "\\s+")[[1]]
        data_lines <- section[(header_idx[1]+1):length(section)]
        data_lines <- data_lines[stringr::str_detect(data_lines, "^\\s*\\d")]
        if (length(data_lines)==0) next

        parsed <- vector("list", length(data_lines))
        for (j in seq_along(data_lines)) {
            vals <- stringr::str_split(stringr::str_squish(data_lines[j]), "\\s+")[[1]]
            if (length(vals) >= length(headers))
                parsed[[j]] <- setNames(vals[1:length(headers)], headers)
        }
        parsed <- parsed[!sapply(parsed, is.null)]
        if (length(parsed)==0) next

        df <- data.frame(do.call(rbind, parsed), stringsAsFactors=FALSE)
        df[df=="-"] <- NA
        df$run_ID   <- i

        num_cols <- c("TIME","YEAR","DOY","WRR14","WAGT","WSO","LAI")
        num_cols <- num_cols[num_cols %in% names(df)]
        df[num_cols] <- sapply(df[num_cols], as.numeric)
        all_data[[i]] <- df
    }

    out <- dplyr::bind_rows(all_data)
    if (nrow(out)==0) return(NULL)

    # Days after transplanting: 0, 10, 20, ... relative to first DOY of each run
    out <- out |>
        dplyr::group_by(run_ID) |>
        dplyr::mutate(
            sYEAR = YEAR[1],
            sDOY  = as.integer(DOY[1]),
            dat   = (YEAR - YEAR[1]) * 365L + as.integer(DOY) - as.integer(DOY[1])
        ) |>
        dplyr::ungroup()

    out$cell_ID <- as.integer(gsub("output_|\\.dat", "", basename(file_path)))
    out
}

# ── Read all 8 runs ────────────────────────────────────────────────────────────
runs <- c("pilot_potential_standard","pilot_potential_mestizo",
          "pilot_potential_local",   "pilot_potential_RC222",
          "pilot_watbal_standard",   "pilot_watbal_mestizo",
          "pilot_watbal_local",      "pilot_watbal_RC222")

cat("Reading full trajectories (may take ~1 min)...\n"); flush.console()
traj_list <- lapply(runs, function(run) {
    ff  <- list.files(file.path("simulate/output", run, "dat"),
                      pattern="\\.dat$", full.names=TRUE)
    out <- lapply(ff, function(f) {
        e <- try(extract_full(f), silent=TRUE)
        if (inherits(e,"try-error") || is.null(e)) NULL else e
    })
    out <- dplyr::bind_rows(out[!sapply(out, is.null)])
    if (nrow(out)==0) return(NULL)
    parts    <- strsplit(run, "_")[[1]]
    out$eenv <- parts[2]
    out$ccvr <- parts[3]
    out
})
traj <- dplyr::bind_rows(traj_list[!sapply(traj_list, is.null)])
cat("Total rows:", nrow(traj), "\n")

# ── Planting month labels ──────────────────────────────────────────────────────
doy_to_month <- function(doy) {
    findInterval(as.integer(doy),
                 c(1L,32L,60L,91L,121L,152L,182L,213L,244L,274L,305L,335L))
}
month_lbl <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
               "Oct","Nov","Dec")
traj$plant_month  <- month_lbl[doy_to_month(traj$sDOY)]
traj$plant_month_n <- doy_to_month(traj$sDOY)   # numeric for ordering

# ── Mean trajectory: average across cells and years per planting month ─────────
traj_avg <- traj |>
    filter(!is.na(WRR14), !is.na(WAGT)) |>
    group_by(eenv, ccvr, plant_month, plant_month_n, sDOY, dat) |>
    summarise(
        WRR14 = mean(WRR14, na.rm=TRUE) / 1000,   # g/m² → t/ha
        WAGT  = mean(WAGT,  na.rm=TRUE) / 1000,
        .groups = "drop"
    )

# ── Cultivar duration labels (from P6 logic, for panel subtitles) ──────────────
cells <- readRDS("data/pilot_selected_cells.rds")
x_sum <- read.csv("data/output/pilot_results.csv")
x_sum <- merge(x_sum, cells[, c("cell","state")], by="cell")
dur_df <- x_sum |>
    filter(WRR14 > 0, as.integer(sDOY) != 259L) |>
    mutate(dur_tp = (eYEAR - sYEAR)*365L +
                    as.integer(eDOY) - as.integer(sDOY) - 30L) |>
    filter(dur_tp > 50L, dur_tp < 250L) |>
    group_by(ccvr) |>
    summarise(dur_tp=round(mean(dur_tp, na.rm=TRUE)), .groups="drop") |>
    mutate(
        dur_sow   = dur_tp + 30L,
        dur_label = paste0(ifelse(dur_tp < 100, "Short", "Medium"),
                           " (~", dur_sow, " d)")   # sowing-to-maturity
    )
dur_lbl <- setNames(dur_df$dur_label, dur_df$ccvr)

# ── Plot constants ─────────────────────────────────────────────────────────────
cvr_order <- c("standard","mestizo","local","RC222")
env_order <- c("potential","watbal")

plant_months_present <- month_lbl[sort(unique(traj$plant_month_n))]
n_pm      <- length(plant_months_present)
month_pal <- setNames(hcl.colors(n_pm, "Zissou 1"), plant_months_present)

# ── FIGURE: Grain + biomass trajectories (4 cultivar × 2 env panels + legend) ──
# Layout: 4 rows × 3 cols  (col 1 = potential, col 2 = watbal, col 3 = legend)
lay_mat <- cbind(matrix(1:8, nrow=4L, ncol=2L, byrow=TRUE), 9L)
#   [1] [2] [9]
#   [3] [4] [9]
#   [5] [6] [9]
#   [7] [8] [9]

png(file.path(out_dir, "maps", "pilot_trajectories.png"),
    width=3400, height=3600, res=150)
layout(lay_mat, widths=c(1, 1, 0.38))

panel_i <- 0L
for (cvr in cvr_order) {
    for (env in env_order) {
        panel_i <- panel_i + 1L
        par(mar = c(4.5, 5.5, 3.5, 0.8))

        d       <- traj_avg[traj_avg$ccvr==cvr & traj_avg$eenv==env, ]
        env_lbl <- ifelse(env=="potential", "Potential", "Water-limited")
        mtitle  <- paste0(tools::toTitleCase(cvr), "  |  ", env_lbl,
                          "  [", dur_lbl[cvr], "]")

        ymax <- ceiling(max(c(d$WAGT, d$WRR14), na.rm=TRUE)) + 1L
        xmax <- max(d$dat, na.rm=TRUE) + 5L

        plot(NA, xlim=c(0, xmax), ylim=c(0, ymax),
             xlab="Days after sowing",
             ylab="t / ha",
             main=mtitle, las=1,
             cex.main=1.05, cex.axis=0.95, cex.lab=1.0)
        abline(h=seq(0, ymax, by=2), col="grey93")
        abline(v=seq(0, xmax, by=20), col="grey93")
        box()

        for (pm in plant_months_present) {
            dd <- d[d$plant_month==pm, ]
            if (nrow(dd)==0) next
            dd  <- dd[order(dd$dat), ]
            col <- month_pal[pm]

            # Total biomass — dashed, semi-transparent
            lines(dd$dat, dd$WAGT,  col=adjustcolor(col, 0.55), lwd=2.0, lty=2L)
            # Grain yield — solid, bold
            lines(dd$dat, dd$WRR14, col=col, lwd=2.8)
            # Maturity marker: filled circle at peak grain
            pk <- which.max(dd$WRR14)
            if (length(pk)>0 && dd$WRR14[pk]>0.05)
                points(dd$dat[pk], dd$WRR14[pk],
                       pch=21L, bg=col, col="white", cex=2.2, lwd=1.5)
        }
    }
}

# ── Legend panel (panel 9 in layout) ──────────────────────────────────────────
par(mar=c(4.5, 0.5, 3.5, 1.0))
plot.new()

# Month colour swatches
legend(0.0, 1.0,
       legend     = plant_months_present,
       fill       = month_pal[plant_months_present],
       border     = "grey40",
       title      = "Planting month",
       title.font = 2L,
       bty        = "n",
       cex        = 1.0,
       y.intersp  = 1.1,
       xpd        = TRUE)

# Line type key
legend(0.0, 0.28,
       legend  = c("Grain yield (WRR14)",
                   "Total biomass (WAGT)",
                   "Maturity (peak grain)"),
       lty     = c(1L, 2L, NA),
       lwd     = c(2.8, 2.0, NA),
       pch     = c(NA, NA, 21L),
       col     = c("grey25", "grey25", "grey25"),
       pt.bg   = c(NA, NA, "grey25"),
       pt.cex  = c(NA, NA, 1.8),
       bty     = "o",
       bg      = "white",
       box.col = "grey55",
       cex     = 0.92,
       y.intersp = 1.25,
       xpd     = TRUE)

# Title above all panels
mtext("Simulated seasonal trajectories — grain (WRR14, solid) and total biomass (WAGT, dashed)\naveraged across pilot cells and years",
      outer=FALSE, side=3, cex=0.85, font=1, line=0.3, xpd=TRUE)

dev.off()
cat("Trajectory figure saved:", file.path(out_dir, "maps", "pilot_trajectories.png"), "\n")
cat("Done.\n")
