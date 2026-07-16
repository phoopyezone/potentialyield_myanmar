##
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
    path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else if (this == "phoopyezone") {
    path <- "G:/My Drive/Research/simulation/main_oryza/v2"
} else { path <- "." }
setwd(path)

library(terra)
library(dplyr)
library(tidyr)
library(openxlsx)

admin_shp <- "G:/My Drive/Research/simulation/data_myanmar/admin_boundaries/state_regions/mmr_polbnda2_adm1_250k_mimu_1.shp"
out_dir   <- "data/output/pilot"
dir.create(file.path(out_dir, "maps"), FALSE, TRUE)

# ── Load data ──────────────────────────────────────────────────────────────────
x     <- read.csv("data/output/pilot_results.csv")
cells <- readRDS("data/pilot_selected_cells.rds")
admin <- vect(admin_shp)
x     <- merge(x, cells[, c("cell","x","y","state")], by="cell")

# ── 1. Cultivar duration ───────────────────────────────────────────────────────
# Exclude sDOY=259 (September planting): crop hits FINTIM=365 before maturity,
# so eDOY=365 is a truncation artefact, not a real harvest date.
dur_df <- x |>
    filter(WRR14 > 0, as.integer(sDOY) != 259L) |>
    mutate(dur = (eYEAR - sYEAR) * 365L +
                  as.integer(eDOY) - as.integer(sDOY) - 30L) |>
    filter(dur > 50L, dur < 250L) |>
    group_by(ccvr) |>
    summarise(mean_dur = round(mean(dur, na.rm=TRUE)), .groups="drop") |>
    mutate(
        dur_class = case_when(mean_dur <  100 ~ "Short",
                              mean_dur <= 120 ~ "Medium",
                              TRUE            ~ "Long"),
        dur_label = paste0(dur_class, " (~", mean_dur, " d)")
    )
dur_lbl <- setNames(dur_df$dur_label, dur_df$ccvr)
cat("Cultivar durations (Sep planting excluded):\n"); print(dur_df); cat("\n")

# ── 2. Cell averages ───────────────────────────────────────────────────────────
cell_avg <- x |>
    filter(WRR14 > 0) |>
    group_by(cell, x, y, state, eenv, ccvr, run) |>
    summarise(mean_yield = mean(WRR14),  sd_yield = sd(WRR14),
              max_yield  = max(WRR14),   min_yield = min(WRR14),
              n = n(), .groups = "drop")

# ── 3. Map: rows = cultivar, cols = Potential | Water-limited ──────────────────
cvr_order <- c("standard", "mestizo", "local", "RC222")
env_order <- c("potential", "watbal")
pal       <- hcl.colors(10, "YlOrRd")

# Separate colour scale per column (environment)
mk_brk <- function(env) {
    v    <- cell_avg$mean_yield[cell_avg$eenv == env]
    step <- ifelse(env == "potential", 1000, 500)
    top  <- ceiling(max(v, na.rm=TRUE) / step) * step
    seq(0, top, by=step)
}
brk <- list(potential = mk_brk("potential"), watbal = mk_brk("watbal"))

# Legend inside each panel — interval labels in t/ha (like the reference figure)
add_panel_legend <- function(b, pal_full) {
    n   <- length(b) - 1L
    # 6 evenly-spaced bins for readability
    idx <- round(seq(1, n, length.out = min(n, 6L)))
    pal_sub <- colorRampPalette(pal_full)(n)[idx]
    lbl <- paste0(round(b[idx] / 1000, 1), " – ",
                  round(b[idx + 1L] / 1000, 1), " t/ha")
    legend("topright",
           legend     = lbl,
           fill       = pal_sub,
           border     = "grey40",
           bty        = "o",
           bg         = "white",
           box.col    = "grey55",
           cex        = 0.95,
           y.intersp  = 1.0,
           title      = "Mean yield",
           title.font = 2,
           inset      = 0.01)
}

# Layout: 4 map rows × 2 cols (no separate legend row — legend inside panels)
png(file.path(out_dir, "maps", "pilot_yield_by_run.png"),
    width = 2000, height = 3800, res = 150)
par(mfrow = c(4L, 2L), mar = c(0.5, 0.5, 3.5, 0.5), oma = c(1, 1, 2, 1))

for (cvr in cvr_order) {
    for (env in env_order) {
        d       <- cell_avg[cell_avg$ccvr == cvr & cell_avg$eenv == env, ]
        env_lbl <- ifelse(env == "potential", "Potential", "Water-limited")
        mtitle  <- paste0(tools::toTitleCase(cvr), "  |  ", env_lbl,
                          "  [", dur_lbl[cvr], "]")
        plot(admin, border="grey55", col="grey96",
             main=mtitle, cex.main=1.05)
        if (nrow(d) > 0) {
            b  <- brk[[env]]
            n  <- length(b) - 1L
            ci <- as.integer(cut(d$mean_yield, breaks=b, include.lowest=TRUE))
            ci[is.na(ci)] <- 1L
            ci <- pmin(ci, n)
            col_ramp <- colorRampPalette(pal)(n)
            points(d$x, d$y, pch=21, bg=col_ramp[ci], col="grey20", cex=3.2)
            text(d$x, d$y, round(d$mean_yield / 1000, 1),
                 cex=0.85, font=2)
        }
        add_panel_legend(brk[[env]], pal)
    }
}
mtext("Simulated mean rice yield — pilot cells",
      outer=TRUE, cex=1.3, font=2, line=0.5)
dev.off()
cat("Map saved\n")

# ── 4. Monthly yield figure ────────────────────────────────────────────────────
doy_to_month <- function(doy) {
    findInterval(as.integer(doy),
                 c(1L,32L,60L,91L,121L,152L,182L,213L,244L,274L,305L,335L))
}
month_lbl <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
               "Oct","Nov","Dec")
cvr_cols <- c(standard="#1B7837", mestizo="#762A83",
              local="#E08214",    RC222="#4393C3")
cvr_pch  <- c(standard=19L, mestizo=17L, local=15L, RC222=18L)

monthly <- x |>
    filter(WRR14 > 0) |>
    mutate(plant_month = doy_to_month(sDOY)) |>
    group_by(eenv, ccvr, plant_month) |>
    summarise(mean_yield = mean(WRR14, na.rm=TRUE) / 1000,
              se_yield   = sd(WRR14,   na.rm=TRUE) / sqrt(n()) / 1000,
              .groups = "drop")

png(file.path(out_dir, "maps", "pilot_monthly_yield.png"),
    width=2200, height=950, res=150)
par(mfrow=c(1,2), mar=c(5,6,4,2), oma=c(0,0,3,15))

for (env in env_order) {
    d       <- monthly[monthly$eenv == env, ]
    ymax    <- ceiling(max(d$mean_yield + d$se_yield, na.rm=TRUE))
    ymax    <- max(ymax, 2L)
    env_lbl <- ifelse(env=="potential",
                      "Potential production", "Water-limited (rainfed)")

    plot(NA, xlim=c(0.5, 9.5), ylim=c(0, ymax),
         xlab="Planting month", ylab="Mean yield (t/ha)",
         main=env_lbl, xaxt="n", las=1,
         cex.main=1.2, cex.axis=1.1, cex.lab=1.15)
    axis(1, at=1:9, labels=month_lbl[1:9], cex.axis=1.1)
    abline(h=seq(0, ymax, by=1), col="grey90")
    abline(v=1:9, col="grey94")
    box()

    for (cvr in cvr_order) {
        dd <- d[d$ccvr == cvr, ]
        if (nrow(dd) == 0) next
        dd <- dd[order(dd$plant_month), ]
        polygon(c(dd$plant_month, rev(dd$plant_month)),
                c(dd$mean_yield + dd$se_yield,
                  rev(dd$mean_yield - dd$se_yield)),
                col=adjustcolor(cvr_cols[cvr], 0.15), border=NA)
        lines(dd$plant_month,  dd$mean_yield,
              col=cvr_cols[cvr], lwd=2.5)
        points(dd$plant_month, dd$mean_yield,
               col=cvr_cols[cvr], pch=cvr_pch[cvr], cex=1.5)
    }
}

# Legend in right outer margin
par(xpd=NA)
usr <- par("usr")
lx0 <- usr[2] + 0.55
ly0 <- usr[4]
ldy <- (usr[4] - usr[3]) / (length(cvr_order) + 0.3)
for (k in seq_along(cvr_order)) {
    cvr <- cvr_order[k]
    yy  <- ly0 - (k - 0.5) * ldy
    lines(c(lx0, lx0+0.5), c(yy,yy), col=cvr_cols[cvr], lwd=3, xpd=NA)
    points(lx0+0.25, yy, col=cvr_cols[cvr], pch=cvr_pch[cvr], cex=1.6, xpd=NA)
    text(lx0+0.65, yy,
         paste0(tools::toTitleCase(cvr), "\n[", dur_lbl[cvr], "]"),
         adj=0, cex=0.95, xpd=NA)
}
mtext("Average monthly yield by cultivar and production environment",
      outer=TRUE, cex=1.3, font=2)
dev.off()
cat("Monthly figure saved\n")

# ── 5. Descriptive statistics ──────────────────────────────────────────────────
state_stats <- cell_avg |>
    group_by(State=state, Environment=eenv, Cultivar=ccvr) |>
    summarise(`Mean Yield (kg/ha)` = round(mean(mean_yield), 0),
              `SD (kg/ha)`         = round(mean(sd_yield, na.rm=TRUE), 0),
              `Min (kg/ha)`        = round(min(min_yield), 0),
              `Max (kg/ha)`        = round(max(max_yield), 0),
              .groups="drop") |>
    arrange(State, Environment, Cultivar)

state_wide <- cell_avg |>
    group_by(State=state, run) |>
    summarise(mean_yield=round(mean(mean_yield), 0), .groups="drop") |>
    pivot_wider(names_from=run, values_from=mean_yield)

cell_out <- cell_avg |>
    select(Cell=cell, Lon=x, Lat=y, State=state,
           Environment=eenv, Cultivar=ccvr,
           `Mean (kg/ha)`=mean_yield, `SD (kg/ha)`=sd_yield,
           `Min (kg/ha)`=min_yield,   `Max (kg/ha)`=max_yield, N=n) |>
    mutate(across(where(is.numeric), round)) |>
    arrange(State, Environment, Cultivar)

dur_table <- dur_df |>
    select(Cultivar=ccvr,
           `Duration Class`=dur_class,
           `Mean Days (transplant to maturity, Sep excluded)`=mean_dur,
           Label=dur_label)

# ── 6. Literature comparison ───────────────────────────────────────────────────
sim_state <- x |>
    filter(WRR14 > 0) |>
    group_by(State=state, eenv) |>
    summarise(`Sim Mean (kg/ha)` = round(mean(WRR14, na.rm=TRUE)), .groups="drop") |>
    pivot_wider(names_from=eenv, values_from=`Sim Mean (kg/ha)`,
                names_prefix="Sim_") |>
    rename(`Sim Potential (kg/ha)` = Sim_potential,
           `Sim Rainfed (kg/ha)`   = Sim_watbal)

gyga_note <- "GYGA Myanmar: national range 8-12 t/ha; region-specific value not reported"
rain_gyga <- "GYGA Myanmar attainable rainfed ~5.5-6.5 t/ha nationally; region-specific not reported"

lit <- data.frame(
    State = c("Ayeyarwady","Bago (East)","Bago (West)","Mandalay",
              "Nay Pyi Taw","Sagaing","Shan (South)","Yangon"),
    `Lit Potential (kg/ha)` = c(9000,   NA,   NA, NA, NA, NA, NA, NA),
    `Lit Potential Source`  = c(
        "Laborte et al. 2019 (Field Crops Res.): Ayeyarwady ~9 t/ha; GYGA 8-12 t/ha",
        gyga_note, gyga_note, gyga_note,
        gyga_note, gyga_note, gyga_note, gyga_note),
    `Lit Rainfed (kg/ha)`   = c(4300, NA, NA, 2500, NA, NA, NA, NA),
    `Lit Rainfed Source`    = c(
        "Laborte et al. 2019: monsoon farmer yield 3.2-4.3 t/ha, Ayeyarwady delta",
        rain_gyga, rain_gyga,
        "Laborte et al. 2019: central dry zone approx. 2-3 t/ha",
        rain_gyga, rain_gyga, rain_gyga, rain_gyga),
    check.names = FALSE
)

lit_compare <- merge(sim_state, lit, by="State", all.x=TRUE) |>
    select(State,
           `Sim Potential (kg/ha)`, `Lit Potential (kg/ha)`, `Lit Potential Source`,
           `Sim Rainfed (kg/ha)`,   `Lit Rainfed (kg/ha)`,   `Lit Rainfed Source`) |>
    arrange(State)

# ── 7. Write Excel ─────────────────────────────────────────────────────────────
hdr_blue <- createStyle(textDecoration="bold", fgFill="#2E75B6",
                        fontColour="white", halign="center")
hdr_grn  <- createStyle(textDecoration="bold", fgFill="#375623",
                        fontColour="white", halign="center")
wrap_top <- createStyle(wrapText=TRUE, valign="top")

wb <- createWorkbook()
sheet_list <- list(
    `Cultivar Duration` = list(data=dur_table,   hdr=hdr_grn,  sty="TableStyleMedium9"),
    `State Summary`     = list(data=state_stats,  hdr=hdr_blue, sty="TableStyleMedium9"),
    `State Wide`        = list(data=state_wide,   hdr=hdr_blue, sty="TableStyleMedium9"),
    `Cell Level`        = list(data=cell_out,     hdr=hdr_blue, sty="TableStyleLight1"),
    `Lit Comparison`    = list(data=lit_compare,  hdr=hdr_grn,  sty="TableStyleMedium2")
)
for (nm in names(sheet_list)) {
    addWorksheet(wb, nm)
    s <- sheet_list[[nm]]
    writeDataTable(wb, nm, s$data, tableStyle=s$sty)
    nr <- nrow(s$data)
    nc <- ncol(s$data)
    addStyle(wb, nm, s$hdr,     rows=1,       cols=1:nc, gridExpand=TRUE)
    addStyle(wb, nm, wrap_top,  rows=2:(nr+1), cols=1:nc, gridExpand=TRUE)
    setColWidths(wb, nm, cols=1:nc, widths="auto")
}
saveWorkbook(wb, file.path(out_dir, "pilot_results_by_state.xlsx"), overwrite=TRUE)
cat("Excel saved\n")
cat("Done.\n")
