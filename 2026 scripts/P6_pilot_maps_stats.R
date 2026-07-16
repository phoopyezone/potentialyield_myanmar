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

# ── Helper functions ───────────────────────────────────────────────────────────
doy_to_month <- function(doy) {
    findInterval(as.integer(doy),
                 c(1L,32L,60L,91L,121L,152L,182L,213L,244L,274L,305L,335L))
}
month_lbl <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
               "Oct","Nov","Dec")

cvr_order <- c("standard","mestizo","local","RC222")
env_order <- c("potential","watbal")
cvr_cols  <- c(standard="#1B7837", mestizo="#762A83",
               local="#E08214",    RC222="#4393C3")
cvr_pch   <- c(standard=19L, mestizo=17L, local=15L, RC222=18L)
pal_map   <- hcl.colors(10, "YlOrRd")

# ── Load data ──────────────────────────────────────────────────────────────────
x     <- read.csv("data/output/pilot_results.csv")
cells <- readRDS("data/pilot_selected_cells.rds")
admin <- vect(admin_shp)
x     <- merge(x, cells[, c("cell","x","y","state")], by="cell")

# ── 1. Cultivar duration — reported as SOWING to maturity ─────────────────────
# STTIME (sDOY) = sowing date; EMD = sDOY+7 (emergence); transplanting = sDOY+30
# dur_tp = transplanting→maturity (class basis); dur_sow = sowing→maturity (+30)
# Duration class uses transplanting-to-maturity (standard rice agronomy convention):
#   Short <100 d, Medium 100-120 d, Long >120 d
dur_df <- x |>
    filter(WRR14 > 0, as.integer(sDOY) != 259L) |>
    mutate(dur_tp = (eYEAR - sYEAR) * 365L +
                    as.integer(eDOY) - as.integer(sDOY) - 30L) |>
    filter(dur_tp > 50L, dur_tp < 250L) |>
    group_by(ccvr) |>
    summarise(dur_tp = round(mean(dur_tp, na.rm=TRUE)), .groups="drop") |>
    mutate(
        dur_sow   = dur_tp + 30L,
        dur_class = case_when(dur_tp <  100 ~ "Short",
                              dur_tp <= 120  ~ "Medium",
                              TRUE           ~ "Long"),
        dur_label = paste0(dur_class, " (~", dur_sow, " d)")   # sowing-to-maturity
    )
dur_lbl <- setNames(dur_df$dur_label, dur_df$ccvr)

cat("Cultivar durations (sowing to maturity):\n"); print(dur_df); cat("\n")

# ── 2. Monthly cell means: mean across years per cell × planting DOY ───────────
monthly_cell <- x |>
    filter(WRR14 > 0) |>
    group_by(cell, x, y, state, eenv, ccvr, sDOY) |>
    summarise(
        mean_yield = mean(WRR14,  na.rm=TRUE),
        sd_yield   = sd(WRR14,    na.rm=TRUE),
        n_years    = n(),
        .groups    = "drop"
    )

# ── 3. Optimal planting date per cell (GYGA Yp / Yw definition) ───────────────
cell_best <- monthly_cell |>
    group_by(cell, x, y, state, eenv, ccvr) |>
    slice_max(mean_yield, n=1L, with_ties=FALSE) |>
    ungroup() |>
    rename(opt_DOY=sDOY, opt_yield=mean_yield,
           opt_sd=sd_yield, opt_n=n_years) |>
    mutate(opt_month = month_lbl[doy_to_month(opt_DOY)])

cat("Optimal sowing months (cell level):\n")
print(cell_best[, c("cell","state","eenv","ccvr","opt_month","opt_yield")])
cat("\n")

# ── 4a. State-level monthly yield (for grid maps) ─────────────────────────────
# Average across cells within each state × env × cultivar × sowing month
state_monthly <- monthly_cell |>
    mutate(plant_month_n = doy_to_month(sDOY),
           plant_month   = month_lbl[plant_month_n]) |>
    filter(plant_month_n >= 1L, plant_month_n <= 9L) |>
    group_by(state, eenv, ccvr, plant_month_n, plant_month) |>
    summarise(yield_tha = mean(mean_yield, na.rm=TRUE) / 1000, .groups="drop")

# Optimal sowing month per env × cultivar (pooled across all states)
opt_month_df <- state_monthly |>
    group_by(eenv, ccvr, plant_month_n, plant_month) |>
    summarise(mean_tha = mean(yield_tha, na.rm=TRUE), .groups="drop") |>
    group_by(eenv, ccvr) |>
    slice_max(mean_tha, n=1L, with_ties=FALSE) |>
    select(eenv, ccvr, opt_month=plant_month) |>
    ungroup()

cat("Optimal sowing months (grid figure):\n"); print(opt_month_df); cat("\n")

# Optimal sowing month per STATE × env × cultivar (for per-state ★ markers)
state_opt_month <- state_monthly |>
    group_by(state, eenv, ccvr) |>
    slice_max(yield_tha, n=1L, with_ties=FALSE) |>
    select(state, eenv, ccvr, opt_month=plant_month) |>
    ungroup()

cat("State-level optimal sowing months:\n"); print(state_opt_month); cat("\n")

# ── 4b. State-level yield gap: Yp − Yw ────────────────────────────────────────
gap_df <- cell_best |>
    group_by(state, ccvr, eenv) |>
    summarise(mean_opt=mean(opt_yield, na.rm=TRUE), .groups="drop") |>
    pivot_wider(names_from=eenv, values_from=mean_opt) |>
    mutate(
        Yp      = potential / 1000,
        Yw      = watbal    / 1000,
        gap     = Yp - Yw,
        gap_pct = round(gap / Yp * 100, 1)
    ) |>
    filter(!is.na(Yp), !is.na(Yw))

# ── 5. State name field from shapefile ────────────────────────────────────────
adm_df     <- as.data.frame(admin)
name_field <- if ("ST"         %in% names(adm_df)) "ST"         else
              if ("State_Name" %in% names(adm_df)) "State_Name" else
              if ("NAME_1"     %in% names(adm_df)) "NAME_1"     else names(adm_df)[2]
adm_names  <- adm_df[[name_field]]
cat("Shapefile state name field:", name_field, "\n")
cat("Shapefile states:", paste(sort(adm_names), collapse=" | "), "\n\n")

# Lookup: our gap_df$state labels → shapefile state name
# Adjust the right-hand values if the shapefile uses different spellings
state_lookup <- c(
    "Ayeyarwady"   = "Ayeyarwady",
    "Yangon"       = "Yangon",
    "Bago (West)"  = "Bago (West)",
    "Bago (East)"  = "Bago (East)",
    "Nay Pyi Taw"  = "Nay Pyi Taw",
    "Mandalay"     = "Mandalay",
    "Shan (South)" = "Shan (South)",
    "Sagaing"      = "Sagaing"
)
# If the exact name is not found in the shapefile, fall back to partial matching
fix_lookup <- function(lu, actual_names) {
    vapply(lu, function(target) {
        if (target %in% actual_names) return(target)
        hit <- actual_names[grepl(strsplit(target," \\(")[[1]][1],
                                  actual_names, ignore.case=TRUE)]
        if (length(hit)==1L) hit else target
    }, character(1))
}
state_lookup <- fix_lookup(state_lookup, adm_names)
rev_lookup   <- setNames(names(state_lookup), state_lookup)   # shp name → our label

# ── MAP HELPERS ────────────────────────────────────────────────────────────────
mk_brk <- function(env) {
    v    <- cell_best$opt_yield[cell_best$eenv == env]
    step <- ifelse(env=="potential", 1000, 500)
    top  <- ceiling(max(v, na.rm=TRUE) / step) * step
    seq(0, top, by=step)
}
brk <- list(potential=mk_brk("potential"), watbal=mk_brk("watbal"))

add_yield_legend <- function(b, pal_full, title="Opt. yield") {
    n   <- length(b) - 1L
    idx <- round(seq(1, n, length.out=min(n, 6L)))
    pal_sub <- colorRampPalette(pal_full)(n)[idx]
    lbl <- paste0(round(b[idx]/1000, 1), " – ", round(b[idx+1L]/1000, 1), " t/ha")
    legend("topright", legend=lbl, fill=pal_sub,
           border="grey40", bty="o", bg="white", box.col="grey55",
           cex=0.95, y.intersp=1.0, title=title, title.font=2, inset=0.01)
}

# ── FIGURE A: Yield at optimal sowing date — 4 × 2 map ────────────────────────
png(file.path(out_dir, "maps", "pilot_yield_by_run.png"),
    width=2000, height=3800, res=150)
par(mfrow=c(4L,2L), mar=c(0.5,0.5,3.5,0.5), oma=c(1,1,2,1))
for (cvr in cvr_order) {
    for (env in env_order) {
        d       <- cell_best[cell_best$ccvr==cvr & cell_best$eenv==env, ]
        env_lbl <- ifelse(env=="potential","Potential","Water-limited")
        mtitle  <- paste0(tools::toTitleCase(cvr), "  |  ", env_lbl,
                          "  [", dur_lbl[cvr], "]")
        plot(admin, border="grey55", col="grey96", main=mtitle, cex.main=1.05)
        if (nrow(d)>0) {
            b  <- brk[[env]];  n <- length(b)-1L
            ci <- as.integer(cut(d$opt_yield, breaks=b, include.lowest=TRUE))
            ci[is.na(ci)] <- 1L;  ci <- pmin(ci, n)
            col_ramp <- colorRampPalette(pal_map)(n)
            points(d$x, d$y, pch=21, bg=col_ramp[ci], col="grey20", cex=3.2)
            text(d$x, d$y, round(d$opt_yield/1000, 1), cex=0.85, font=2)
        }
        add_yield_legend(brk[[env]], pal_map)
    }
}
mtext("Simulated rice yield at optimal sowing date (Yp / Yw) — pilot cells",
      outer=TRUE, cex=1.3, font=2, line=0.5)
dev.off()
cat("Figure A saved: pilot_yield_by_run.png\n")

# ── FIGURE B: Optimal sowing month map — 4 × 2 ────────────────────────────────
present_months <- month_lbl[sort(unique(doy_to_month(cell_best$opt_DOY)))]
month_pal <- setNames(hcl.colors(length(present_months), "Dark 3"), present_months)

png(file.path(out_dir, "maps", "pilot_optimal_month.png"),
    width=2000, height=3800, res=150)
par(mfrow=c(4L,2L), mar=c(0.5,0.5,3.5,0.5), oma=c(1,1,2,1))
for (cvr in cvr_order) {
    for (env in env_order) {
        d       <- cell_best[cell_best$ccvr==cvr & cell_best$eenv==env, ]
        env_lbl <- ifelse(env=="potential","Potential","Water-limited")
        mtitle  <- paste0(tools::toTitleCase(cvr), "  |  ", env_lbl)
        plot(admin, border="grey55", col="grey96", main=mtitle, cex.main=1.05)
        if (nrow(d)>0) {
            pt_col <- month_pal[d$opt_month];  pt_col[is.na(pt_col)] <- "grey70"
            points(d$x, d$y, pch=21, bg=pt_col, col="grey20", cex=3.5)
            text(d$x, d$y, d$opt_month, cex=0.72, font=2)
            shown <- intersect(present_months, d$opt_month)
            legend("topright", legend=shown, fill=month_pal[shown],
                   border="grey40", bty="o", bg="white", box.col="grey55",
                   cex=0.95, y.intersp=1.0,
                   title="Opt. sowing", title.font=2, inset=0.01)
        }
    }
}
mtext("Optimal sowing month (highest 10-yr mean yield) by cultivar and environment",
      outer=TRUE, cex=1.3, font=2, line=0.5)
dev.off()
cat("Figure B saved: pilot_optimal_month.png\n")

# ── FIGURE C: Yield gap dumbbell — 2 × 2 ─────────────────────────────────────
yp_max <- ceiling(max(gap_df$Yp, na.rm=TRUE)) + 0.5

png(file.path(out_dir, "maps", "pilot_yield_gap.png"),
    width=2400, height=1800, res=150)
par(mfrow=c(2L,2L), mar=c(5,10,3.5,2), oma=c(1,1,3,1))
for (cvr in cvr_order) {
    d   <- gap_df[gap_df$ccvr==cvr, ];  d <- d[order(d$state), ]
    nr  <- nrow(d);  col <- cvr_cols[cvr]
    plot(NA, xlim=c(0, yp_max), ylim=c(0.4, nr+0.6),
         xlab="Yield (t/ha)", ylab="",
         main=paste0(tools::toTitleCase(cvr), "  [", dur_lbl[cvr], "]"),
         yaxt="n", las=1, cex.main=1.1, cex.axis=1.0, cex.lab=1.05)
    axis(2, at=seq_len(nr), labels=d$state, las=1, cex.axis=0.95)
    abline(v=seq(0, yp_max, by=2), col="grey93")
    abline(h=seq_len(nr), col="grey93", lty=3)
    box()
    for (j in seq_len(nr)) {
        yp <- d$Yp[j];  yw <- d$Yw[j]
        if (is.na(yp)||is.na(yw)) next
        rect(yw, j-0.22, yp, j+0.22, col=adjustcolor(col,0.20), border=NA)
        lines(c(yw,yp), c(j,j), col=col, lwd=2.5)
        points(yw, j, pch=21, bg="white", col=col, cex=2.5, lwd=2)
        points(yp, j, pch=21, bg=col,     col=col, cex=2.5)
        text((yw+yp)/2, j+0.33,
             paste0("Δ", round(d$gap[j],1), " t/ha  (",d$gap_pct[j],"%)"),
             cex=0.82, col=col, font=2)
    }
    if (identical(cvr, cvr_order[1]))
        legend("bottomright",
               legend=c("Yw — water-limited","Yp — potential"),
               pch=21, pt.bg=c("white","grey30"), col="grey30",
               pt.lwd=2, pt.cex=1.8, bty="o", bg="white", box.col="grey60",
               cex=0.95, inset=0.02)
}
mtext("Yield gap (Yp − Yw) at optimal sowing date — pilot states",
      outer=TRUE, cex=1.3, font=2)
dev.off()
cat("Figure C saved: pilot_yield_gap.png\n")

# ── FIGURE D: Monthly yield profile ───────────────────────────────────────────
monthly_agg <- x |>
    filter(WRR14 > 0) |>
    mutate(plant_month=doy_to_month(sDOY)) |>
    group_by(eenv, ccvr, plant_month) |>
    summarise(mean_yield = mean(WRR14, na.rm=TRUE)/1000,
              se_yield   = sd(WRR14,   na.rm=TRUE)/sqrt(n())/1000,
              .groups="drop")

png(file.path(out_dir, "maps", "pilot_monthly_yield.png"),
    width=2200, height=950, res=150)
par(mfrow=c(1,2), mar=c(5,6,4,2), oma=c(0,0,3,15))
for (env in env_order) {
    d       <- monthly_agg[monthly_agg$eenv==env, ]
    ymax    <- ceiling(max(d$mean_yield+d$se_yield, na.rm=TRUE));  ymax <- max(ymax,2L)
    env_lbl <- ifelse(env=="potential","Potential production","Water-limited (rainfed)")
    plot(NA, xlim=c(0.5,9.5), ylim=c(0,ymax),
         xlab="Sowing month", ylab="Mean yield (t/ha)",
         main=env_lbl, xaxt="n", las=1,
         cex.main=1.2, cex.axis=1.1, cex.lab=1.15)
    axis(1, at=1:9, labels=month_lbl[1:9], cex.axis=1.1)
    abline(h=seq(0,ymax,by=1), col="grey90")
    abline(v=1:9, col="grey94")
    box()
    for (cvr in cvr_order) {
        dd <- d[d$ccvr==cvr, ];  if(nrow(dd)==0) next
        dd <- dd[order(dd$plant_month), ]
        polygon(c(dd$plant_month, rev(dd$plant_month)),
                c(dd$mean_yield+dd$se_yield, rev(dd$mean_yield-dd$se_yield)),
                col=adjustcolor(cvr_cols[cvr],0.15), border=NA)
        lines(dd$plant_month,  dd$mean_yield, col=cvr_cols[cvr], lwd=2.5)
        points(dd$plant_month, dd$mean_yield, col=cvr_cols[cvr], pch=cvr_pch[cvr], cex=1.5)
    }
}
par(xpd=NA)
usr <- par("usr");  lx0 <- usr[2]+0.55;  ly0 <- usr[4]
ldy <- (usr[4]-usr[3])/(length(cvr_order)+0.3)
for (k in seq_along(cvr_order)) {
    cvr <- cvr_order[k];  yy <- ly0-(k-0.5)*ldy
    lines(c(lx0,lx0+0.5), c(yy,yy), col=cvr_cols[cvr], lwd=3, xpd=NA)
    points(lx0+0.25, yy, col=cvr_cols[cvr], pch=cvr_pch[cvr], cex=1.6, xpd=NA)
    text(lx0+0.65, yy,
         paste0(tools::toTitleCase(cvr),"\n[",dur_lbl[cvr],"]"),
         adj=0, cex=0.95, xpd=NA)
}
mtext("Yield by sowing month — all pilot cells pooled (shading = ±1 SE)",
      outer=TRUE, cex=1.3, font=2)
dev.off()
cat("Figure D saved: pilot_monthly_yield.png\n")

# ── FIGURES E & F: State choropleth maps — Yp and Yw ─────────────────────────
# Each figure: 2×2 panels (one per cultivar) + horizontal colour bar at bottom
# State polygons filled by yield (t/ha); grey = no pilot data for that state
choro_pal <- colorRampPalette(c("#FFFFD4","#FEC44F","#D95F0E","#7F0000"))(100)

# Pre-compute polygon centroids (inside=TRUE keeps point within polygon)
ctr     <- centroids(admin, inside=TRUE)
ctr_xy  <- as.data.frame(geom(ctr))[, c("x","y")]
ctr_xy$shp_name <- adm_names

draw_state_choro <- function(env, out_file) {
    env_lbl   <- ifelse(env=="potential",
                        "Potential Production (Yp)", "Water-Limited Rainfed (Yw)")
    yield_col <- ifelse(env=="potential", "Yp", "Yw")

    d         <- gap_df[, c("state","ccvr", yield_col)]
    names(d)[3] <- "yield"

    # Shared colour scale: same range across all 4 panels
    vmin  <- 0
    vmax  <- ceiling(max(d$yield, na.rm=TRUE) * 2) / 2
    brks  <- seq(vmin, vmax, length.out=101L)

    yield_col_fn <- function(yval) {
        if (is.na(yval)) return("grey92")
        idx <- findInterval(yval, brks, rightmost.closed=TRUE)
        choro_pal[min(max(idx, 1L), 100L)]
    }

    png(out_file, width=2400, height=3400, res=150)
    layout(matrix(c(1L,2L,3L,4L,5L,5L), nrow=3L, byrow=TRUE),
           heights=c(1, 1, 0.18))

    for (cvr in cvr_order) {
        par(mar=c(0.5, 0.5, 3.2, 0.5))
        dc <- d[d$ccvr==cvr, ]

        # Build fill colour for every polygon
        poly_fill <- vapply(seq_along(adm_names), function(i) {
            shp_nm    <- adm_names[i]
            our_state <- rev_lookup[shp_nm]
            if (is.na(our_state)) return("grey92")
            yval      <- dc$yield[dc$state==our_state]
            if (length(yval)==0L || is.na(yval[1L])) return("grey92")
            yield_col_fn(yval[1L])
        }, character(1))

        plot(admin, col=poly_fill, border="white", lwd=1.4,
             main=paste0(tools::toTitleCase(cvr), "  [", dur_lbl[cvr], "]"),
             cex.main=1.15)
        plot(admin, col=NA, border="grey50", lwd=0.6, add=TRUE)   # thin outline

        # State label + yield at centroid
        for (i in seq_along(adm_names)) {
            shp_nm    <- adm_names[i]
            our_state <- rev_lookup[shp_nm]
            if (is.na(our_state)) next
            yval <- dc$yield[dc$state==our_state]
            if (length(yval)==0L || is.na(yval[1L])) next
            cx <- ctr_xy$x[i];  cy <- ctr_xy$y[i]
            dark_bg <- yval[1L] / vmax > 0.55
            txt_col <- ifelse(dark_bg, "white", "grey10")
            text(cx, cy,
                 paste0(our_state, "\n", sprintf("%.1f", yval[1L]), " t/ha"),
                 cex=0.72, font=2, col=txt_col)
        }
    }

    # ── Horizontal colour bar ────────────────────────────────────────────────
    par(mar=c(3.8, 8, 1.2, 8))
    plot(NA, xlim=c(vmin, vmax), ylim=c(0,1), axes=FALSE, xlab="", ylab="")
    bar_x <- seq(vmin, vmax, length.out=101L)
    for (k in seq_len(100L))
        rect(bar_x[k], 0, bar_x[k+1L], 1, col=choro_pal[k], border=NA)
    rect(vmin, 0, vmax, 1, col=NA, border="grey40", lwd=1.5)
    tick_at <- pretty(c(vmin, vmax), n=6)
    tick_at <- tick_at[tick_at>=vmin & tick_at<=vmax]
    axis(1, at=tick_at, labels=paste0(tick_at, " t/ha"),
         cex.axis=1.15, font=2, lwd=0, lwd.ticks=1.2)
    mtext("Low yield",  side=1, at=vmin, adj=0, line=2.6, cex=1.0, col="grey50")
    mtext("High yield", side=1, at=vmax, adj=1, line=2.6, cex=1.0, col="grey50")
    mtext(paste0("Mean yield (t/ha) — ", env_lbl),
          side=3, line=0, cex=1.05, font=2)

    mtext(paste0("Simulated rice yield at optimal sowing date — ", env_lbl,
                 "\n(mean across pilot cells per state)"),
          outer=TRUE, cex=1.2, font=2, line=-1.2)
    dev.off()
    cat("State choropleth saved:", basename(out_file), "\n")
}

draw_state_choro("potential",
                 file.path(out_dir, "maps", "pilot_map_potential.png"))
draw_state_choro("watbal",
                 file.path(out_dir, "maps", "pilot_map_rainfed.png"))

# ── FIGURES G & H: 4 cultivar rows × 9 sowing-month columns of Myanmar maps ──
# Each cell = state choropleth for that cultivar × month combination
# Shared colour scale within figure; gold border + ★ = optimal sowing month
grid_pal <- colorRampPalette(c("#FFFFD4","#FEC44F","#D95F0E","#7F0000"))(100)

draw_grid_choro <- function(env, out_file) {
    env_lbl <- ifelse(env == "potential",
                      "Potential Production (Yp)",
                      "Water-Limited Rainfed (Yw)")
    months  <- month_lbl[1:9]   # Jan–Sep
    d       <- state_monthly[state_monthly$eenv == env, ]

    # Global colour scale (same across all 36 panels)
    vmin <- 0
    vmax <- ceiling(max(d$yield_tha, na.rm=TRUE) * 2) / 2
    brks <- seq(vmin, vmax, length.out=101L)

    col_fn <- function(yval) {
        if (is.na(yval) || yval <= 0) return("grey93")
        grid_pal[min(max(findInterval(yval, brks, rightmost.closed=TRUE), 1L), 100L)]
    }

    # ── Layout ────────────────────────────────────────────────────────────────
    # Row 1 : [0 corner] [1–9  month-header cells]
    # Rows 2–5: [10/20/30/40 cvr-label] [11–19 / 21–29 / 31–39 / 41–49 map cells]
    # Row 6 : [50  colorbar, full width]
    lay_mat <- matrix(c(
         0L,  1L,  2L,  3L,  4L,  5L,  6L,  7L,  8L,  9L,
        10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L,
        20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L,
        30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L,
        40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L,
        50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L
    ), nrow=6L, ncol=10L, byrow=TRUE)

    png(out_file, width=3800, height=3200, res=150)
    par(oma=c(0.5, 0.5, 3.0, 0.5))
    layout(lay_mat,
           widths  = c(0.55, rep(1, 9)),
           heights = c(0.20, 1, 1, 1, 1, 0.24))

    # ── Month header panels 1–9 ───────────────────────────────────────────────
    for (pm in months) {
        par(mar=c(0,0,0.2,0))
        plot.new()
        text(0.5, 0.45, pm, cex=2.5, font=2, adj=c(0.5,0.5))
    }

    # ── Cultivar rows (label + 9 maps each) ──────────────────────────────────
    for (cvr in cvr_order) {
        opt_m  <- opt_month_df$opt_month[opt_month_df$eenv==env &
                                         opt_month_df$ccvr==cvr]
        if (!length(opt_m)) opt_m <- ""
        dc_cvr <- d[d$ccvr == cvr, ]

        # Cultivar label cell (panels 10 / 20 / 30 / 40)
        par(mar=c(0,0,0,0))
        plot.new()
        text(0.5, 0.5,
             paste0(tools::toTitleCase(cvr), "\n[", dur_lbl[cvr], "]"),
             cex=2.5, font=2, adj=c(0.5, 0.5), srt=90)

        # 9 map panels for this cultivar
        for (pm in months) {
            is_opt <- identical(pm, opt_m)
            par(mar=c(0.1, 0.1, 0.1, 0.1))
            dc <- dc_cvr[dc_cvr$plant_month == pm, ]

            poly_fill <- vapply(seq_along(adm_names), function(i) {
                our_st <- rev_lookup[adm_names[i]]
                if (is.na(our_st)) return("grey93")
                yv <- dc$yield_tha[dc$state == our_st]
                if (!length(yv) || is.na(yv[1L])) return("grey93")
                col_fn(yv[1L])
            }, character(1L))

            plot(admin, col=poly_fill, border="white", lwd=0.5)
            plot(admin, col=NA, border="grey60", lwd=0.25, add=TRUE)

            # Yield value + per-state optimal-month star at each pilot state centroid
            usr <- par("usr")
            y_star_off <- (usr[4] - usr[3]) * 0.065   # ~6.5% of y-range above text

            for (j in seq_along(adm_names)) {
                our_st <- rev_lookup[adm_names[j]]
                if (is.na(our_st)) next
                yv <- dc$yield_tha[dc$state == our_st]
                if (!length(yv) || is.na(yv[1L]) || yv[1L] <= 0) next
                cx <- ctr_xy$x[j];  cy <- ctr_xy$y[j]
                txt_col <- ifelse(yv[1L]/vmax > 0.55, "white", "grey10")

                # Yield number (3× larger than original)
                text(cx, cy, sprintf("%.1f", yv[1L]),
                     cex=1, font=2, col=txt_col)

                # Red ★ if this month is optimal for this specific state
                st_opt <- state_opt_month$opt_month[
                    state_opt_month$state == our_st &
                    state_opt_month$eenv  == env    &
                    state_opt_month$ccvr  == cvr]
                if (length(st_opt) > 0L && identical(st_opt[1L], pm))
                    text(cx, cy + y_star_off, "★",
                         cex=1.5, col="red", font=2)
            }

            # Blue border + ★ for optimal month
            if (is_opt) {
              box(col="#1565C0", lwd=5.5)
              usr <- par("usr")
              text(usr[2] - 0.03*(usr[2]-usr[1]),
                   usr[4] - 0.05*(usr[4]-usr[3]),
                   "★",   # ★ Unicode star
                   cex=1.6, col="#1565C0", adj=c(1,1), font=2)
            }
        }
    }

    # ── Colorbar panel 50 ────────────────────────────────────────────────────
    par(mar=c(4.8, 11, 2.2, 11))
    plot(NA, xlim=c(vmin,vmax), ylim=c(0,1), axes=FALSE, xlab="", ylab="")
    bar_x <- seq(vmin, vmax, length.out=101L)
    for (k in seq_len(100L))
        rect(bar_x[k], 0, bar_x[k+1L], 1, col=grid_pal[k], border=NA)
    rect(vmin, 0, vmax, 1, col=NA, border="grey40", lwd=1.5)
    tick_at <- pretty(c(vmin, vmax), n=7)
    tick_at <- tick_at[tick_at >= vmin & tick_at <= vmax]
    axis(1, at=tick_at, labels=paste0(tick_at, " t/ha"),
         cex.axis=1.55, font=2, lwd=0, lwd.ticks=1.3)
    mtext("Low yield",  side=1, at=vmin, adj=0, line=3.3, cex=1.2, col="grey45")
    mtext("High yield", side=1, at=vmax, adj=1, line=3.3, cex=1.2, col="grey45")
    mtext(paste0("Mean state yield (t/ha) — ", env_lbl,
                 "   (★ = optimal sowing month)"),
          side=3, line=0.4, cex=1.3, font=2)

    mtext(paste0("Simulated rice yield by sowing month and cultivar — ", env_lbl),
          outer=TRUE, side=3, cex=1.5, font=2, line=1.2)
    dev.off()
    cat("Grid choropleth saved:", basename(out_file), "\n")
}

draw_grid_choro("potential",
                file.path(out_dir, "maps", "pilot_grid_potential.png"))
draw_grid_choro("watbal",
                file.path(out_dir, "maps", "pilot_grid_rainfed.png"))

# ── Excel tables ───────────────────────────────────────────────────────────────
dur_table <- dur_df |>
    select(Cultivar=ccvr,
           `Duration Class`=dur_class,
           `Transplanting to Maturity (d)`=dur_tp,
           `Sowing to Maturity (d)`=dur_sow,
           Label=dur_label)

gap_table <- gap_df |>
    mutate(`Yp (t/ha)`=round(Yp,2), `Yw (t/ha)`=round(Yw,2),
           `Gap (t/ha)`=round(gap,2), `Gap (%)`=gap_pct) |>
    select(State=state, Cultivar=ccvr,
           `Yp (t/ha)`, `Yw (t/ha)`, `Gap (t/ha)`, `Gap (%)`) |>
    arrange(State, Cultivar)

state_best <- cell_best |>
    group_by(State=state, Environment=eenv, Cultivar=ccvr) |>
    summarise(`Opt Yield (kg/ha)` = round(mean(opt_yield),0),
              `Opt Sowing Month`  = paste(unique(opt_month), collapse=", "),
              .groups="drop") |>
    arrange(State, Environment, Cultivar)

cell_out <- cell_best |>
    select(Cell=cell, Lon=x, Lat=y, State=state,
           Environment=eenv, Cultivar=ccvr,
           `Opt Yield (kg/ha)`=opt_yield,
           `Opt Sowing Month`=opt_month,
           `SD (kg/ha)`=opt_sd, `N years`=opt_n) |>
    mutate(across(where(is.numeric), round)) |>
    arrange(State, Environment, Cultivar)

sim_state <- cell_best |>
    group_by(State=state, eenv) |>
    summarise(`Sim Opt (kg/ha)` = round(mean(opt_yield,na.rm=TRUE)), .groups="drop") |>
    pivot_wider(names_from=eenv, values_from=`Sim Opt (kg/ha)`,
                names_prefix="Sim_") |>
    rename(`Sim Yp (kg/ha)`=Sim_potential, `Sim Yw (kg/ha)`=Sim_watbal)

gyga_note <- "GYGA Myanmar: national range 8-12 t/ha; region-specific not reported"
rain_gyga <- "GYGA Myanmar attainable rainfed ~5.5-6.5 t/ha nationally"
lit <- data.frame(
    State = c("Ayeyarwady","Bago (East)","Bago (West)","Mandalay",
              "Nay Pyi Taw","Sagaing","Shan (South)","Yangon"),
    `Lit Yp (kg/ha)` = c(9000,NA,NA,NA,NA,NA,NA,NA),
    `Lit Yp Source`  = c(
        "Laborte et al. 2019: Ayeyarwady ~9 t/ha; GYGA 8-12 t/ha",
        gyga_note,gyga_note,gyga_note,gyga_note,gyga_note,gyga_note,gyga_note),
    `Lit Yw (kg/ha)` = c(4300,NA,NA,2500,NA,NA,NA,NA),
    `Lit Yw Source`  = c(
        "Laborte et al. 2019: monsoon farmer yield 3.2-4.3 t/ha, Ayeyarwady delta",
        rain_gyga,rain_gyga,
        "Laborte et al. 2019: central dry zone ~2-3 t/ha",
        rain_gyga,rain_gyga,rain_gyga,rain_gyga),
    check.names=FALSE
)
lit_compare <- merge(sim_state, lit, by="State", all.x=TRUE) |>
    select(State, `Sim Yp (kg/ha)`, `Lit Yp (kg/ha)`, `Lit Yp Source`,
           `Sim Yw (kg/ha)`, `Lit Yw (kg/ha)`, `Lit Yw Source`) |>
    arrange(State)

hdr_blue <- createStyle(textDecoration="bold",fgFill="#2E75B6",
                        fontColour="white",halign="center")
hdr_grn  <- createStyle(textDecoration="bold",fgFill="#375623",
                        fontColour="white",halign="center")
hdr_ora  <- createStyle(textDecoration="bold",fgFill="#C55A11",
                        fontColour="white",halign="center")
wrap_top <- createStyle(wrapText=TRUE, valign="top")

wb <- createWorkbook()
sheet_list <- list(
    `Cultivar Duration` = list(data=dur_table,   hdr=hdr_grn,  sty="TableStyleMedium9"),
    `Yield Gap`         = list(data=gap_table,    hdr=hdr_ora,  sty="TableStyleMedium7"),
    `State Summary`     = list(data=state_best,   hdr=hdr_blue, sty="TableStyleMedium9"),
    `Cell Level`        = list(data=cell_out,     hdr=hdr_blue, sty="TableStyleLight1"),
    `Lit Comparison`    = list(data=lit_compare,  hdr=hdr_grn,  sty="TableStyleMedium2")
)
for (nm in names(sheet_list)) {
    addWorksheet(wb, nm)
    s  <- sheet_list[[nm]]
    writeDataTable(wb, nm, s$data, tableStyle=s$sty)
    nr <- nrow(s$data);  nc <- ncol(s$data)
    addStyle(wb, nm, s$hdr,    rows=1,        cols=1:nc, gridExpand=TRUE)
    addStyle(wb, nm, wrap_top, rows=2:(nr+1), cols=1:nc, gridExpand=TRUE)
    setColWidths(wb, nm, cols=1:nc, widths="auto")
}
saveWorkbook(wb, file.path(out_dir, "pilot_results_by_state.xlsx"), overwrite=TRUE)
cat("Excel saved\n")
cat("Done.\n")
