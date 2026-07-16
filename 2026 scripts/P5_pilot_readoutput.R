##
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else if (this == "phoopyezone") {
	path <- "G:/My Drive/Research/simulation/main_oryza/v2"
} else {
	path <- "."
}
setwd(path)


# Extract ORYZA yields from res.dat file created with IPFORM=5
extract_5 <- function(file_path) {

	#print(file_path); flush.console()

  lines <- readLines(file_path, warn = FALSE)
  start_idx <- grep("OUTPUT FROM RERUN SET:", lines)
  end_idx <- c(start_idx[-1], length(lines))

  all_data <- vector("list", length(start_idx))

  for (i in seq_along(start_idx)) {
    section <- lines[start_idx[i]:end_idx[i]]

    # Get run ID
    run_line <- section[grep("OUTPUT FROM RERUN SET:", section)]

    # Find data
    header_idx <- grep("^TIME\\s+", section)

	## this could break things.
    if (length(header_idx) == 0) next

    headers <- stringr::str_split(stringr::str_squish(section[header_idx[1]]), "\\s+")[[1]]
    data_lines <- section[(header_idx[1] + 1):length(section)]
    data_lines <- data_lines[stringr::str_detect(data_lines, "^\\s*\\d")]

	## this would break things.
    #if (length(data_lines) == 0) next

    # Parse data
    parsed_data <- vector("list", length(data_lines))
    for (j in seq_along(data_lines)) {
      vals <- stringr::str_split(stringr::str_squish(data_lines[j]), "\\s+")[[1]]
      if (length(vals) >= length(headers)) {
        parsed_data[[j]] <- setNames(vals[1:length(headers)], headers)
      }
    }

	## this would break things.
    #if (length(parsed_data) == 0) next

    df <- data.frame(do.call(rbind, parsed_data), stringsAsFactors = FALSE)
    df[df == "-"] <- NA
    df$run_ID <- i

    # Convert to numeric
    num_cols <- c("TIME", "YEAR", "DOY", "WRR14", "WAGT", "WSO", "WST", "LAI", "TMIN", "TMAX", "RAIN")
	num_cols <- num_cols[num_cols %in% names(df)]
	df[num_cols] <- sapply(df[num_cols], as.numeric)

    all_data[[i]] <- df
  }

  combined <- dplyr::bind_rows(all_data)
  out <- combined |>
    dplyr::group_by(run_ID) |>
    dplyr::summarise(
	  sYEAR = YEAR[1],
	  sDOY = DOY[1],
	  eYEAR = YEAR[which.max(WRR14)],
      eDOY = DOY[which.max(WRR14)],
      WRR14 = max(WRR14, na.rm = TRUE),
      WAGT = max(WAGT, na.rm = TRUE),
      .groups = 'drop'
    )
	out$cell_ID <- gsub("output_|\\.dat", "", basename(file_path))
	out
}


#### change
runs <- c("pilot_potential_standard", "pilot_potential_mestizo",
          "pilot_potential_local",    "pilot_potential_RC222",
          "pilot_watbal_standard",    "pilot_watbal_mestizo",
          "pilot_watbal_local",       "pilot_watbal_RC222")

#### do not change
cells <- readRDS("data/pilot_selected_cells.rds")

all_output <- lapply(runs, function(run) {
	ff <- list.files(file.path("simulate/output", run, "dat"), pattern="\\.dat$", full=TRUE)
	output <- lapply(ff, function(f) {
		e <- try(extract_5(f))
		if (inherits(e, "try-error")) { print(f); flush.console(); NULL } else { e }
	})
	x <- do.call(rbind, output)
	x$cell    <- cells$cell[as.integer(x$cell_ID)]
	x$cell_ID <- NULL
	x$start_date <- meteor::fromDoy(x$sDOY, x$sYEAR)
	parts     <- strsplit(run, "_")[[1]]   # "pilot", "potential/watbal", "standard/mestizo"
	x$eenv    <- parts[2]
	x$ccvr    <- parts[3]
	x$run     <- run
	x
})

x <- do.call(rbind, all_output)
x <- data.frame(x)

write.csv(x, "data/output/pilot_results.csv", row.names=FALSE)
cat("Saved: data/output/pilot_results.csv\n")
cat("Rows:", nrow(x), "| Cells:", length(unique(x$cell)),
    "| Runs:", length(unique(x$run)), "\n")
