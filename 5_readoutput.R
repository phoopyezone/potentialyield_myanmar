

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	path <- "H:/.shortcut-targets-by-id/12UEUrVBpKKmwv8Ub1xsstTrrhZoIn_1A/simulation_data/v2"
} else {
	path <- "C:/1_Research/yield_gap/v2"
}
setwd(path)

dir.create("data/output", FALSE, FALSE)


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
    #run_id <- stringr::str_split(run_line, "\\s+")[[1]]
    #run_id <- as.numeric(run_id[length(run_id)])
	
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
    #df$Run_ID <- run_id
    df$run_ID <- i
	
    # Convert to numeric
    num_cols <- c("TIME", "YEAR", "DOY", "WRR14", "WAGT", "WSO", "WST", "LAI", "TMIN", "TMAX", "RAIN")
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
      WSO = max(WSO, na.rm = TRUE),
      WST = max(WST, na.rm = TRUE),
      LAI = max(LAI, na.rm = TRUE),
      #TMIN = mean(TMIN, na.rm = TRUE),
      #TMAX = mean(TMAX, na.rm = TRUE),
      #RAIN = sum(RAIN, na.rm = TRUE),
      .groups = 'drop'
    )
	out$cell_ID <- gsub("output_|\\.dat", "", basename(file_path))
	out
}


# Extract ORYZA yields from res.dat file created with IPFORM=9
extract_9 <- function(filename) {
  x <- readLines(filename, warn = FALSE)
  x <- x[!grepl("*", x, fixed=TRUE)]
  x <- x[grep(",", x, fixed=TRUE)]
  x <- strsplit(x, ",")
  x <- lapply(x, \(i) if (length(i) == 41) c(i, "") else i)
  x <- do.call(rbind, x)
  nms <- x[1,]
  x <- x[-1,]
  x[x=="-"] <- NA
  x <- matrix(as.numeric(x), nrow=nrow(x), ncol=ncol(x))
  colnames(x) <- nms
  i <- rep(1:2, nrow(x)/2) 
  starts <- x[i==1, c("YEAR", "DOY")]
  colnames(starts) <- c("sYEAR", "sDOY")
  starts <- data.frame(starts, x[i==2, ])
  starts$cell_ID <- gsub("output_|\\.dat", "", basename(filename))
  starts
}

# Extract ORYZA op.dat
extract_op <- function(filename) {

}


ff <- list.files("oryza/output/test", pattern="\\.dat$", full=TRUE)

output <- lapply(ff, function(f) {
		e <- try(extract_5(f))
		if (inherits(e, "try-error")) {
			print(f); flush.console()
			NULL
		} else {
			e
		}
	})
	

#x <- dplyr::bind_rows(output)
x <- do.call(rbind, output)
cells <- readRDS("data/cells.rds")
x$cell <- cells$cell[as.integer(x$cell_ID)]
x$cell_ID <- NULL
x$start_date <- meteor::fromDoy(x$sDOY, x$sYEAR)
x <- data.frame(x)

write.csv(x, "data/output/test.csv", row.names=FALSE)

y <- x[, c("cell", "start_date", "WRR14")]

w <- reshape(y, timevar = "start_date", idvar = "cell", direction = "wide")
r <- terra::rast(terra::rast("data/raw/soil_agg.tif"), nlyr=ncol(w)-1)

r[w$cell] <- w[,-1]
terra::time(r) <- as.Date(gsub("WRR14.", "", names(w)[-1]))

terra::writeRaster(r, "data/output/test.tif")

x <- terra::mean(r, na.rm=TRUE)
terra::plot(x)
	