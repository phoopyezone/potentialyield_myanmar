library(dplyr)
library(readr)
library(stringr)

# Extract ORYZA yields from res.dat file
extract_yields <- function(file_path = "C:/1_Research/simulation_main/res.dat",
                           output_path = "C:/1_Research/simulation_main/rice_yields_2015_2024.csv") {
  
  lines <- readLines(file_path, warn = FALSE)
  rerun_lines <- grep("OUTPUT FROM RERUN SET:", lines)
  all_data <- list()
  
  for(i in seq_along(rerun_lines)) {
    start_idx <- rerun_lines[i]
    end_idx <- if(i < length(rerun_lines)) rerun_lines[i+1] - 1 else length(lines)
    section <- lines[start_idx:end_idx]
    
    # Get run ID
    run_line <- section[grep("OUTPUT FROM RERUN SET:", section)]
    run_id <- str_split(run_line, "\\s+")[[1]]
    run_id <- as.numeric(run_id[length(run_id)])
    
    # Find data
    header_idx <- grep("^TIME\\s+", section)
    if(length(header_idx) == 0) next
    
    headers <- str_split(str_squish(section[header_idx[1]]), "\\s+")[[1]]
    data_lines <- section[(header_idx[1] + 1):length(section)]
    data_lines <- data_lines[str_detect(data_lines, "^\\s*\\d")]
    
    if(length(data_lines) == 0) next
    
    # Parse data
    parsed_data <- list()
    for(j in seq_along(data_lines)) {
      vals <- str_split(str_squish(data_lines[j]), "\\s+")[[1]]
      if(length(vals) >= length(headers)) {
        parsed_data[[j]] <- setNames(vals[1:length(headers)], headers)
      }
    }
    
    if(length(parsed_data) == 0) next
    
    df <- data.frame(do.call(rbind, parsed_data), stringsAsFactors = FALSE)
    df$Run_ID <- run_id
    
    # Convert to numeric
    numeric_cols <- c("TIME", "YEAR", "DOY", "WRR14", "WAGT", "WSO", "WST", 
                      "LAI","TMIN", "TMAX", "RAIN")
    for(col in numeric_cols) {
      if(col %in% names(df)) {
        df[[col]] <- as.numeric(df[[col]])
      }
    }
    
    all_data[[i]] <- df
  }
  
  # Combine and get final yields
  combined <- bind_rows(all_data)
  yields <- combined %>%
    filter(!is.na(WRR14), WRR14 > 0, !is.na(YEAR), !is.na(WSO)) %>%
    group_by(Run_ID, YEAR) %>%
    summarise(
      Yield_kg_ha = max(WRR14, na.rm = TRUE),
      Total_Biomass_kg_ha = max(WAGT, na.rm = TRUE),
      Storage_Organ_kg_ha = max(WSO, na.rm = TRUE),
      Stem_kg_ha = max(WST, na.rm = TRUE),
      Max_LAI = max(LAI, na.rm = TRUE),
      Mean_Tmin = mean(TMIN, na.rm = TRUE),
      Mean_Tmax = mean(TMAX, na.rm = TRUE),
      Total_Rain = sum(RAIN, na.rm = TRUE),
      Harvest_DOY = DOY[which.max(WRR14)],
      .groups = 'drop'
    )
  
  write_csv(yields, output_path)
  return(yields)
}

# Run extraction
yields <- extract_yields()

