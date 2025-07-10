library(tidyverse)
library(jsonlite)
library(stringr)

# --- Process CSV files starting with 'DiabetesAtlas' ---
process_file <- function(file_path) {
  message("Processing file: ", file_path)
  lines <- readLines(file_path)
  
  metadata_line <- lines[1]
  metadata_values <- trimws(unlist(strsplit(metadata_line, ";")))
  indicator <- metadata_values[1]
  unit <- metadata_values[4]
  
  data_start_line <- which(grepl("^Year,", lines))
  if(length(data_start_line) == 0) stop("No data header found in ", file_path)
  
  data_raw <- read.csv(file_path, skip = data_start_line - 1, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  data_raw <- data_raw[, colnames(data_raw) != ""]
  data_raw <- data_raw %>% filter(!grepl("^US Diabetes Surveillance System", data_raw[[1]]))
  colnames(data_raw)[1] <- "year"
  data_raw$year <- as.integer(data_raw$year)
  
  data_cols <- colnames(data_raw)[-1]
  parts <- str_match(data_cols, "^(.*) - (.*)$")
  col_info <- data.frame(
    col_name = data_cols,
    group = str_trim(parts[, 2]),
    measure_raw = tolower(str_trim(parts[, 3])),
    stringsAsFactors = FALSE
  )
  
  measure_map <- c(
    "percentage" = "value",
    "rate per 1000" = "value",
    "rate" = "value",
    "lower limit" = "lower",
    "upper limit" = "upper"
  )
  col_info$measure <- measure_map[col_info$measure_raw]
  col_info$measure[is.na(col_info$measure)] <- col_info$measure_raw[is.na(col_info$measure)]
  
  data_long <- data_raw %>%
    pivot_longer(cols = -year, names_to = "col_name", values_to = "val_raw") %>%
    left_join(col_info, by = "col_name")
  
  data_long <- data_long %>%
    mutate(value_num = suppressWarnings(as.numeric(val_raw))) %>%
    select(-val_raw)
  
  data_long <- data_long %>%
    group_by(year, group, measure) %>%
    mutate(row_id = row_number()) %>%
    ungroup()
  
  data_wide <- data_long %>%
    select(year, group, measure, value_num, row_id) %>%
    pivot_wider(names_from = measure, values_from = value_num) %>%
    arrange(year, group)
  
  data_collapsed <- data_wide %>%
    group_by(year, group) %>%
    summarise(
      value = first(na.omit(value)),
      lower = first(na.omit(lower)),
      upper = first(na.omit(upper)),
      .groups = "drop"
    )
  
  data_final <- data_collapsed %>%
    mutate(
      indicator = indicator,
      unit = unit
    ) %>%
    select(year, group, value, lower, upper, indicator, unit)
  
  return(data_final)
}

# --- Process JSON from Cardiovascular Stroke source ---
process_json_stroke <- function() {
  url <- "https://data.cdc.gov/resource/7b9s-s8ck.json?locationdesc=McLennan&$limit=2000"
  json_data <- fromJSON(url)
  
  stroke_data <- json_data %>%
    transmute(
      year = as.integer(year),
      group = paste(stratification1, stratification3, sep = " - "),
      value = as.numeric(data_value),
      lower = as.numeric(confidence_limit_low),
      upper = as.numeric(confidence_limit_high),
      indicator = topic,
      unit = data_value_unit
    ) %>%
    distinct()
  
  return(stroke_data)
}

# --- Indicator name standardization map for BRFSS ---
indicator_map <- c(
  # Teeth loss
  "All teeth lost among adults aged >=65 years" = "All teeth lost (65+)",
  
  # Disability
  "Any disability among adults" = "Disability (18+)",
  "Any disability among adults aged >=18 years" = "Disability (18+)",
  
  # Arthritis
  "Arthritis among adults" = "Arthritis (18+)",
  "Arthritis among adults aged >=18 years" = "Arthritis (18+)",
  
  # Binge drinking
  "Binge drinking among adults" = "Binge drinking (18+)",
  "Binge drinking among adults aged >=18 years" = "Binge drinking (18+)",
  
  # Cancer
  "Cancer (excluding skin cancer) among adults aged >=18 years" = "Cancer (non-skin) (18+)",
  "Cancer (non-skin) or melanoma among adults" = "Cancer (non-skin) (18+)"
)

# --- Process JSON from BRFSS (multiple sources) ---
process_json_brfss <- function() {
  urls <- c(
    "https://data.cdc.gov/resource/swc5-untb.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/h3ej-a9ec.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/duw2-7jbt.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/pqpp-u99h.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/dv4u-3x3q.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence"
  )
  
  brfss_list <- map(urls, function(url) {
    json_data <- fromJSON(url)
    
    unmatched <- setdiff(unique(json_data$measure), names(indicator_map))
    if(length(unmatched) > 0) {
      message("⚠️ Unmatched indicators found in URL: ", url)
      message(paste(unmatched, collapse = "\n"))
    }
    
    json_data %>%
      transmute(
        year = as.integer(year),
        group = NA_character_,
        value = as.numeric(data_value),
        lower = as.numeric(low_confidence_limit),
        upper = as.numeric(high_confidence_limit),
        indicator = recode(measure, !!!indicator_map),
        unit = data_value_unit
      ) %>%
      distinct()
  })
  
  brfss_data <- bind_rows(brfss_list)
  return(brfss_data)
}

# --- Run all processors ---
files <- list.files(pattern = "^DiabetesAtlas")
diabetes_data <- map_dfr(files, process_file)
stroke_data <- process_json_stroke()
brfss_data  <- process_json_brfss()

# --- Combine all datasets ---
combined_data <- bind_rows(diabetes_data, stroke_data, brfss_data)

# --- Filter out bad rows ---
combined_data <- combined_data %>% 
  filter(!is.na(year), !is.na(value)) %>%
  arrange(indicator) # Sort by year for final deployment. Sort by indicator to detect problems

# --- Write final CSV ---
write_csv(combined_data, "data.csv")
message("✅ All data processed and written to data.csv")
