library(tidyverse)
library(jsonlite)
library(stringr)

# --- Indicator renaming map (named vector) ---
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

# --- Category/Subcategory lookup (final indicator name → category, subcategory) ---
category_lookup <- tribble(
  ~indicator,              ~category,           ~subcategory,
  "All teeth lost (65+)",  "Oral Health",       "Tooth Loss",
  "Disability (18+)",      "Chronic Conditions","Disability",
  "Arthritis (18+)",       "Chronic Conditions","Arthritis",
  "Binge drinking (18+)",  "Behavioral Risk",   "Alcohol Use",
  "Cancer (non-skin) (18+)","Chronic Conditions","Cancer"
)

# --- Process DiabetesAtlas CSV ---
process_file <- function(file_path) {
  message("📄 Processing: ", file_path)
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

# --- Process Stroke JSON ---
process_json_stroke <- function() {
  url <- "https://data.cdc.gov/resource/7b9s-s8ck.json?locationdesc=McLennan&$limit=2000"
  json_data <- fromJSON(url)
  
  json_data %>%
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
}

# --- Process BRFSS JSON ---
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
    
    json_data %>%
      mutate(
        indicator = recode(measure, !!!indicator_map),
        group = NA_character_
      ) %>%
      transmute(
        year = as.integer(year),
        group,
        value = as.numeric(data_value),
        lower = as.numeric(low_confidence_limit),
        upper = as.numeric(high_confidence_limit),
        indicator,
        unit = data_value_unit
      ) %>%
      distinct()
  })
  
  bind_rows(brfss_list)
}

# --- Main Script ---

files <- list.files(pattern = "^DiabetesAtlas")
diabetes_data <- map_dfr(files, process_file)
stroke_data <- process_json_stroke()
brfss_data <- process_json_brfss()

combined_data <- bind_rows(diabetes_data, stroke_data, brfss_data) %>%
  mutate(
    # Standardize indicator names for all data
    indicator = recode(indicator, !!!indicator_map)
  ) %>%
  left_join(category_lookup, by = "indicator") %>%
  mutate(
    category = coalesce(category, "Uncategorized"),
    subcategory = coalesce(subcategory, "Uncategorized")
  ) %>%
  filter(!is.na(year), !is.na(value)) %>%
  arrange(indicator, year)

write_csv(combined_data, "data.csv")
message("✅ Data processed and saved to data.csv")
