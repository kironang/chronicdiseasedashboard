library(tidyverse)
library(jsonlite)
library(stringr)

message("📦 Libraries loaded")

# --- Indicator renaming map ---
message("🔄 Defining indicator name mappings...")
indicator_map <- c(
  "All teeth lost among adults aged >=65 years" = "All teeth lost (65+)",
  "Any disability among adults" = "Disability (18+)",
  "Any disability among adults aged >=18 years" = "Disability (18+)",
  "Arthritis among adults" = "Arthritis (18+)",
  "Arthritis among adults aged >=18 years" = "Arthritis (18+)",
  "Binge drinking among adults" = "Binge drinking (18+)",
  "Binge drinking among adults aged >=18 years" = "Binge drinking (18+)",
  "Cancer (excluding skin cancer) among adults aged >=18 years" = "Cancer (non-skin) (18+)",
  "Cancer (non-skin) or melanoma among adults" = "Cancer (non-skin) (18+)"
)

# --- Category/Subcategory lookup ---
message("📚 Creating category lookup table...")
category_lookup <- tribble(
  ~indicator,               ~category,           ~subcategory,
  "All teeth lost (65+)",   "Oral Health",       "Tooth Loss",
  "Disability (18+)",       "Chronic Conditions","Disability",
  "Arthritis (18+)",        "Chronic Conditions","Arthritis",
  "Binge drinking (18+)",   "Behavioral Risk",   "Alcohol Use",
  "Cancer (non-skin) (18+)", "Chronic Conditions","Cancer"
)

# --- DiabetesAtlas CSV processor ---
process_file <- function(file_path) {
  message("📄 Processing DiabetesAtlas file: ", file_path)
  
  lines <- readLines(file_path)
  metadata_line <- lines[1]
  metadata_values <- trimws(unlist(strsplit(metadata_line, ";")))
  indicator <- metadata_values[1]
  unit <- metadata_values[4]
  
  data_start_line <- which(grepl("^Year,", lines))
  if (length(data_start_line) == 0) stop("❌ No data header found in ", file_path)
  
  data_raw <- read.csv(file_path, skip = data_start_line - 1, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Remove any columns with empty column names
  data_raw <- data_raw[, colnames(data_raw) != ""]
  
  # Filter out rows starting with US Diabetes Surveillance System (metadata rows)
  data_raw <- data_raw %>% filter(!grepl("^US Diabetes Surveillance System", data_raw[[1]]))
  
  colnames(data_raw)[1] <- "year"
  data_raw$year <- as.integer(data_raw$year)
  
  # Parse columns into group and measure info
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
  
  # Pivot longer and join with col_info
  data_long <- data_raw %>%
    pivot_longer(cols = -year, names_to = "col_name", values_to = "val_raw") %>%
    left_join(col_info, by = "col_name") %>%
    mutate(value_num = suppressWarnings(as.numeric(val_raw))) %>%
    select(-val_raw) %>%
    group_by(year, group, measure) %>%
    mutate(row_id = row_number()) %>%
    ungroup()
  
  # Pivot wider so measures become columns (value, lower, upper)
  data_wide <- data_long %>%
    select(year, group, measure, value_num, row_id) %>%
    pivot_wider(names_from = measure, values_from = value_num) %>%
    arrange(year, group)
  
  # Collapse rows by year and group - take first non-NA for each measure
  data_collapsed <- data_wide %>%
    group_by(year, group) %>%
    summarise(
      value = first(na.omit(value)),
      lower = first(na.omit(lower)),
      upper = first(na.omit(upper)),
      .groups = "drop"
    )
  
  # Add metadata columns
  data_final <- data_collapsed %>%
    mutate(indicator = indicator, unit = unit) %>%
    select(year, group, value, lower, upper, indicator, unit)
  
  return(data_final)
}

# --- Stroke JSON processor ---
process_json_stroke <- function() {
  message("🌐 Downloading Stroke data JSON...")
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

# --- PLACES JSON processor ---
process_json_places <- function() {
  message("🌐 Downloading PLACES data JSONs...")
  urls <- c(
    "https://data.cdc.gov/resource/swc5-untb.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/h3ej-a9ec.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/duw2-7jbt.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/pqpp-u99h.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/dv4u-3x3q.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence"
  )
  
  places_list <- map(urls, function(url) {
    message("  🔗 Fetching: ", url)
    json_data <- fromJSON(url)
    json_data %>%
      mutate(indicator = recode(measure, !!!indicator_map), group = NA_character_) %>%
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
  
  bind_rows(places_list)
}

# --- County Health Rankings processor ---
process_county_health_rankings <- function() {
  library(tidyverse)
  
  message("⬇️ Downloading County Health Rankings CSV...")
  url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2025_v2.csv"
  chr_data <- read_csv(url, show_col_types = FALSE)
  
  message("🔎 Filtering for McLennan County only...")
  chr_data <- chr_data %>% filter(Name == "McLennan County")
  
  # Step 1: Melt all relevant columns to long format
  long_data <- chr_data %>%
    select(`Release Year`, starts_with("Median"), everything()) %>%
    pivot_longer(
      cols = -`Release Year`,
      names_to = "raw_col",
      values_to = "value"
    ) %>%
    filter(!is.na(value))  # drop empty entries immediately
  
  # Step 2: Regex parsing of column names
  parsed_data <- long_data %>%
    mutate(
      value = as.numeric(value),
      raw_col = str_trim(raw_col),
      indicator = str_match(raw_col, "^(.+?)(?: (?:CI low|CI high|raw value))?(?: \\([^)]+\\))?$")[, 2],
      measure = case_when(
        str_detect(raw_col, "CI low") ~ "lower",
        str_detect(raw_col, "CI high") ~ "upper",
        str_detect(raw_col, "raw value") ~ "value",
        str_detect(raw_col, "\\)$") & !str_detect(raw_col, "CI") ~ "value",
        TRUE ~ NA_character_
      ),
      group = str_match(raw_col, "\\(([^)]+)\\)")[, 2]
    ) %>%
    filter(!is.na(measure) & !is.na(indicator)) %>%
    select(`Release Year`, indicator, group, measure, value)
  
  # Step 3: Reshape to wide format with value, lower, upper in columns
  wide_data <- parsed_data %>%
    pivot_wider(
      names_from = measure,
      values_from = value
    ) %>%
    mutate(
      year = as.integer(`Release Year`),
      unit = "value"
    ) %>%
    select(year, group, indicator, value, lower, upper, unit) %>%
    arrange(year, indicator, group)
  
  return(wide_data)
}



# --- Run everything ---
message("🚀 Starting full data processing...")

files <- list.files(pattern = "^DiabetesAtlas")
diabetes_data <- map_dfr(files, process_file)
message("✅ DiabetesAtlas done: ", nrow(diabetes_data), " rows")

stroke_data <- process_json_stroke()
message("✅ Stroke done: ", nrow(stroke_data), " rows")

places_data <- process_json_places()
message("✅ PLACES done: ", nrow(places_data), " rows")

chr_data <- process_county_health_rankings()
message("✅ County Health Rankings done: ", nrow(chr_data), " rows")

# Combine all datasets
all_data <- bind_rows(diabetes_data, stroke_data, places_data, chr_data) %>%
  # Rename indicators according to map
  mutate(
    indicator = recode(indicator, !!!indicator_map),
    category = category_lookup$category[match(indicator, category_lookup$indicator)],
    subcategory = category_lookup$subcategory[match(indicator, category_lookup$indicator)],
    group = ifelse(is.na(group), "Total", group)
  ) %>%
  arrange(year, indicator, group)

message("🎉 All data combined: ", nrow(all_data), " rows")

write_csv(all_data, "data.csv")
