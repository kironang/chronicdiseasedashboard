library(tidyverse)
library(jsonlite)
library(stringr)

message("📦 Libraries loaded")

chr_urls <- c(
  "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2025_v2.csv",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2024.csv",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2023_0.csv",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2022.csv",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2021.csv",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020_0.csv",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2019.csv",
  "https://www.countyhealthrankings.org/sites/default/files/analytic_data2018_0.csv",
  "https://www.countyhealthrankings.org/sites/default/files/analytic_data2017.csv",
  "https://www.countyhealthrankings.org/sites/default/files/analytic_data2016.csv",
  "https://www.countyhealthrankings.org/sites/default/files/analytic_data2015.csv",
  "https://www.countyhealthrankings.org/sites/default/files/analytic_data2014.csv",
  "https://www.countyhealthrankings.org/sites/default/files/analytic_data2013.csv",
  "https://www.countyhealthrankings.org/sites/default/files/analytic_data2012.csv",
  "https://www.countyhealthrankings.org/sites/default/files/analytic_data2011.csv",
  "https://www.countyhealthrankings.org/sites/default/files/analytic_data2010.csv"
)

clean_indicator <- function(x) {
  x %>%
    str_trim() %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("%\\s*", "% ") %>%
    str_to_lower() %>%
    str_replace_all("(^|[\\s-])([a-z])", ~ toupper(.x))
}

process_chr_file <- function(url) {
  message("⬇️ Downloading CHR CSV: ", url)
  
  chr_data <- tryCatch(read_csv(url, show_col_types = FALSE), error = function(e) {
    message("❌ Failed to read: ", url)
    return(NULL)
  })
  
  if (is.null(chr_data)) return(NULL)
  
  chr_data <- chr_data %>% select(!matches("="))
  chr_data <- chr_data %>% filter(Name == "McLennan County")
  message("✅ McLennan County rows: ", nrow(chr_data))
  
  long_data <- chr_data %>%
    select(`Release Year`, starts_with("Median"), everything()) %>%
    pivot_longer(
      cols = -`Release Year`,
      names_to = "raw_col",
      values_to = "value"
    ) %>%
    filter(!is.na(value))
  
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
  
  wide_data <- parsed_data %>%
    pivot_wider(names_from = measure, values_from = value) %>%
    mutate(
      year = as.integer(`Release Year`),
      unit = "value"
    ) %>%
    select(year, group, indicator, value, lower, upper, unit) %>%
    arrange(year, indicator, group)
  
  return(wide_data)
}

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
    left_join(col_info, by = "col_name") %>%
    mutate(value_num = suppressWarnings(as.numeric(val_raw))) %>%
    select(-val_raw) %>%
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
      group = ifelse(str_detect(group, "^\\d"), paste0("Ages ", group), group),
      indicator = indicator,
      unit = unit
    ) %>%
    select(year, group, value, lower, upper, indicator, unit)
  
  return(data_final)
}

process_json_stroke <- function() {
  message("🌐 Downloading Stroke data JSON...")
  url <- "https://data.cdc.gov/resource/7b9s-s8ck.json?locationdesc=McLennan&$limit=2000&$order=year"
  json_data <- fromJSON(url)
  
  json_data %>%
    transmute(
      year = as.integer(year),
      group = paste(stratification1, stratification2, stratification3, sep = " - "),
      value = as.numeric(data_value),
      lower = as.numeric(confidence_limit_low),
      upper = as.numeric(confidence_limit_high),
      indicator = topic,
      unit = data_value_unit
    ) %>%
    distinct()
}

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
      mutate(
        indicator = str_replace(measure, " aged.*", ""),
        indicator = str_trim(indicator),
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
  
  bind_rows(places_list)
}

message("🚀 Starting full data processing...")

files <- list.files(pattern = "^DiabetesAtlas")
diabetes_data <- map_dfr(files, process_file)
stroke_data <- process_json_stroke()
places_data <- process_json_places()
chr_data <- map_dfr(chr_urls, process_chr_file)

all_data <- bind_rows(diabetes_data, stroke_data, places_data, chr_data) %>%
  mutate(indicator = clean_indicator(indicator)) %>%
  filter(!is.na(year) & !is.na(value)) %>%
  mutate(group = if_else(is.na(group), "Total", group))

if (!file.exists("indicators.csv")) {
  indicator_template <- all_data %>%
    distinct(indicator) %>%
    arrange(indicator) %>%
    mutate(
      new_indicator = "",
      category = "",
      subcategory = "",
      description = ""
    )
  write_csv(indicator_template, "indicators.csv")
  message("📄 Classification template written to: indicators.csv")
  stop("🛑 Edit 'indicators.csv' before continuing. Then re-run this script.")
}

indicator_meta <- read_csv("indicators.csv", show_col_types = FALSE)

all_data_final <- all_data %>%
  left_join(indicator_meta, by = "indicator") %>%
  mutate(
    indicator = case_when(
      !is.na(new_indicator) & new_indicator != "" ~ new_indicator,
      TRUE ~ indicator
    ),
    category = if_else(is.na(category) | category == "", "Uncategorized", category),
    subcategory = if_else(is.na(subcategory) | subcategory == "", "Uncategorized", subcategory),
    description = if_else(is.na(description) | description == "", "No description yet", description)
  ) %>%
  select(year, group, indicator, value, lower, upper, unit, category, subcategory, description) %>%
  arrange(year)

write_csv(all_data_final, "data.csv")
message("🎉 Done! Final dataset saved as data.csv with ", nrow(all_data_final), " rows.")
