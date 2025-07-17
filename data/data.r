setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(jsonlite)
library(stringr)

clean_indicator <- function(x) {
  x %>%
    str_trim() %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("%\\s*", "% ") %>%
    str_to_lower() %>%
    str_replace_all("(^|[\\s-])([a-z])", ~ toupper(.x))
}

safe_numeric <- function(x, quiet = FALSE) {
  x[x %in% c("TX", "McLennan County", "NA")] <- NA
  suppressWarnings({
    x_num <- as.numeric(x)
  })
  if (!quiet) {
    bad_vals <- unique(x[is.na(x_num) & !is.na(x)])
    if (length(bad_vals) > 0) {
      warning("Some values could not be converted to numeric: ", paste(bad_vals, collapse = ", "))
    }
  }
  x_num
}

safe_integer <- function(x, quiet = FALSE) {
  bad_strings <- c("1999 - 2010", "2010 - 2019")
  x[x %in% bad_strings] <- NA
  suppressWarnings({
    x_int <- as.integer(x)
  })
  if (!quiet) {
    bad_vals <- unique(x[is.na(x_int) & !is.na(x)])
    if (length(bad_vals) > 0) {
      warning("Some values could not be converted to integer: ", paste(bad_vals, collapse = ", "))
    }
  }
  x_int
}

countyhealthrankings_links <- c(
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

countyhealthrankings <- function(url) {
  chr_data <- tryCatch(read_csv(url, show_col_types = FALSE), error = function(e) return(NULL))
  if (is.null(chr_data)) return(NULL)
  chr_data <- chr_data %>% select(!matches("=")) %>% filter(Name == "McLennan County")
  long_data <- chr_data %>%
    pivot_longer(cols = -`Release Year`, names_to = "raw_col", values_to = "value") %>%
    filter(!is.na(value))
  parsed_data <- long_data %>%
    mutate(
      value = safe_numeric(value),
      raw_col = str_trim(raw_col),
      indicator = str_match(raw_col, "^(.+?)(?: (?:CI low|CI high|raw value))?(?: \\([^)]+\\))?$")[, 2],
      measure = case_when(
        str_detect(raw_col, "CI low") ~ "lower",
        str_detect(raw_col, "CI high") ~ "upper",
        str_detect(raw_col, "raw value") ~ "value",
        str_detect(raw_col, "\\)$") & !str_detect(raw_col, "CI") ~ "value",
        TRUE ~ NA_character_
      ),
      race = str_match(raw_col, "\\(([^)]+)\\)")[, 2]
    ) %>%
    filter(!is.na(measure) & !is.na(indicator)) %>%
    select(`Release Year`, indicator, race, measure, value)
  
  wide_data <- parsed_data %>%
    pivot_wider(names_from = measure, values_from = value) %>%
    mutate(
      year = safe_integer(`Release Year`),
      age = NA_character_,
      sex = NA_character_,
      unit = if_else(str_detect(indicator, "%"), "Percentage", "Raw Count"),
      source = "CHR"
    ) %>%
    select(year, age, sex, race, indicator, value, lower, upper, unit, source) %>%
    arrange(year, indicator, race)
  
  return(wide_data)
}

diabetesatlas <- function(file_path) {
  lines <- readLines(file_path)
  metadata_line <- lines[1]
  metadata_values <- trimws(unlist(strsplit(metadata_line, ";")))
  indicator <- metadata_values[1]
  unit <- paste(metadata_values[4], metadata_values[3], sep = ", ")
  data_start_line <- which(grepl("^Year,", lines))
  data_raw <- read.csv(file_path, skip = data_start_line - 1, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  data_raw <- data_raw[1:nrow(data_raw) - 1, colnames(data_raw) != ""]
  
  colnames(data_raw)[1] <- "year"
  data_raw$year <- safe_integer(data_raw$year)
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
    mutate(value_num = safe_numeric(val_raw)) %>%
    select(-val_raw)
  
  data_wide <- data_long %>%
    select(year, group, measure, value_num) %>%
    pivot_wider(names_from = measure, values_from = value_num)
  
  data_final <- data_wide %>%
    mutate(
      age = ifelse(str_detect(group, "^(\\d{1,2}-\\d{1,2}|65\\+)"), group, NA_character_),
      sex = ifelse(str_detect(group, "\\b(Male|Female)\\b"), group, NA_character_),
      race = ifelse(group %in% c("Black", "White", "Hispanic", "Asian", "American Indian/Alaska Native"), group, NA_character_),
      age = ifelse(str_detect(group, "^Ages "), str_remove(group, "^Ages "), age),
      sex = ifelse(sex == group, sex, NA_character_),
      race = ifelse(race == group, race, NA_character_),
      age = ifelse(group == "Total", NA_character_, age),
      sex = ifelse(group == "Total", NA_character_, sex),
      race = ifelse(group == "Total", NA_character_, race),
      indicator = indicator,
      unit = unit,
      source = "DA"
    ) %>%
    select(year, age, sex, race, indicator, value, lower, upper, unit, source)
  
  return(data_final)
}

heartdiseasestrokemortality <- function() {
  url <- "https://data.cdc.gov/resource/7b9s-s8ck.json?locationdesc=McLennan&$limit=2000&$order=year"
  json_data <- fromJSON(url)
  json_data %>%
    transmute(
      year = safe_integer(year),
      age = stratification1,
      race = stratification2,
      sex = stratification3,
      value = safe_numeric(data_value),
      lower = safe_numeric(confidence_limit_low),
      upper = safe_numeric(confidence_limit_high),
      indicator = topic,
      unit = paste(data_value_type, data_value_unit, sep = ", "),
      source = "HDSM"
    ) %>%
    distinct()
}

places <- function() {
  urls <- c(
    "https://data.cdc.gov/resource/swc5-untb.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/h3ej-a9ec.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/duw2-7jbt.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/pqpp-u99h.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence",
    "https://data.cdc.gov/resource/dv4u-3x3q.json?locationname=McLennan&$limit=2000&data_value_type=Age-adjusted%20prevalence"
  )
  
  extract_demographics <- function(text) {
    age <- str_extract(text, "aged\\s+(\\d{1,2}[-–]\\d{1,2}|\\d{2,3}|>=\\d{1,2})\\s*years?")
    age <- str_replace_all(age, "aged\\s*|years?", "")
    age <- str_trim(age)
    
    # If no specific age range found, but "adults" is present, use "18+"
    if (is.na(age) || age == "") {
      if (str_detect(text, regex("adults", ignore_case = TRUE))) {
        age <- "18+"
      } else {
        age <- NA_character_
      }
    }
    
    sex <- case_when(
      str_detect(text, regex("women|females", ignore_case = TRUE)) ~ "Female",
      str_detect(text, regex("men|males", ignore_case = TRUE)) ~ "Male",
      TRUE ~ NA_character_
    )
    
    race <- case_when(
      str_detect(text, regex("Black|African American", ignore_case = TRUE)) ~ "Black or African American",
      str_detect(text, regex("White", ignore_case = TRUE)) ~ "White",
      str_detect(text, regex("Hispanic|Latino", ignore_case = TRUE)) ~ "Hispanic or Latino",
      str_detect(text, regex("Asian", ignore_case = TRUE)) ~ "Asian",
      str_detect(text, regex("American Indian|Alaska Native", ignore_case = TRUE)) ~ "American Indian or Alaska Native",
      str_detect(text, regex("Pacific Islander", ignore_case = TRUE)) ~ "Pacific Islander",
      TRUE ~ NA_character_
    )
    
    list(age = age, sex = sex, race = race)
  }
  
  
  places_list <- map(urls, function(url) {
    json_data <- fromJSON(url)
    
    demographics <- map(json_data$measure, extract_demographics) %>% transpose()
    
    json_data %>%
      mutate(
        indicator = str_replace(measure, " among.*", ""),
        age = demographics$age %>% unlist(),
        sex = demographics$sex %>% unlist(),
        race = demographics$race %>% unlist()
      ) %>%
      transmute(
        year = safe_integer(year),
        age,
        sex,
        race,
        value = safe_numeric(data_value),
        lower = safe_numeric(low_confidence_limit),
        upper = safe_numeric(high_confidence_limit),
        indicator = str_trim(indicator),
        unit = paste(data_value_type, data_value_unit, sep = ", "),
        source = "PLACES"
      ) %>%
      distinct()
  })
  
  bind_rows(places_list)
}
ensure_final_newline <- function(file_path) {
  con <- file(file_path, open = "rb")
  raw_data <- readBin(con, what = "raw", n = file.info(file_path)$size)
  close(con)
  if (raw_data[length(raw_data)] != as.raw(0x0A)) {
    cat("\n", file = file_path, append = TRUE)
  }
}

files <- list.files(pattern = "^DiabetesAtlas")
walk(files, ensure_final_newline)

diabetesatlas_data <- map_dfr(files, diabetesatlas)
hdsm_data <- heartdiseasestrokemortality()
places_data <- places()
chr_data <- map_dfr(countyhealthrankings_links, countyhealthrankings)

all_data <- bind_rows(diabetesatlas_data, hdsm_data, places_data, chr_data) %>%
  mutate(
    indicator = clean_indicator(indicator),
    age = if_else(is.na(age), "Overall", age),
    sex = if_else(is.na(sex), "Overall", sex),
    race = if_else(is.na(race), "Overall", race)
  ) %>%
  filter(!is.na(year) & !is.na(value))

if (!file.exists("indicators.csv")) {
  message("UNABLE TO LOCATE 'indicators.csv' IN THE CURRENT WORKING DIRECTORY, CREATING IT NOW...")
  indicator_template <- all_data %>%
    distinct(indicator, unit, source) %>%
    arrange(indicator) %>%
    rowwise() %>%
    mutate(
      new_indicator = "",
      new_unit = "",
      exclude = "no",
      category = "",
      subcategory = "",
      description = ""
    ) %>%
    ungroup()
  write_csv(indicator_template, "indicators.csv")
  stop("FILE CREATED SUCCESSFULLY. PLEASE COMPLETE 'indicators.csv'")
}

indicator_meta <- read_csv("indicators.csv", show_col_types = FALSE)
included_indicators <- indicator_meta %>%
  filter(is.na(exclude) | tolower(exclude) != "yes")

unmatched <- anti_join(all_data, included_indicators, by = c("indicator", "unit", "source"))
if (nrow(unmatched) > 0) {
  print(unmatched %>% distinct(indicator, unit, source), n = 100)
}

filtered_data <- all_data %>%
  inner_join(included_indicators, by = c("indicator", "unit", "source")) %>%
  mutate(
    indicator = if_else(!is.na(new_indicator), new_indicator, indicator),
    unit = if_else(!is.na(new_unit), new_unit, unit),
    category = if_else(!is.na(category), category, "No Category"),
    subcategory = if_else(!is.na(subcategory), subcategory, "No Subcategory"),
    description = if_else(!is.na(description), description, "No Description"),
  ) %>%
  select(year, age, sex, race, indicator, value, lower, upper, unit, source, category, subcategory, description)

write_csv(filtered_data, "data.csv")
