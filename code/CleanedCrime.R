library(tidyverse)

# Step 1: Define Yorkshire districts
south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
west_yorkshire_districts <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")
yorkshire_districts <- c(south_yorkshire_districts, west_yorkshire_districts)

# Step 2: Load and summarize population data
population <- read_csv("Cleaned Data/cleanPopulation2011.csv")
cat("Rows in cleanPopulation2011.csv:", nrow(population), "\n")
cat("Unique districts in population:", unique(population$District), "\n")
cat("Sample LSOA in population:", head(population$LSOA, 5), "\n")

population <- population %>%
  group_by(District, LSOA) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")
cat("Rows in summarized population:", nrow(population), "\n")

# Step 3: Crime cleaning function
clean_crime_data <- function(crime_data, region_name) {
  region_districts <- if (region_name == "South Yorkshire") {
    south_yorkshire_districts
  } else {
    west_yorkshire_districts
  }
  
  # Initial row count
  cat("Initial rows in crime_data (", region_name, "):", nrow(crime_data), "\n")
  
  # Filter valid LSOA and Crime type
  crime_filtered <- crime_data %>%
    filter(!is.na(`LSOA code`), !is.na(`LSOA name`), !is.na(`Crime type`))
  cat("Rows after filtering non-NA LSOA and Crime type:", nrow(crime_filtered), "\n")
  
  # Filter for region districts in LSOA name
  crime_filtered <- crime_filtered %>%
    filter(str_detect(toupper(`LSOA name`), paste(toupper(region_districts), collapse = "|")))
  cat("Rows after filtering for", region_name, "districts:", nrow(crime_filtered), "\n")
  
  # Assign District and County
  crime_processed <- crime_filtered %>%
    mutate(
      District = case_when(
        str_detect(toupper(`LSOA name`), "SHEFFIELD") ~ "SHEFFIELD",
        str_detect(toupper(`LSOA name`), "BARNSLEY") ~ "BARNSLEY",
        str_detect(toupper(`LSOA name`), "DONCASTER") ~ "DONCASTER",
        str_detect(toupper(`LSOA name`), "ROTHERHAM") ~ "ROTHERHAM",
        str_detect(toupper(`LSOA name`), "LEEDS") ~ "LEEDS",
        str_detect(toupper(`LSOA name`), "BRADFORD") ~ "BRADFORD",
        str_detect(toupper(`LSOA name`), "CALDERDALE") ~ "CALDERDALE",
        str_detect(toupper(`LSOA name`), "KIRKLEES") ~ "KIRKLEES",
        str_detect(toupper(`LSOA name`), "WAKEFIELD") ~ "WAKEFIELD",
        TRUE ~ NA_character_
      ),
      County = case_when(
        District %in% south_yorkshire_districts ~ "SOUTH YORKSHIRE",
        District %in% west_yorkshire_districts ~ "WEST YORKSHIRE",
        TRUE ~ NA_character_
      ),
      LSOA = `LSOA code`
    ) %>%
    filter(District %in% region_districts)
  cat("Rows after assigning District/County and filtering:", nrow(crime_processed), "\n")
  
  # Join with population
  crime_joined <- crime_processed %>%
    left_join(population, by = c("District", "LSOA"), suffix = c("", ".pop"))
  cat("Rows after joining with population:", nrow(crime_joined), "\n")
  cat("Rows with non-NA Population:", sum(!is.na(crime_joined$Population)), "\n")
  
  # Select and filter final columns
  crime_final <- crime_joined %>%
    select(Month, `Crime type`, LSOA, District, County, Population) %>%
    distinct() %>%
    filter(!is.na(Month), !is.na(`Crime type`), !is.na(LSOA), !is.na(District), !is.na(County), !is.na(Population))
  cat("Rows after final filtering:", nrow(crime_final), "\n")
  
  crime_final %>%
    mutate(Region = region_name)
}

# Step 4: Recursively list all monthly crime files
all_files <- list.files("Obtained Data/crime/", pattern = "-yorkshire-street\\.csv$", recursive = TRUE, full.names = TRUE)
south_files <- all_files[str_detect(all_files, "south-yorkshire")]
west_files <- all_files[str_detect(all_files, "west-yorkshire")]
cat("Number of South Yorkshire files:", length(south_files), "\n")
cat("Number of West Yorkshire files:", length(west_files), "\n")

# Step 5: Process South Yorkshire files
south_combined <- map_dfr(south_files, function(file) {
  message("Processing South file: ", file)
  tryCatch({
    data <- read_csv(file, show_col_types = FALSE)
    clean_crime_data(data, "South Yorkshire")
  }, error = function(e) {
    message("Error in file: ", file)
    tibble()
  })
})
cat("Rows in south_combined:", nrow(south_combined), "\n")

# Step 6: Process West Yorkshire files
west_combined <- map_dfr(west_files, function(file) {
  message("Processing West file: ", file)
  tryCatch({
    data <- read_csv(file, show_col_types = FALSE)
    clean_crime_data(data, "West Yorkshire")
  }, error = function(e) {
    message("Error in file: ", file)
    tibble()
  })
})
cat("Rows in west_combined:", nrow(west_combined), "\n")

# Step 7: Combine all cleaned data
yorkshire_crime_all <- bind_rows(south_combined, west_combined)
cat("Rows in yorkshire_crime_all:", nrow(yorkshire_crime_all), "\n")

# Step 8: Trim and finalize dataset
yorkshire_crime_trimmed <- yorkshire_crime_all %>%
  select(Month, `Crime type`, LSOA, District, County, Population) %>%
  filter(!is.na(Month), !is.na(`Crime type`), !is.na(LSOA), !is.na(District), !is.na(County), !is.na(Population))
cat("Rows in yorkshire_crime_trimmed:", nrow(yorkshire_crime_trimmed), "\n")

# Step 9: Save cleaned dataset or warn if empty
if (nrow(yorkshire_crime_trimmed) > 0) {
  write_csv(yorkshire_crime_trimmed, "Cleaned Data/cleanedYorkshireCrime_2022-05_to_2024-12.csv")
  cat("Cleaned dataset saved to Cleaned Data/cleanedYorkshireCrime_2022-05_to_2024-12.csv with", nrow(yorkshire_crime_trimmed), "rows\n")
} else {
  cat("Warning: No rows in final dataset. CSV not saved.\n")
  write_csv(population, "Cleaned Data/debug_population.csv")
  write_csv(south_combined, "Cleaned Data/debug_south_crime.csv")
  write_csv(west_combined, "Cleaned Data/debug_west_crime.csv")
}

# Debugging: Check column names
cat("Columns in yorkshire_crime_trimmed:", colnames(yorkshire_crime_trimmed), "\n")

View(yorkshire_crime_trimmed)
