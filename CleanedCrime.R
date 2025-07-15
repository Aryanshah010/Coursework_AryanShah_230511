library(tidyverse)

# Define Yorkshire districts
south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
west_yorkshire_districts  <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")
yorkshire_districts       <- c(south_yorkshire_districts, west_yorkshire_districts)

# Load population
population <- read_csv("Cleaned Data/cleanPopulation2011.csv") %>%
  group_by(District, County) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# Crime cleaning function
clean_crime_data <- function(crime_data, region_name) {
  region_districts <- if (region_name == "South Yorkshire") {
    south_yorkshire_districts
  } else {
    west_yorkshire_districts
  }
  
  crime_data %>%
    filter(!is.na(`LSOA code`), !is.na(`LSOA name`), !is.na(`Crime type`)) %>%
    filter(str_detect(toupper(`LSOA name`), paste(toupper(region_districts), collapse = "|"))) %>%
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
      )
    ) %>%
    filter(District %in% region_districts) %>%
    left_join(population, by = c("District", "County"), suffix = c("", ".pop")) %>%
    select(
      `Crime ID`, Month, Location,
      `LSOA code`, `LSOA name`, `Crime type`, `Last outcome category`,
      District, County, Population
    ) %>%
    distinct() %>%
    filter(!is.na(District), !is.na(County)) %>%
    mutate(Region = region_name)
}

# 🔁 Recursively list all monthly crime files in subfolders
all_files <- list.files("Obtained Data/crime/", pattern = "-yorkshire-street\\.csv$", recursive = TRUE, full.names = TRUE)

# Separate South and West Yorkshire files
south_files <- all_files[str_detect(all_files, "south-yorkshire")]
west_files  <- all_files[str_detect(all_files, "west-yorkshire")]

# 🔄 Process all South Yorkshire files
south_combined <- map_dfr(south_files, function(file) {
  message("Processing South file: ", file)
  tryCatch({
    data <- read_csv(file, show_col_types = FALSE)
    clean_crime_data(data, "South Yorkshire")
  }, error = function(e) {
    message("Error in file: ", file)
    tibble()  # return empty in case of error
  })
})

# 🔄 Process all West Yorkshire files
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

# 📦 Combine all cleaned data
yorkshire_crime_all <- bind_rows(south_combined, west_combined)

yorkshire_crime_trimmed <- yorkshire_crime_all %>%
  select(Month, `Crime type`, District, County, Population) %>%
  filter(!is.na(District), !is.na(County), !is.na(`Crime type`), !is.na(Population))


write_csv(yorkshire_crime_trimmed, "Cleaned Data/cleanedYorkshireCrime_2022-05_to_2024-12.csv")

colnames(yorkshire_crime_trimmed)
View(yorkshire_crime_trimmed)
