library(tidyverse)
library(lubridate)

# Read LSOA-to-Postcode mapping
LSOA_to_Postcode <- read_csv("Obtained Data/Postcode to LSOA 1.csv") %>%
  select(lsoa11cd, pcds, ladnm) %>%
  rename(LSOA_code = lsoa11cd, Postcode = pcds, District = ladnm) %>%
  mutate(
    shortPostcode = str_trim(substring(Postcode, 1, 4)),
    District = str_to_upper(str_trim(District))
  ) %>%
  filter(!is.na(Postcode) & !is.na(shortPostcode) & !is.na(District)) %>%
  # Select one postcode per LSOA to avoid duplication
  group_by(LSOA_code) %>%
  slice(1) %>%
  ungroup()

# Read Towns.csv for Town mapping
Towns <- read_csv("Cleaned Data/Towns.csv") %>%
  select(shortPostcode, Town) %>%
  distinct(shortPostcode, .keep_all = TRUE)

# Generate list of months from May 2022 to December 2024
start_date <- ym("2022-05")
end_date <- ym("2024-12")
months <- seq(start_date, end_date, by = "month")
month_strings <- format(months, "%Y-%m")

# Get list of all crime CSV files
crime_files <- unlist(lapply(month_strings, function(month) {
  c(
    paste0("Obtained Data/crime/", month, "/", month, "-south-yorkshire-street.csv"),
    paste0("Obtained Data/crime/", month, "/", month, "-west-yorkshire-street.csv")
  )
}))

# Filter for existing files
crime_files <- crime_files[file.exists(crime_files)]

# Read and combine all crime data
crime_data <- map_dfr(crime_files, function(file) {
  data <- read_csv(file, show_col_types = FALSE)
  # Log parsing issues
  problems_data <- problems(data)
  if (nrow(problems_data) > 0) {
    message("Parsing issues in ", file, ": ", nrow(problems_data), " rows affected.")
    write_csv(problems_data, paste0("Cleaned Data/crime_parsing_issues_", basename(file)))
    data <- data %>% filter(!row_number() %in% problems_data$row)
  }
  data
})

# Clean crime data
crime_clean <- crime_data %>%
  # Select and rename relevant columns
  select(
    Month,
    LSOA_code = `LSOA code`,
    LSOA_name = `LSOA name`,
    CrimeType = `Crime type`,
    County = `Falls within`
  ) %>%
  # Remove NA or empty values in critical columns
  filter(!is.na(LSOA_code) & LSOA_code != "" &
           !is.na(CrimeType) & CrimeType != "" &
           !is.na(Month) & Month != "") %>%
  # Filter for relevant crime types
  filter(CrimeType %in% c("Drugs", "Vehicle crime", "Robbery")) %>%
  # Clean and standardize columns
  mutate(
    Month = str_trim(Month),
    LSOA_code = str_trim(LSOA_code),
    LSOA_name = str_trim(LSOA_name),
    CrimeType = str_trim(CrimeType),
    County = str_to_upper(str_remove(str_trim(County), " Police$")),
    Year = as.numeric(substr(Month, 1, 4))
  ) %>%
  # Join with LSOA_to_Postcode to get Postcode, shortPostcode, District
  left_join(LSOA_to_Postcode, by = "LSOA_code", relationship = "many-to-one") %>%
  # Join with Towns to get Town
  left_join(Towns, by = "shortPostcode", relationship = "many-to-one") %>%
  # Filter for South and West Yorkshire
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") & !is.na(Town)) %>%
  # Select final columns, **including Month**
  select(Postcode, shortPostcode, Town, District, County, CrimeType, Year, Month) %>%
  # Remove any remaining NA values
  filter(complete.cases(.))


# Write to CSV
write.csv(crime_clean, "Cleaned Data/cleanCrime.csv", row.names = FALSE)

View(crime_clean)
