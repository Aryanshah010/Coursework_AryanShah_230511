library(tidyverse)

# Read data for 2021-2024
HousePrices2021 <- read_csv("Obtained Data/HousePrices2021.csv")
HousePrices2022 <- read_csv("Obtained Data/HousePrices2022.csv")
HousePrices2023 <- read_csv("Obtained Data/HousePrices2023.csv")
HousePrices2024 <- read_csv("Obtained Data/HousePrices2024.csv")

# Standardize column names for each dataset
colnames(HousePrices2021) <- c("TransactionID", "Price", "Date", "Postcode", "PropertyType", "OldNew", "Tenure", "HouseNumber", "Unknown1", "Street", "Locality", "TownCity", "District", "County", "PPDCategory", "RecordStatus")
colnames(HousePrices2022) <- c("TransactionID", "Price", "Date", "Postcode", "PropertyType", "OldNew", "Tenure", "HouseNumber", "Unknown1", "Street", "Locality", "TownCity", "District", "County", "PPDCategory", "RecordStatus")
colnames(HousePrices2023) <- c("TransactionID", "Price", "Date", "Postcode", "PropertyType", "OldNew", "Tenure", "HouseNumber", "Unknown1", "Street", "Locality", "TownCity", "District", "County", "PPDCategory", "RecordStatus")
colnames(HousePrices2024) <- c("TransactionID", "Price", "Date", "Postcode", "PropertyType", "OldNew", "Tenure", "HouseNumber", "Unknown1", "Street", "Locality", "TownCity", "District", "County", "PPDCategory", "RecordStatus")

# Clean each dataset: Remove rows with NA in Price, Postcode, or District
HousePrices2021 <- HousePrices2021 %>%
  filter(!is.na(Price) & !is.na(Postcode) & !is.na(District))
HousePrices2022 <- HousePrices2022 %>%
  filter(!is.na(Price) & !is.na(Postcode) & !is.na(District))
HousePrices2023 <- HousePrices2023 %>%
  filter(!is.na(Price) & !is.na(Postcode) & !is.na(District))
HousePrices2024 <- HousePrices2024 %>%
  filter(!is.na(Price) & !is.na(Postcode) & !is.na(District))

# Combine datasets
HousePrices <- bind_rows(HousePrices2021, HousePrices2022, HousePrices2023, HousePrices2024)

# Clean the combined dataset
cleanHousePrices <- HousePrices %>%
  # Convert County and District to uppercase for consistency
  mutate(County = toupper(County),
         District = toupper(District)) %>%
  # Filter for South Yorkshire and West Yorkshire
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  # Create shortPostcode (first 4 characters) and trim whitespace
  mutate(shortPostcode = str_trim(substring(Postcode, 1, 4))) %>%
  # Extract year from Date
  mutate(Year = substring(Date, 1, 4)) %>%
  # Filter for 2021-2024
  filter(Year %in% c("2021", "2022", "2023", "2024")) %>%
  # Remove duplicates
  distinct() %>%
  # Select relevant columns, including TownCity for potential use with other datasets
  select(Postcode, shortPostcode, Price, Year, PropertyType, District, TownCity, County) %>%
  # Sort by County
  arrange(County)

# Write to file
write.csv(cleanHousePrices, "Cleaned Data/cleanHousePrices.csv", row.names = FALSE)

# View the cleaned data
View(cleanHousePrices)
