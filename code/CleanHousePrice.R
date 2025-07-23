library(tidyverse)

# Step 1: Define South and West Yorkshire districts
south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
west_yorkshire_districts <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")
yorkshire_districts <- c(south_yorkshire_districts, west_yorkshire_districts)

# Step 2: Load datasets
house_prices <- read_csv("Cleaned Data/cleanHousePrices.csv")
postcode_lsoa <- read_csv("Obtained Data/Postcode to LSOA 1.csv")

# Debugging: Check initial data
cat("Rows in cleanHousePrices.csv:", nrow(house_prices), "\n")
cat("Sample postcodes in house_prices:", head(house_prices$Postcode, 5), "\n")
cat("Columns in cleanHousePrices.csv:", colnames(house_prices), "\n")
cat("Rows in Postcode to LSOA 1.csv:", nrow(postcode_lsoa), "\n")
cat("Sample postcodes in postcode_lsoa:", head(postcode_lsoa$pcds, 5), "\n")

# Step 3: Clean and prepare postcode-to-LSOA lookup
postcode_lsoa <- postcode_lsoa %>%
  mutate(
    Postcode = toupper(gsub(" ", "", pcds)),  # Standardize full postcode
    LSOA_code = lsoa11cd,  # Use LSOA code (e.g., E01012345)
    District_lookup = toupper(ladnm)
  ) %>%
  filter(District_lookup %in% yorkshire_districts) %>%  # Restrict to South/West Yorkshire
  select(Postcode, LSOA_code, District_lookup) %>%
  distinct()

# Debugging: Check postcode_lsoa after filtering
cat("Rows in postcode_lsoa after filtering for South/West Yorkshire:", nrow(postcode_lsoa), "\n")
cat("Unique districts in postcode_lsoa:", unique(postcode_lsoa$District_lookup), "\n")
cat("Sample LSOA_code in postcode_lsoa:", head(postcode_lsoa$LSOA_code, 5), "\n")

# Debugging: Check postcode overlap
cat("Number of house_prices postcodes in postcode_lsoa:", sum(house_prices$Postcode %in% postcode_lsoa$Postcode), "\n")

# Step 4: Clean house price postcodes
house_prices <- house_prices %>%
  mutate(Postcode = toupper(gsub(" ", "", Postcode)))

# Debugging: Check house_prices after cleaning
cat("Sample cleaned postcodes in house_prices:", head(house_prices$Postcode, 5), "\n")

# Step 5: Join and filter for valid LSOA
house_prices_lsoa <- house_prices %>%
  left_join(postcode_lsoa, by = "Postcode") %>%
  mutate(
    LSOA = if_else(!is.na(LSOA_code), LSOA_code, LSOA),  # Prefer LSOA_code from lookup, fallback to existing LSOA
    District = if_else(!is.na(District_lookup), District_lookup, District)  # Prefer lookup District
  ) %>%
  filter(!is.na(LSOA)) %>%  # Keep only rows with valid LSOA
  filter(District %in% yorkshire_districts) %>%  # Ensure only South/West Yorkshire districts
  select(Postcode, Price, Year, PropertyType, District, County, LSOA)  # Remove shortPostcode and TownCity

# Debugging: Check house_prices_lsoa
cat("Rows in house_prices_lsoa:", nrow(house_prices_lsoa), "\n")
cat("Unique districts in house_prices_lsoa:", unique(house_prices_lsoa$District), "\n")
cat("Sample LSOA in house_prices_lsoa:", head(house_prices_lsoa$LSOA, 5), "\n")
cat("Columns in house_prices_lsoa:", colnames(house_prices_lsoa), "\n")

# Step 6: Save cleaned dataset or warn if empty
if (nrow(house_prices_lsoa) > 0) {
  write_csv(house_prices_lsoa, "Cleaned Data/cleanHousePrices.csv")
  cat("Cleaned dataset saved to Cleaned Data/cleanHousePrices.csv with", nrow(house_prices_lsoa), "rows\n")
} else {
  cat("Warning: No rows in final house price dataset. CSV not saved.\n")
  cat("Check postcode formats in cleanHousePrices.csv and Postcode to LSOA 1.csv, or verify South/West Yorkshire districts.\n")
  write_csv(house_prices, "Cleaned Data/debug_house_prices.csv")
  write_csv(postcode_lsoa, "Cleaned Data/debug_postcode_lsoa.csv")
}

View(house_prices_lsoa)
