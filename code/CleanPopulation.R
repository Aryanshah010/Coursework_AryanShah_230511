library(tidyverse)

# Step 1: Define South and West Yorkshire districts
south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
west_yorkshire_districts <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")
yorkshire_districts <- c(south_yorkshire_districts, west_yorkshire_districts)

# Step 2: Load datasets
population <- read_csv("Obtained Data/Population2011.csv")
postcode_lsoa <- read_csv("Obtained Data/Postcode to LSOA 1.csv")

# Debugging: Check initial data
cat("Rows in Population2011.csv:", nrow(population), "\n")
cat("Sample postcodes in population:", head(population$Postcode, 5), "\n")
cat("Rows in Postcode to LSOA 1.csv:", nrow(postcode_lsoa), "\n")
cat("Sample postcodes in postcode_lsoa:", head(postcode_lsoa$pcds, 5), "\n")

# Step 3: Clean and prepare postcode-to-LSOA lookup
postcode_lsoa <- postcode_lsoa %>%
  mutate(
    PartialPostcode = toupper(gsub(" ", "", substring(pcds, 1, 4))),  # Use first 4 characters
    LSOA = lsoa11cd,  # Use LSOA code (e.g., E01012345)
    District = toupper(ladnm)
  ) %>%
  filter(District %in% yorkshire_districts) %>%  # Restrict to South/West Yorkshire
  select(PartialPostcode, LSOA, District) %>%
  distinct()

# Debugging: Check postcode_lsoa after filtering
cat("Rows in postcode_lsoa after filtering for South/West Yorkshire:", nrow(postcode_lsoa), "\n")
cat("Unique districts in postcode_lsoa:", unique(postcode_lsoa$District), "\n")
cat("Sample LSOA in postcode_lsoa:", head(postcode_lsoa$LSOA, 5), "\n")

# Step 4: Define Yorkshire postcode prefixes
yorkshire_prefixes <- c("S", "DN", "LS", "BD", "WF", "HX", "HD")

# Step 5: Clean population postcodes and filter for Yorkshire
population_yorkshire <- population %>%
  mutate(
    PartialPostcode = toupper(gsub(" ", "", substring(Postcode, 1, 4)))  # Use first 4 characters
  ) %>%
  filter(substring(PartialPostcode, 1, 2) %in% yorkshire_prefixes | substring(PartialPostcode, 1, 1) %in% yorkshire_prefixes)

# Debugging: Check population_yorkshire
cat("Rows in population_yorkshire:", nrow(population_yorkshire), "\n")
cat("Sample PartialPostcode in population_yorkshire:", head(population_yorkshire$PartialPostcode, 5), "\n")

# Step 6: Join with LSOA lookup using partial postcodes
population_lsoa <- population_yorkshire %>%
  left_join(postcode_lsoa, by = "PartialPostcode", relationship = "many-to-many") %>%
  mutate(
    Postcode = Postcode  # Retain original Postcode
  ) %>%
  filter(!is.na(LSOA)) %>%  # Keep only rows with valid LSOA
  select(Postcode, Population, LSOA, District) %>%
  distinct()

# Debugging: Check population_lsoa
cat("Rows in population_lsoa:", nrow(population_lsoa), "\n")
cat("Unique districts in population_lsoa:", unique(population_lsoa$District), "\n")
cat("Sample LSOA in population_lsoa:", head(population_lsoa$LSOA, 5), "\n")

# Step 7: Save cleaned dataset or warn if empty
if (nrow(population_lsoa) > 0) {
  write_csv(population_lsoa, "Cleaned Data/cleanPopulation2011.csv")
  cat("Cleaned dataset saved to Cleaned Data/cleanPopulation2011.csv with", nrow(population_lsoa), "rows\n")
} else {
  cat("Warning: No rows in final population dataset. CSV not saved.\n")
  cat("Check postcode formats in Population2011.csv and Postcode to LSOA 1.csv, or verify Yorkshire prefixes and districts.\n")
  write_csv(population_yorkshire, "Cleaned Data/debug_population_yorkshire.csv")
  write_csv(postcode_lsoa, "Cleaned Data/debug_postcode_lsoa.csv")
}

View(population_lsoa)
