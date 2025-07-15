library(tidyverse)

# Load datasets
population <- read_csv("Obtained Data/Population2011.csv")
lsoa_lookup <- read_csv("Cleaned Data/cleanPostcodeLSOA.csv")

# Define Yorkshire postcode prefixes
yorkshire_prefixes <- c("S", "DN", "LS", "BD", "WF", "HX", "HD")

# Clean population data and match with LSOA
population_clean <- population %>%
  mutate(
    Postcode = toupper(gsub(" ", "", Postcode))  # Standardize (e.g., "BD1  1" -> "BD11")
  ) %>%
  filter(substring(Postcode, 1, 2) %in% yorkshire_prefixes | substring(Postcode, 1, 1) %in% yorkshire_prefixes) %>%
  left_join(
    lsoa_lookup %>% 
      mutate(PartialPostcode = toupper(gsub(" ", "", substring(Postcode, 1, 4)))),  # Extract e.g., "BD11" from "BD11AA"
    by = c("Postcode" = "PartialPostcode")
  ) %>%
  filter(!is.na(District), !is.na(County)) %>%  # Keep only South/West Yorkshire
  select(Postcode, Population, District, County) %>%
  distinct()

View(population_clean)

# Save cleaned dataset
write_csv(population_clean, "Cleaned Data/cleanPopulation2011.csv")

# Diagnostic
cat("Rows in cleaned population dataset:", nrow(population_clean), "\n")
cat("Unique Districts:", unique(population_clean$District), "\n")
cat("Unique Counties:", unique(population_clean$County), "\n")
