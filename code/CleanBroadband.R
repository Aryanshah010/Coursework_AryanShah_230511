library(tidyverse)

# Step 1: Define South and West Yorkshire districts
south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
west_yorkshire_districts <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")
yorkshire_districts <- c(south_yorkshire_districts, west_yorkshire_districts)

# Step 2: Load datasets
broadband_perf <- read_csv("Obtained Data/Broadband/201805_fixed_pc_performance_r03.csv")
broadband_cov <- read_csv("Obtained Data/Broadband/201809_fixed_pc_coverage_r01.csv")
postcode_lsoa <- read_csv("Obtained Data/Postcode to LSOA 1.csv")

# Debugging: Check initial data
cat("Rows in broadband performance:", nrow(broadband_perf), "\n")
cat("Sample postcodes in broadband_perf:", head(broadband_perf$postcode, 5), "\n")
cat("Rows in broadband coverage:", nrow(broadband_cov), "\n")
cat("Sample postcodes in broadband_cov:", head(broadband_cov$postcode, 5), "\n")
cat("Rows in Postcode to LSOA 1.csv:", nrow(postcode_lsoa), "\n")
cat("Sample postcodes in postcode_lsoa:", head(postcode_lsoa$pcds, 5), "\n")
cat("Unique ladnm values in postcode_lsoa:", unique(postcode_lsoa$ladnm), "\n")

# Step 3: Clean and prepare postcode-to-LSOA lookup
postcode_lsoa <- postcode_lsoa %>%
  mutate(
    Postcode = toupper(gsub(" ", "", pcds)),  # Standardize full postcode
    LSOA = lsoa11cd,  # Use LSOA code (e.g., E01012345)
    District = toupper(ladnm),
    County = case_when(
      str_detect(toupper(ladnm), "SHEFFIELD|BARNSLEY|DONCASTER|ROTHERHAM") ~ "South Yorkshire",
      str_detect(toupper(ladnm), "LEEDS|BRADFORD|CALDERDALE|KIRKLEES|WAKEFIELD") ~ "West Yorkshire",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(County)) %>%  # Keep only South/West Yorkshire rows
  select(Postcode, LSOA, District, County) %>%
  distinct()

# Debugging: Check postcode_lsoa after filtering
cat("Rows in postcode_lsoa after filtering for South/West Yorkshire:", nrow(postcode_lsoa), "\n")
cat("Unique districts in postcode_lsoa:", unique(postcode_lsoa$District), "\n")
cat("Sample LSOA in postcode_lsoa:", head(postcode_lsoa$LSOA, 5), "\n")

# Step 4: Clean broadband performance data
broadband_perf_clean <- broadband_perf %>%
  mutate(
    postcode = toupper(gsub(" ", "", postcode))
  ) %>%
  select(postcode, `Average download speed (Mbit/s)`) %>%
  filter(
    !is.na(postcode),
    !is.na(`Average download speed (Mbit/s)`)
  )

# Debugging: Check broadband_perf_clean
cat("Rows in broadband_perf_clean:", nrow(broadband_perf_clean), "\n")
cat("Sample postcodes in broadband_perf_clean:", head(broadband_perf_clean$postcode, 5), "\n")
cat("Number of broadband_perf postcodes in postcode_lsoa:", sum(broadband_perf_clean$postcode %in% postcode_lsoa$Postcode), "\n")

# Step 5: Map performance to LSOA, District, County
broadband_perf_mapped <- broadband_perf_clean %>%
  left_join(postcode_lsoa, by = c("postcode" = "Postcode")) %>%
  filter(
    !is.na(LSOA),
    !is.na(District),
    !is.na(County)
  ) %>%
  select(postcode, `Average download speed (Mbit/s)`, LSOA, District, County)

# Debugging: Check broadband_perf_mapped
cat("Rows in broadband_perf_mapped:", nrow(broadband_perf_mapped), "\n")
cat("Unique districts in broadband_perf_mapped:", unique(broadband_perf_mapped$District), "\n")
cat("Sample LSOA in broadband_perf_mapped:", head(broadband_perf_mapped$LSOA, 5), "\n")

# Step 6: Save cleaned performance dataset
if (nrow(broadband_perf_mapped) > 0) {
  write_csv(broadband_perf_mapped, "Cleaned Data/cleanBroadbandPerformance.csv")
  cat("Cleaned performance dataset saved to Cleaned Data/cleanBroadbandPerformance.csv with", nrow(broadband_perf_mapped), "rows\n")
} else {
  cat("Warning: No rows in final broadband performance dataset. CSV not saved.\n")
  cat("Check postcode formats in broadband performance and Postcode to LSOA 1.csv, or verify South/West Yorkshire districts.\n")
  write_csv(broadband_perf_clean, "Cleaned Data/debug_broadband_perf.csv")
  write_csv(postcode_lsoa, "Cleaned Data/debug_postcode_lsoa.csv")
}

# Step 7: Clean broadband coverage data
broadband_cov_clean <- broadband_cov %>%
  mutate(
    postcode = toupper(gsub(" ", "", postcode))
  ) %>%
  select(
    postcode,
    `SFBB availability (% premises)`,
    `UFBB availability (% premises)`,
    `FTTP availability (% premises)`
  ) %>%
  filter(
    !is.na(postcode),
    (`SFBB availability (% premises)` > 0) |
      (`UFBB availability (% premises)` > 0) |
      (`FTTP availability (% premises)` > 0)
  )

# Debugging: Check broadband_cov_clean
cat("Rows in broadband_cov_clean:", nrow(broadband_cov_clean), "\n")
cat("Sample postcodes in broadband_cov_clean:", head(broadband_cov_clean$postcode, 5), "\n")
cat("Number of broadband_cov postcodes in postcode_lsoa:", sum(broadband_cov_clean$postcode %in% postcode_lsoa$Postcode), "\n")

# Step 8: Map coverage to LSOA, District, County
broadband_cov_mapped <- broadband_cov_clean %>%
  left_join(postcode_lsoa, by = c("postcode" = "Postcode")) %>%
  filter(
    !is.na(LSOA),
    !is.na(District),
    !is.na(County)
  ) %>%
  select(
    postcode,
    `SFBB availability (% premises)`,
    `UFBB availability (% premises)`,
    `FTTP availability (% premises)`,
    LSOA,
    District,
    County
  )

# Debugging: Check broadband_cov_mapped
cat("Rows in broadband_cov_mapped:", nrow(broadband_cov_mapped), "\n")
cat("Unique districts in broadband_cov_mapped:", unique(broadband_cov_mapped$District), "\n")
cat("Sample LSOA in broadband_cov_mapped:", head(broadband_cov_mapped$LSOA, 5), "\n")

# Step 9: Save cleaned coverage dataset
if (nrow(broadband_cov_mapped) > 0) {
  write_csv(broadband_cov_mapped, "Cleaned Data/cleanBroadbandCoverage.csv")
  cat("Cleaned coverage dataset saved to Cleaned Data/cleanBroadbandCoverage.csv with", nrow(broadband_cov_mapped), "rows\n")
} else {
  cat("Warning: No rows in final broadband coverage dataset. CSV not saved.\n")
  cat("Check postcode formats in broadband coverage and Postcode to LSOA 1.csv, or verify South/West Yorkshire districts.\n")
  write_csv(broadband_cov_clean, "Cleaned Data/debug_broadband_cov.csv")
  write_csv(postcode_lsoa, "Cleaned Data/debug_postcode_lsoa.csv")
}

View(broadband_cov_mapped)
View(broadband_perf_mapped)
