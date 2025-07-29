library(tidyverse)
library(stringr)

# 0. Read raw broadband performance data
BroadbandPerformance_raw <- read_csv("Obtained Data/Broadband/201805_fixed_pc_performance_r03.csv")

# 1. Extract outward code and average download speed
BroadbandPerformance_cleaned <- BroadbandPerformance_raw %>%
  mutate(
    shortPostcode = str_to_upper(str_extract(postcode, "^[A-Z]{1,2}[0-9R][0-9A-Z]?")),
    AvgDownload   = as.numeric(`Average download speed (Mbit/s)`)
  ) %>%
  select(shortPostcode, AvgDownload) %>%
  drop_na(shortPostcode, AvgDownload)

# 2. Load and dedupe your cleaned Towns lookup
Towns <- read_csv("Cleaned Data/Towns.csv") %>%
  mutate(shortPostcode = str_to_upper(str_trim(shortPostcode))) %>%
  distinct(shortPostcode, Town, District, County)   # ← critical: remove duplicates

# 3. Join and then aggregate
final_BroadbandPerformance <- BroadbandPerformance_cleaned %>%
  inner_join(Towns, by = "shortPostcode") %>%
  group_by(shortPostcode, Town, District, County) %>%
  summarise(
    AvgDownload = mean(AvgDownload, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))


# 5. Save
write_csv(final_BroadbandPerformance, "Cleaned Data/cleanBroadbandPerformance.csv")
