library(tidyverse)

# 0. Define South and West Yorkshire districts
yorkshire_districts <- list(
  "South Yorkshire" = c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM"),
  "West Yorkshire"  = c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")
)
all_districts <- unlist(yorkshire_districts)

# 1. Load & clean raw performance data
broadband_perf_clean <- read_csv("Obtained Data/Broadband/201805_fixed_pc_performance_r03.csv") %>%
  mutate(
    postcode = postcode %>% toupper() %>% str_replace_all(" ", "")
  ) %>%
  select(postcode, `Average download speed (Mbit/s)`) %>%
  filter(
    !is.na(postcode),
    !is.na(`Average download speed (Mbit/s)`)
  )

# 2. (Optional) Load & clean coverage data
broadband_cov_clean <- read_csv("Obtained Data/Broadband/201809_fixed_pc_coverage_r01.csv") %>%
  mutate(postcode = postcode %>% toupper() %>% str_replace_all(" ", "")) %>%
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

# 3. Build postcode → district/county lookup via LSOA
lookup <- read_csv("Obtained Data/Postcode to LSOA 1.csv") %>%
  mutate(
    Postcode = pcds %>% toupper() %>% str_replace_all(" ", ""),
    ladnm = ladnm %>% toupper()
  ) %>%
  filter(ladnm %in% all_districts) %>%
  # Map back to title-case county names
  mutate(
    County = case_when(
      ladnm %in% yorkshire_districts$`South Yorkshire` ~ "South Yorkshire",
      ladnm %in% yorkshire_districts$`West Yorkshire`  ~ "West Yorkshire",
      TRUE ~ NA_character_
    )
  ) %>%
  select(Postcode, District = ladnm, County) %>%
  filter(!is.na(County)) %>%
  distinct()

# 4. Bring in TownCity from your house‑price lookup
house_prices <- read_csv("Cleaned Data/cleanHousePrices.csv") %>%
  mutate(Postcode = Postcode %>% toupper() %>% str_replace_all(" ", "")) %>%
  select(Postcode, TownCity) %>%
  distinct()

lookup <- lookup %>%
  left_join(house_prices, by = "Postcode") %>%
  filter(!is.na(TownCity))

# 5. Map performance → district/town, drop any unmapped rows
broadband_perf_mapped <- broadband_perf_clean %>%
  left_join(lookup, by = c("postcode" = "Postcode")) %>%
  filter(
    !is.na(District),
    !is.na(County),
    !is.na(TownCity)
  )

# 6. (Optional) Quick check
summary(broadband_perf_mapped)

# 7. Save your final cleaned performance table
write_csv(broadband_perf_mapped, "Cleaned Data/cleanBroadbandPerformance.csv")

# 8. (Optional) Repeat for coverage if needed
broadband_cov_mapped <- broadband_cov_clean %>%
  left_join(lookup, by = c("postcode" = "Postcode")) %>%
  filter(
    !is.na(District),
    !is.na(County),
    !is.na(TownCity)
  )
write_csv(broadband_cov_mapped, "Cleaned Data/cleanBroadbandCoverage.csv")
