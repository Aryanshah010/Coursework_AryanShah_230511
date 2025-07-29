library(tidyverse)

# 1. Read in House Prices to get the Town ↔ District mapping
HousePrices <- read_csv("Cleaned Data/cleanHousePrices.csv")

# 2. Load and aggregate 2011 population by outward postcode
PopulationData <- read_csv("Obtained Data/Population2011.csv") %>%
  mutate(
    shortPostcode = str_trim(str_sub(Postcode, 1, 4))
  ) %>%
  group_by(shortPostcode) %>%
  summarise(
    Pop2011 = sum(Population, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Project 2011 → 2021–2024 using your growth factors
growth_factors <- c(
  "2021" = 1.0777505487,
  "2022" = 1.0820490383,
  "2023" = 1.0869272715,
  "2024" = 1.0915110625
)

PopulationData <- PopulationData %>%
  mutate(
    Population2021 = Pop2011 * growth_factors["2021"],
    Population2022 = Pop2011 * growth_factors["2022"],
    Population2023 = Pop2011 * growth_factors["2023"],
    Population2024 = Pop2011 * growth_factors["2024"]
  ) %>%
  select(-Pop2011)

# 4. Build initial Town ↔ District ↔ County table
towns_raw <- HousePrices %>%
  select(shortPostcode, Town, District, County) %>%
  distinct() %>%
  left_join(PopulationData, by = "shortPostcode") %>%
  filter(!is.na(Population2021))

# 5. Explicitly correct Yorkshire districts
south_yorkshire <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
west_yorkshire  <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")

towns_clean <- towns_raw %>%
  # normalize case
  mutate(
    District = str_to_upper(str_trim(District)),
    County   = str_to_upper(str_trim(County))
  ) %>%
  # overwrite any mis‑assigned county based on district
  mutate(
    County = case_when(
      District %in% south_yorkshire ~ "SOUTH YORKSHIRE",
      District %in% west_yorkshire  ~ "WEST YORKSHIRE",
      TRUE                           ~ County
    )
  ) %>%
  # keep only the two target counties
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  arrange(County, District, Town)

# 6. Save out your cleaned lookup
write_csv(towns_clean, "Cleaned Data/Towns.csv")
