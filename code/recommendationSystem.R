library(tidyverse)

crime <- read_csv("Cleaned Data/cleanedYorkshireCrime_2022-05_to_2024-12.csv")
house <- read_csv("Cleaned Data/cleanHousePrices.csv")
broadband <- read_csv("Cleaned Data/cleanBroadbandPerformance.csv")
school<- read_csv("Cleaned Data/cleaned_ks4final_all_years.csv")

# Step 1: Process and aggregate data by LSOA

# Affordability: Average house price per LSOA
affordability <- house %>%
  group_by(LSOA, District) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(AvgPrice))

# Connectivity: Average broadband speed per LSOA
connectivity <- broadband %>%
  group_by(LSOA, District) %>%
  summarise(AvgSpeed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(AvgSpeed))

# Safety: Crime rate per LSOA (crimes per 1000 people)
crime_agg <- crime %>%
  group_by(LSOA, District, Population) %>%
  summarise(TotalCrimes = n(), .groups = "drop") %>%
  mutate(CrimeRate = (TotalCrimes / Population) * 1000) %>%
  select(LSOA, District, CrimeRate) %>%
  filter(!is.na(CrimeRate))

# Quality of Life: Average Attainment8 score per LSOA
quality <- school %>%
  group_by(LSOA, District) %>%
  summarise(AvgAttainment8 = mean(Attainment8, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(AvgAttainment8))

# Step 2: Normalize metrics to 0–10 scale
normalize_higher <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  if (max_x == min_x) return(rep(5, length(x)))  # Handle edge case
  (x - min_x) / (max_x - min_x) * 10
}

normalize_lower <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  if (max_x == min_x) return(rep(5, length(x)))  # Handle edge case
  (max_x - x) / (max_x - min_x) * 10
}

# Apply normalization
affordability <- affordability %>%
  mutate(AffordabilityScore = normalize_lower(AvgPrice))

connectivity <- connectivity %>%
  mutate(ConnectivityScore = normalize_higher(AvgSpeed))

crime_agg <- crime_agg %>%
  mutate(SafetyScore = normalize_lower(CrimeRate))

quality <- quality %>%
  mutate(QualityScore = normalize_higher(AvgAttainment8))

# Step 3: Combine data by LSOA and calculate composite score
combined <- affordability %>%
  select(LSOA, District, AffordabilityScore) %>%
  full_join(select(connectivity, LSOA, District, ConnectivityScore), by = c("LSOA", "District")) %>%
  full_join(select(crime_agg, LSOA, District, SafetyScore), by = c("LSOA", "District")) %>%
  full_join(select(quality, LSOA, District, QualityScore), by = c("LSOA", "District")) %>%
  # Handle missing values by replacing NA with median score for that metric
  mutate(
    AffordabilityScore = replace(AffordabilityScore, is.na(AffordabilityScore), median(AffordabilityScore, na.rm = TRUE)),
    ConnectivityScore = replace(ConnectivityScore, is.na(ConnectivityScore), median(ConnectivityScore, na.rm = TRUE)),
    SafetyScore = replace(SafetyScore, is.na(SafetyScore), median(SafetyScore, na.rm = TRUE)),
    QualityScore = replace(QualityScore, is.na(QualityScore), median(QualityScore, na.rm = TRUE))
  ) %>%
  # Calculate composite score with weights
  mutate(
    CompositeScore = 0.4 * AffordabilityScore + 0.2 * ConnectivityScore + 
      0.2 * SafetyScore + 0.2 * QualityScore
  )

# Step 4: Aggregate to District level and rank
top_districts <- combined %>%
  group_by(District) %>%
  summarise(
    AffordabilityScore = mean(AffordabilityScore, na.rm = TRUE),
    ConnectivityScore = mean(ConnectivityScore, na.rm = TRUE),
    SafetyScore = mean(SafetyScore, na.rm = TRUE),
    QualityScore = mean(QualityScore, na.rm = TRUE),
    CompositeScore = mean(CompositeScore, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(CompositeScore)) %>%
  slice_head(n = 3)

View(top_districts)

# Save top districts to CSV
write_csv(top_districts, "recommendation system/top3_recommended_districts.csv")

