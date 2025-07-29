# Loading required libraries
library(tidyverse)
library(ggplot2)

# Loading cleaned datasets (adjust file paths as needed)
house<-read_csv("Cleaned Data/cleanHousePrices.csv")
crime<-read_csv("Cleaned Data/cleanCrime.csv")
schools<-read_csv("Cleaned Data/cleanSchool.csv")
broadband<-read_csv("Cleaned Data/cleanBroadbandPerformance.csv")
towns<-read_csv("Cleaned Data/Towns.csv") # Columns: Town, County, Year, Attainment8Score

# Step 1: Aggregate crime data (2022–2024)
crime_agg <- crime %>%
  filter(Year %in% 2022:2024) %>%
  group_by(Town, County, shortPostcode) %>%
  summarise(CrimeCount = n(), .groups = "drop") %>%
  left_join(select(towns, shortPostcode, Town, County, Population2024), by = c("shortPostcode", "Town", "County")) %>%
  filter(!is.na(Population2024)) %>%
  mutate(CrimeRate = (CrimeCount / Population2024) * 1000) %>%
  # Remove duplicates, keeping first occurrence
  distinct(Town, County, .keep_all = TRUE) %>%
  select(Town, County, CrimeRate)

# Check for duplicates
print("Duplicates in crime_agg:")
print(sum(duplicated(crime_agg[, c("Town", "County")])))

# Step 2: Aggregate house prices (2021–2024)
house_prices_agg <- house %>%
  filter(Year %in% 2021:2024) %>%
  group_by(Town, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop") %>%
  # Remove duplicates
  distinct(Town, County, .keep_all = TRUE) %>%
  select(Town, County, avg_price)

# Check for duplicates
print("Duplicates in house_prices_agg:")
print(sum(duplicated(house_prices_agg[, c("Town", "County")])))

# Step 3: Aggregate school scores (2022–2024)
schools_agg <- school %>%
  mutate(
    Town = toupper(Town),
    County   = toupper(County),
    Year     = year
  ) %>%
  filter(Year %in% 2022:2024) %>%
  group_by(Town, County) %>%
  summarise(AvgAttainment8 = mean(attainment_8_score, na.rm = TRUE), .groups = "drop") %>% 
  # Remove duplicates
  distinct(Town, County, .keep_all = TRUE) %>%
  select(Town, County, AvgAttainment8)

# Check for duplicates
print("Duplicates in schools_agg:")
print(sum(duplicated(schools_agg[, c("Town", "County")])))

# Step 4: Aggregate broadband data (assumed 2023)
broadband_agg <- broadband %>%
  group_by(Town, County) %>%
  summarise(AvgDownload = mean(AvgDownload, na.rm = TRUE), .groups = "drop") %>%
  # Remove duplicates
  distinct(Town, County, .keep_all = TRUE) %>%
  select(Town, County, AvgDownload)

# Check for duplicates
print("Duplicates in broadband_agg:")
print(sum(duplicated(broadband_agg[, c("Town", "County")])))

# Step 5: Merge all datasets
merged_data <- crime_agg %>%
  left_join(house_prices_agg, by = c("Town", "County")) %>%
  left_join(broadband_agg, by = c("Town", "County"), relationship = "one-to-one") %>%
  left_join(schools_agg, by = c("Town", "County")) %>%
  filter(!is.na(avg_price) & !is.na(AvgDownload) & !is.na(AvgAttainment8))  # Remove incomplete rows

# Check for duplicates in merged data
print("Duplicates in merged_data:")
print(sum(duplicated(merged_data[, c("Town", "County")])))

View(merged_data)

# Step 6: Normalize metrics to 0–10 scale, handling edge cases
merged_data <- merged_data %>%
  mutate(
    # Affordability (lower price is better)
    AffordabilityScore = case_when(
      max(avg_price, na.rm = TRUE) == min(avg_price, na.rm = TRUE) ~ 5,
      TRUE ~ 10 * (max(avg_price, na.rm = TRUE) - avg_price) / 
        (max(avg_price, na.rm = TRUE) - min(avg_price, na.rm = TRUE))
    ),
    # Connectivity (higher speed is better)
    ConnectivityScore = case_when(
      max(AvgDownload, na.rm = TRUE) == min(AvgDownload, na.rm = TRUE) ~ 5,
      TRUE ~ 10 * (AvgDownload - min(AvgDownload, na.rm = TRUE)) / 
        (max(AvgDownload, na.rm = TRUE) - min(AvgDownload, na.rm = TRUE))
    ),
    # Safety (lower crime rate is better)
    SafetyScore = case_when(
      max(CrimeRate, na.rm = TRUE) == min(CrimeRate, na.rm = TRUE) ~ 5,
      TRUE ~ 10 * (max(CrimeRate, na.rm = TRUE) - CrimeRate) / 
        (max(CrimeRate, na.rm = TRUE) - min(CrimeRate, na.rm = TRUE))
    ),
    # Quality of Life (higher school score is better)
    QualityScore = case_when(
      max(AvgAttainment8, na.rm = TRUE) == min(AvgAttainment8, na.rm = TRUE) ~ 5,
      TRUE ~ 10 * (AvgAttainment8 - min(AvgAttainment8, na.rm = TRUE)) / 
        (max(AvgAttainment8, na.rm = TRUE) - min(AvgAttainment8, na.rm = TRUE))
    )
  )

# Diagnostic: Check for zero or NA scores
print("Rows with zero or NA scores:")
print(merged_data %>% 
        filter(AffordabilityScore == 0 | ConnectivityScore == 0 | 
                 SafetyScore == 0 | QualityScore == 0 |
                 is.na(AffordabilityScore) | is.na(ConnectivityScore) | 
                 is.na(SafetyScore) | is.na(QualityScore)) %>%
        select(Town, County, avg_price, AvgDownload, CrimeRate, AvgAttainment8, 
               AffordabilityScore, ConnectivityScore, SafetyScore, QualityScore))

# Step 7: Calculate composite score with weights (40% affordability, 20% each for others)
merged_data <- merged_data %>%
  mutate(CompositeScore = 0.4 * AffordabilityScore + 
           0.2 * ConnectivityScore + 
           0.2 * SafetyScore + 
           0.2 * QualityScore)

# Step 8: Select top 3 towns
top_towns <- merged_data %>%
  arrange(desc(CompositeScore)) %>%
  select(Town, County, AffordabilityScore, ConnectivityScore, SafetyScore, QualityScore, CompositeScore) %>%
  head(10)

# Print results
print("Top 3 Towns for Property Investment:")
print(top_towns)
View(top_towns)

write.csv(top_towns, "recommendation system/recommendation_data.csv", row.names = FALSE)
