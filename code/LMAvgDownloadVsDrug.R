library(tidyverse)
library(scales)

# Load datasets
crime <- read_csv("Cleaned Data/cleanCrime.csv")
towns <- read_csv("Cleaned Data/Towns.csv")

# Standardize short postcodes
crime <- crime %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode))) %>%
  select(-District, -County)

towns <- towns %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

# Step 1: Deduplicate towns to avoid postcode ambiguity
towns_clean <- towns %>%
  group_by(shortPostcode, District, County) %>%
  summarise(n = n(), Population2023 = mean(Population2023, na.rm = TRUE), .groups = "drop") %>%
  group_by(shortPostcode) %>%
  slice_max(n, n = 1) %>%  
  ungroup()


crime_filtered <- crime %>%
  filter(CrimeType == "Drugs", Year == 2023) %>%
  left_join(
    towns_clean %>% select(shortPostcode, District, County, Population2023),
    by = "shortPostcode"
  )

crime_rates <- crime_filtered %>%
  drop_na(Population2023, District) %>%
  group_by(District, County, Year) %>%
  summarise(
    DrugOffenseCount = n(),
    Population = first(Population2023),
    DrugOffenseRate = (DrugOffenseCount / Population) * 10000,
    .groups = "drop"
  )


# Load cleaned broadband data (assumed to be mostly 2023)
broadband <- read_csv("Cleaned Data/cleanBroadbandPerformance.csv")

# Define Yorkshire districts
south_yorkshire <- c("SHEFFIELD", "BARNSLEY", "ROTHERHAM", "DONCASTER")
west_yorkshire  <- c("LEEDS", "BRADFORD", "WAKEFIELD", "KIRKLEES", "CALDERDALE")
yorkshire_districts <- c(south_yorkshire, west_yorkshire)

# Filter and prepare broadband data (already at district level)
broadband_clean <- broadband %>%
  filter(District %in% yorkshire_districts) %>%
  group_by(District, County) %>%
  summarise(avg_download = mean(AvgDownload, na.rm = TRUE), .groups = "drop")

# Filter and prepare crime rates (from previous script)
crime_rates_2023 <- crime_rates %>%
  filter(Year == 2023, District %in% yorkshire_districts)

# Join broadband and crime
bb_crime_joined <- inner_join(broadband_clean, crime_rates_2023, by = c("District", "County"))

# Label CountyGroup
bb_crime_joined <- bb_crime_joined %>%
  mutate(CountyGroup = case_when(
    District %in% south_yorkshire ~ "South Yorkshire",
    District %in% west_yorkshire ~ "West Yorkshire"
  ))

# --- Correlation ---
correlation <- bb_crime_joined %>%
  group_by(CountyGroup) %>%
  summarise(
    correlation = cor(avg_download, DrugOffenseRate, use = "complete.obs")
  )
print("📊 Correlation by CountyGroup:")
print(correlation)

# --- Linear Model ---
model <- lm(DrugOffenseRate ~ avg_download * CountyGroup, data = bb_crime_joined)
print("📈 Linear Model Summary:")
summary(model)

# --- Plot ---
ggplot(bb_crime_joined, aes(x = avg_download, y = DrugOffenseRate, color = CountyGroup)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Average Download Speed vs Drug Offense Rate (per 10,000) in 2023",
    x = "Average Download Speed (Mbps)",
    y = "Drug Offense Rate per 10,000 People",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )
