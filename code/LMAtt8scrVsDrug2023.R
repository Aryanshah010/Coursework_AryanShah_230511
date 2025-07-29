library(tidyverse)
library(scales)

# Load datasets
crime <- read_csv("Cleaned Data/cleanCrime.csv")
towns <- read_csv("Cleaned Data/Towns.csv")
school<-read_csv("Cleaned Data/cleanSchool.csv")

# Standardize short postcodes
crime <- crime %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode))) %>%
  select(-District, -County)  # Remove to avoid .x/.y issues

towns <- towns %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

# Step 1: Deduplicate towns to avoid postcode ambiguity
towns_clean <- towns %>%
  group_by(shortPostcode, District, County) %>%
  summarise(n = n(), Population2023 = mean(Population2023, na.rm = TRUE), .groups = "drop") %>%
  group_by(shortPostcode) %>%
  slice_max(n, n = 1) %>%   # Take most common mapping
  ungroup()

# Step 2: Join towns to crime (after filtering for Drugs 2023)
crime_filtered <- crime %>%
  filter(CrimeType == "Drugs", Year == 2023) %>%
  left_join(
    towns_clean %>% select(shortPostcode, District, County, Population2023),
    by = "shortPostcode"
  )

# Step 3: Drop rows with missing District/Population, compute drug rate
crime_rates <- crime_filtered %>%
  drop_na(Population2023, District) %>%
  group_by(District, County, Year) %>%
  summarise(
    DrugOffenseCount = n(),
    Population = first(Population2023),
    DrugOffenseRate = (DrugOffenseCount / Population) * 10000,
    .groups = "drop"
  )

south_yorkshire <- c("SHEFFIELD", "BARNSLEY", "ROTHERHAM", "DONCASTER")
west_yorkshire  <- c("LEEDS", "BRADFORD", "WAKEFIELD", "KIRKLEES", "CALDERDALE")
yorkshire_districts <- c(south_yorkshire, west_yorkshire)


school_clean <- school %>%
  mutate(
    District = toupper(District),
    Year = year
  ) %>%
  filter(Year == 2023) %>%
  group_by(District, County) %>%
  summarise(
    avg_attainment8 = mean(attainment_8_score, na.rm = TRUE),
    .groups = "drop"
  )

# Filter and combine datasets for only the defined districts
edu_crime_combined <- inner_join(
  school_clean %>% filter(District %in% yorkshire_districts),
  crime_rates %>% filter(District %in% yorkshire_districts),
  by = c("District", "County")
)

# Add county group label
edu_crime_combined <- edu_crime_combined %>%
  mutate(CountyGroup = case_when(
    District %in% south_yorkshire ~ "South Yorkshire",
    District %in% west_yorkshire ~ "West Yorkshire"
  ))

correlation <- edu_crime_combined %>%
  group_by(CountyGroup) %>%
  summarise(
    correlation = cor(avg_attainment8, DrugOffenseRate, use = "complete.obs")
  )
print("📊 Correlation by CountyGroup:")
print(correlation)

# --- Linear Model ---
model <- lm(DrugOffenseRate ~ avg_attainment8 * CountyGroup, data = edu_crime_combined)
print("📈 Linear Model Summary:")
summary(model)

ggplot(edu_crime_combined, aes(x = avg_attainment8, y = DrugOffenseRate, color = CountyGroup)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Attainment 8 Score vs Drug Offense Rate (per 10,000) in 2023",
    x = "Average Attainment 8 Score",
    y = "Drug Offense Rate per 10,000 People",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )
