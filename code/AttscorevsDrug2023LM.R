library(tidyverse)
library(scales)
library(ggplot2)
library(lubridate)

# Step 1: Load Data
crime_data <- read_csv("Cleaned Data/cleanedYorkshireCrime_2022-05_to_2024-12.csv")
school_scores <- read_csv("Cleaned Data/cleaned_ks4final_2022-2023.csv")

# Step 2: Clean and filter school scores (2022–2023 academic year)
school_scores <- school_scores %>%
  mutate(
    Year = str_sub(Year, 1, 4),
    LSOA = str_trim(toupper(LSOA)),
    County = str_trim(toupper(County))
  ) %>%
  filter(Year == "2022",  # Corresponds to 2022–2023 academic year
         County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  select(LSOA, County, Attainment8) %>%
  drop_na(Attainment8)

# Step 3: Filter drug offenses in 2023
drug_data <- crime_data %>%
  mutate(
    Month = ym(Month),
    Year = year(Month),
    LSOA = toupper(str_trim(LSOA)),
    County = toupper(str_trim(County))
  ) %>%
  filter(`Crime type` == "Drugs",
         Year == 2023,
         County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# Step 4: Aggregate drug crime counts and compute rate
drug_rates <- drug_data %>%
  group_by(LSOA, County, Population) %>%
  summarise(drug_offenses = n(), .groups = "drop") %>%
  mutate(drug_rate_per_10000 = (drug_offenses / Population) * 10000) %>%
  filter(is.finite(drug_rate_per_10000))

# Step 5: Join with school scores by LSOA and County
merged_df <- inner_join(drug_rates, school_scores, by = c("LSOA", "County")) %>%
  filter(is.finite(Attainment8))

# Step 6: Correlation
cor_test <- cor.test(merged_df$Attainment8, merged_df$drug_rate_per_10000)
print(cor_test)

# Step 7: Linear model
model <- lm(drug_rate_per_10000 ~ Attainment8, data = merged_df)
summary(model)

# Step 8: Plot
ggplot(merged_df, aes(x = Attainment8, y = drug_rate_per_10000, color = County)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE,fullrange=TRUE) +
  scale_y_continuous(name = "Drug Offense Rate per 10,000 People") +
  scale_x_continuous(name = "Attainment 8 Score") +
  labs(
    title = "Attainment 8 vs Drug Offense Rate per 10,000 People (2023)",
    subtitle = paste0("Pearson r = ", round(cor_test$estimate, 2),
                      ", p‑value = ", signif(cor_test$p.value, 2))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
