# Load libraries
library(tidyverse)
library(scales)
library(ggplot2)

# STEP 1: Load the datasets
downspeed <- read_csv("Cleaned Data/cleanBroadbandPerformance.csv")  
crime <- read_csv("Cleaned Data/cleanedYorkshireCrime_2022-05_to_2024-12.csv")

# STEP 2: Clean download speed — average by LSOA
broadband_clean <- downspeed %>%
  mutate(
    LSOA = toupper(str_trim(LSOA)),
    County = toupper(County)
  ) %>%
  group_by(LSOA, County) %>%
  summarise(avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE), .groups = "drop")

# STEP 3: Filter drug crimes for 2023 only
crime_2023 <- crime %>%
  filter(
    `Crime type` == "Drugs",
    str_detect(Month, "^2023")
  ) %>%
  mutate(
    LSOA = toupper(str_trim(LSOA)),
    County = toupper(County)
  )

# STEP 4: Aggregate crime rate by LSOA
drug_rates <- crime_2023 %>%
  group_by(LSOA, County, Population) %>%
  summarise(drug_crimes = n(), .groups = "drop") %>%
  mutate(drug_rate_per_10000 = (drug_crimes / Population) * 10000)

# STEP 5: Merge both datasets by LSOA and County
merged_df <- inner_join(broadband_clean, drug_rates, by = c("LSOA", "County"))

# STEP 6: Filter valid numeric entries
df_filtered <- merged_df %>%
  filter(is.finite(avg_speed), is.finite(drug_rate_per_10000))

# STEP 7: Correlation
cor_test <- cor.test(df_filtered$avg_speed, df_filtered$drug_rate_per_10000)
print(cor_test)

# STEP 8: Linear Model
model <- lm(drug_rate_per_10000 ~ avg_speed, data = df_filtered)
summary(model)

# STEP 9: Plot
ggplot(df_filtered, aes(x = avg_speed, y = drug_rate_per_10000, color = County)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE,fullrange=TRUE) +
  scale_x_continuous(name = "Average Download Speed (Mbps)", labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(name = "Drug Offense Rate per 10,000 People") +
  labs(
    title = "Average Download Speed vs Drug Offense Rate per 10,000 People (2023)",
    subtitle = paste0("Pearson r = ", round(cor_test$estimate, 2),
                      ", p-value = ", signif(cor_test$p.value, 2)),
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
