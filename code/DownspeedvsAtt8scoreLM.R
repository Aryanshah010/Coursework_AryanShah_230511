# Load libraries
library(tidyverse)
library(scales)
library(ggplot2)

# STEP 1: Load data
broadband <- read_csv("Cleaned Data/cleanBroadbandPerformance.csv")
attainment <- read_csv("Cleaned Data/cleaned_ks4final_2022-2023.csv")

# STEP 2: Clean and prepare download speed (aggregate by LSOA)
broadband_clean <- broadband %>%
  mutate(
    LSOA = toupper(str_trim(LSOA)),
    County = toupper(County)
  ) %>%
  group_by(LSOA, County) %>%
  summarise(avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE), .groups = "drop")

# STEP 3: Filter Attainment 8 data for 2022–2023 academic year
attainment_clean <- attainment %>%
  mutate(
    LSOA = toupper(str_trim(LSOA)),
    County = toupper(str_trim(County))
  ) %>%
  select(LSOA, County, Attainment8) %>%
  drop_na(Attainment8)

# STEP 4: Merge datasets by LSOA and County
merged_df <- inner_join(broadband_clean, attainment_clean, by = c("LSOA", "County"))

# STEP 5: Filter valid numeric entries
df_filtered <- merged_df %>%
  filter(is.finite(avg_speed), is.finite(Attainment8))

# STEP 6: Correlation
cor_test <- cor.test(df_filtered$avg_speed, df_filtered$Attainment8)
print(cor_test)

# STEP 7: Linear Model
model <- lm(Attainment8 ~ avg_speed, data = df_filtered)
summary(model)

# STEP 8: Plot
ggplot(df_filtered, aes(x = avg_speed, y = Attainment8, color = County)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE,fullrange=TRUE) +
  scale_x_continuous(name = "Average Download Speed (Mbps)", labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(name = "Attainment 8 Score") +
  labs(
    title = "Download Speed vs Attainment 8 Score (2022–2023)",
    subtitle = paste0("Pearson r = ", round(cor_test$estimate, 2),
                      ", p‑value = ", signif(cor_test$p.value, 2)),
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
