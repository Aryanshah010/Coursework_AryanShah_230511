library(tidyverse)
library(scales)
library(ggplot2)

# STEP 1: Load datasets
house_prices <- read_csv("Cleaned Data/cleanHousePrices.csv")
school_scores <- read_csv("Cleaned Data/cleaned_ks4final_all_years.csv")

# STEP 2: Preprocess and clean
house_prices <- house_prices %>%
  mutate(Year = as.character(Year),  # Ensure Year is character to match
         County = toupper(County),
         LSOA = toupper(LSOA)) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

school_scores <- school_scores %>%
  mutate(
    Year = str_sub(Year, 1, 4),  # Extract "2021" from "2021-2022"
    LSOA = str_trim(toupper(LSOA)),
    County = str_trim(toupper(County))
  ) %>%
  select(LSOA, Year, County, District, Attainment8) %>%
  drop_na(Attainment8)


# STEP 3: Aggregate average house price per LSOA-Year
avg_house_price <- house_prices %>%
  group_by(LSOA, Year, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")

# STEP 4: Merge datasets on LSOA and Year
merged_df <- inner_join(avg_house_price, school_scores, by = c("LSOA", "Year", "County"))


# STEP 5: Filter for valid Attainment8 and Price
df_filtered <- merged_df %>%
  filter(!is.na(Attainment8), !is.na(avg_price), is.finite(avg_price), is.finite(Attainment8))

# STEP 6: Correlation
cor_test <- cor.test(df_filtered$avg_price, df_filtered$Attainment8)
print(cor_test)

# STEP 7: Linear Model
model <- lm(avg_price ~ Attainment8, data = df_filtered)
summary(model)

# STEP 8: Plot
ggplot(df_filtered, aes(x = Attainment8, y = avg_price, color = County)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, fullrange=TRUE) +
  scale_y_log10(labels = label_number(big.mark = ","), name = "Average House Price (£)") +
  scale_x_continuous(name = "Attainment 8 Score") +
  labs(
    title = "House Price vs Attainment 8 Score",
    subtitle = paste0("Pearson r = ", round(cor_test$estimate, 2),
                      ", p‑value = ", signif(cor_test$p.value, 2)),
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



