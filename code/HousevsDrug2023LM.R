library(tidyverse)
library(scales)
library(broom)

# Load data
crime <- read_csv("Cleaned Data/cleanedYorkshireCrime_2022-05_to_2024-12.csv")
house <- read_csv("Cleaned Data/cleanHousePrices.csv")

# Filter drug crimes in 2023
drug_2023 <- crime %>%
  filter(`Crime type` == "Drugs", str_starts(Month, "2023-")) %>%
  group_by(LSOA, County, Population) %>%
  summarise(drug_crimes = n(), .groups = "drop") %>%
  mutate(drug_rate_per_10000 = (drug_crimes / Population) * 10000)

# Aggregate house prices (2023)
price_2023 <- house %>%
  filter(Year == 2023) %>%
  group_by(LSOA) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")


# Join
df <- inner_join(drug_2023, price_2023, by = "LSOA")

# Remove house price outliers (top 1%)
q99 <- quantile(df$avg_price, 0.99, na.rm = TRUE)
df_filtered <- df %>% filter(avg_price <= q99)

# Correlation
cor_test <- cor.test(df_filtered$avg_price, df_filtered$drug_rate_per_10000)
print(cor_test)

# Linear model
model <- lm(avg_price ~ drug_rate_per_10000, data = df_filtered)
print(tidy(model))
print(glance(model))

# Plot
ggplot(df_filtered, aes(x = drug_rate_per_10000, y = avg_price, color = County)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  scale_x_continuous("Drug Offense Rate per 10,000 People", labels = label_number(accuracy = 0.1)) +
  scale_y_continuous("Average House Price (£)", labels = label_number(big.mark = ",")) +
  labs(
    title = "House Price vs Drug Offense Rate (2023)",
    subtitle = paste0("Pearson r = ", round(cor_test$estimate, 2), ", p-value = ", signif(cor_test$p.value, 2)),
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
