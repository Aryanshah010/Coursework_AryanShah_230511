# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)

# Load your cleaned house price data
house_prices <- read_csv("Cleaned Data/cleanHousePrices.csv")

# Aggregate: average price per District, County, and Year
avg_prices <- house_prices %>%
  group_by(Year, District, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Year = as.factor(Year))  # for discrete x-axis

# Plot both counties in one diagram
#Line Graphs for Average house prices from (2021-2024) for both counties in same diagram (Price and District)
ggplot(avg_prices, aes(x = Year, y = avg_price, group = District, color = District)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Average House Prices by District (2021–2024)",
    subtitle = "South Yorkshire and West Yorkshire — Combined View",
    x = "Year",
    y = "Average House Price (£)",
    color = "District"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Step 1: Filter for 2023 and compute average price per district
avg_prices_2023 <- house_prices %>%
  filter(Year == 2023) %>%
  group_by(District, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")

# Step 2: Plot bar chart
#Bar chart Average house prices of 2023 for both counties (take variables, Prices and Districts
ggplot(avg_prices_2023, aes(x = reorder(District, avg_price), y = avg_price, fill = County)) +
  geom_col(width = 0.7) +
   # Flip for better readability
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Average House Prices by District in 2023",
    subtitle = "South Yorkshire and West Yorkshire",
    x = "District",
    y = "Average House Price (£)",
    fill = "County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# Step 1: Filter out extreme outliers (e.g., above £1,000,000)
house_prices_cleaned <- house_prices %>%
  filter( Price < 1000000) 


# Step 2: Make proper boxplot with separate diagrams
ggplot(house_prices_cleaned, aes(x = District, y = Price, fill = County)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, outlier.alpha = 1) +
  facet_wrap(~County, scales = "free_x") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Boxplot of House Prices by District",
    subtitle = "South Yorkshire and West Yorkshire",
    x = "District",
    y = "House Price (£)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

