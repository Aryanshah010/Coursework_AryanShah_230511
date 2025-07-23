library(tidyverse)
library(ggplot2)
library(scales)

# Load your cleaned house price data
house_prices <- read_csv("Cleaned Data/cleanHousePrices.csv")

#Line Graphs for Average house prices from (2021-2024) for both counties in same diagram (take variables Price and District)
# Aggregate: average price per District and Year (one average per district per year)
avg_prices <- house_prices %>%
  group_by(Year, District) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Year = as.factor(Year))  # for discrete x-axis

# Define districts for each county (for reference and labeling)
south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
west_yorkshire_districts <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")

# Plot both counties in one diagram with lines by district
ggplot(avg_prices, aes(x = Year, y = avg_price, group = District, color = District)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Paired") +  # Distinct colors for visibility
  scale_y_continuous(labels = label_dollar(prefix = "£", big.mark = ",")) +  # GBP formatting
  labs(
    title = "Average House Prices by District in South and West Yorkshire (2021–2024)",
    x = "Year",
    y = "Average House Price (£)",
    color = "District"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank()  # Remove minor gridlines
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

# Filter out extreme outliers and keep only desired districts
house_prices_cleaned <- house_prices_corrected %>%
  filter(Price < 1000000) %>%
  filter(District %in% c(south_yorkshire_districts, west_yorkshire_districts))

# Ensure District is a factor with consistent order
house_prices_cleaned$District <- factor(
  house_prices_cleaned$District,
  levels = c(south_yorkshire_districts, west_yorkshire_districts)
)

# Plot
#Boxplot for average house prices for both counties in separate diagrams (take variables Price and District)
ggplot(house_prices_cleaned, aes(x = District, y = Price, fill = District)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, outlier.alpha = 0.8) +
  facet_wrap(~County, scales = "free_x") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Distribution of Average House Prices by District (Boxplot)",
    subtitle = "South Yorkshire and West Yorkshire shown separately",
    x = "District",
    y = "House Price (£)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    legend.position = "none"
  )
