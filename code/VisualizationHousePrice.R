library(tidyverse)
library(scales)

house<-read_csv("Cleaned Data/cleanHousePrices.csv")

# Define district groups
south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
west_yorkshire_districts  <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")
yorkshire_districts <- c(south_yorkshire_districts, west_yorkshire_districts)

# Filter and clean data
house_filtered <- house %>%
  filter(District %in% yorkshire_districts, Year %in% 2021:2024) %>%
  mutate(District = toupper(District))

# Calculate average price per District and Year
avg_price_by_year <- house_filtered %>%
  group_by(Year, District) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    County = case_when(
      District %in% south_yorkshire_districts ~ "South Yorkshire",
      District %in% west_yorkshire_districts ~ "West Yorkshire",
      TRUE ~ "Other"
    )
  )

# Plot: Line graph by District over Year
ggplot(avg_price_by_year, aes(x = Year, y = avg_price, color = District, linetype = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Average House Prices (2021–2024)",
    subtitle = "By District in South and West Yorkshire",
    x = "Year",
    y = "Average House Price (£)",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Filter for 2023 and relevant districts
house_2023 <- house %>%
  filter(District %in% yorkshire_districts, Year == 2023) %>%
  mutate(
    District = toupper(District),
    County = case_when(
      District %in% south_yorkshire_districts ~ "South Yorkshire",
      District %in% west_yorkshire_districts ~ "West Yorkshire"
    )
  )

# Compute average price per district
avg_price_2023 <- house_2023 %>%
  group_by(District, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")

# Bar chart
ggplot(avg_price_2023, aes(x = reorder(District, -avg_price), y = avg_price, fill = County)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(labels = label_number(prefix = "£", big.mark = ",")) +
  labs(
    title = "Average House Prices by District (2023)",
    x = "District",
    y = "Average House Price",
    fill = "County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Filter for Yorkshire districts
house_filtered <- house %>%
  filter(District %in% yorkshire_districts) %>%
  mutate(
    District = toupper(District),
    County = case_when(
      District %in% south_yorkshire_districts ~ "South Yorkshire",
      District %in% west_yorkshire_districts ~ "West Yorkshire"
    )
  )

# Create boxplots faceted by County
ggplot(house_filtered, aes(x = District, y = Price, fill = District)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.8) + 
  scale_y_log10(labels = scales::label_number(prefix = "£", big.mark = ",")) +
  labs(
    title = "Average House Prices by District",
    x = "District",
    y = "House Price (£, log scale)"
  ) +
  facet_wrap(~ County, scales = "free_x") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

