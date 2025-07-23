library(tidyverse)
library(scales)
library(ggplot2)
library(dplyr)
library(janitor)
library(fmsb)


# Load your cleaned and trimmed crime data
crime_data <- read_csv("Cleaned Data/cleanedYorkshireCrime_2022-05_to_2024-12.csv")


#Boxplot for Drug offense rate in the district of both counties (two separate diagrams).
#Take variables (drug offense rate vs District)
# Step 1: Filter for drug-related offenses only
drug_crimes <- crime_data %>%
  filter(`Crime type` == "Drugs")

# Step 2: Group by Month, District, and County to count drug offenses
drug_counts <- drug_crimes %>%
  group_by(Month, District, County, Population) %>%
  summarise(num_drug_crimes = n(), .groups = "drop") %>%
  # Step 3: Calculate rate per 10,000 people
  mutate(drug_rate_per_10k = (num_drug_crimes / Population) * 10000)

# Step 4: Boxplot — one per County, with Districts on x-axis
ggplot(drug_counts, aes(x = District, y = drug_rate_per_10k, fill = County)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.alpha = 0.3) +
  facet_wrap(~County, scales = "free_x") +
  scale_y_continuous(labels = label_number(suffix = " /10k")) +
  labs(
    title = "Drug Offense Rate per 10,000 People by District",
    subtitle = "Separate Boxplots for South Yorkshire and West Yorkshire (2022–2024)",
    x = "District",
    y = "Drug Offense Rate per 10,000 People",
    fill = "County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )


# Load and clean data
crime_data <- read_csv("Cleaned Data/cleanedYorkshireCrime_2022-05_to_2024-12.csv") %>%
  clean_names()

# Step 1: Filter for Vehicle Crime in West Yorkshire, March 2023
vehicle_crime <- crime_data %>%
  filter(
    crime_type == "Vehicle crime",
    county == "WEST YORKSHIRE",
    month == "2023-03"
  )

# Step 2: Group and calculate rate per 10k
vehicle_rates <- vehicle_crime %>%
  group_by(district, population) %>%
  summarise(vehicle_crimes = n(), .groups = "drop") %>%
  mutate(vehicle_rate_per_10k = (vehicle_crimes / population) * 10000)

# Step 3: Pivot to wide format for radar chart
vehicle_wide <- vehicle_rates %>%
  select(district, vehicle_rate_per_10k) %>%
  pivot_wider(
    names_from = district,
    values_from = vehicle_rate_per_10k,
    values_fn = mean
  )

View(vehicle_wide)
# Step 4: Add max and min rows for scaling
vehicle_matrix <- rbind(
  rep(25, ncol(vehicle_wide)),  # max values for scaling
  rep(0, ncol(vehicle_wide)),   # min values for scaling
  vehicle_wide
)

# Step 5: Draw radar chart
radarchart(
  vehicle_matrix,
  axistype = 1,
  pcol = "#2E8B57",
  pfcol = scales::alpha("#2E8B57", 0.5),
  plwd = 3,
  cglcol = "grey70",
  cglty = 1,
  axislabcol = "grey30",
  vlcex = 1.1
)

# Add title
title(main = "Vehicle Crime Rate per 10,000 People — West Yorkshire (Mar 2023)",
      sub = paste0("Max: ", round(max(vehicle_rates$vehicle_rate_per_10k), 1),
                   " | Min: ", round(min(vehicle_rates$vehicle_rate_per_10k), 1),
                   "\nData Source: cleanedYorkshireCrime_2022-05_to_2024-12.csv"))



#Pie chart for Robbery rate for any one of two counties (for any specific month and year
# Load and clean data
crime_data <- read_csv("Cleaned Data/cleanedYorkshireCrime_2022-05_to_2024-12.csv") %>%
  clean_names()

# Step 1: Filter for Robbery in South Yorkshire, May 2024
robbery_data <- crime_data %>%
  filter(
    crime_type == "Robbery",
    county == "SOUTH YORKSHIRE",
    month == "2024-05"
  )

# Step 2 & 3: Aggregate robberies and calculate rate per district
robbery_rate <- robbery_data %>%
  group_by(district) %>%
  summarise(
    population = first(population),  # Assuming population is consistent per district
    robbery_count = n(),             # Total number of robberies
    .groups = "drop"
  ) %>%
  mutate(robbery_rate_per_10k = (robbery_count / population) * 10000)

# Step 4: Calculate percentage of total robberies
robbery_rate <- robbery_rate %>%
  mutate(percentage = (robbery_count / sum(robbery_count)) * 100)

# Step 5: Create Pie Chart
ggplot(robbery_rate, aes(x = "", y = percentage, fill = district)) +
  geom_col(width = 1, color = "white") +           # Create bars for pie slices
  coord_polar("y") +                               # Convert to pie chart
  geom_text(aes(label = if_else(percentage >= 1,   # Show labels only if percentage ≥ 1%
                                paste0(round(robbery_rate_per_10k, 2), " /10k"), 
                                "")),
            position = position_stack(vjust = 0.5), 
            color = "black", 
            size = 3) +
  scale_fill_brewer(palette = "Set2") +            # Color scheme
  labs(
    title = "Robbery rate by District",
    subtitle = "SOUTH YORKSHIRE — May 2024",
    fill = "District",
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 14) +                     # Minimal theme for pie chart
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )



#Line chart for Drug offense rates per 10,000 people for both counties in same diagram for all years.
# Step 1: Filter for drug-related crimes only
drug_crime <- crime_data %>%
  filter(`crime_type` == "Drugs")

# Step 2: Aggregate by Month and County
drug_rate_by_month <- drug_crime %>%
  group_by(month, county) %>%
  summarise(
    drug_count = n(),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    drug_rate_per_10k = (drug_count / population) * 10000,
    month = ym(month)  # Convert to date format for plotting
  )

# Step 3: Plot line chart
ggplot(drug_rate_by_month, aes(x = month, y = drug_rate_per_10k, color = county)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous() +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "3 months") +
  labs(
    title = "Monthly Drug Offense Rate per 10,000 People",
    subtitle = "South and West Yorkshire (2022–2024)",
    x = "Month",
    y = "Drug Offense Rate (/10,000 people)",
    color = "County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )   


