library(tidyverse)
library(scales)
library(fmsb)
library(lubridate)


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


#Radar chart for Vehicle crime rate for any one of the two counties (for any specific month and year)
# Step 1: Filter for vehicle crimes in West Yorkshire, March 2023
vehicle_crime <- crime_data %>%
  filter(`Crime type` == "Vehicle crime",
         County == "WEST YORKSHIRE",
         Month == "2023-03")

# Step 2: Count vehicle crimes per district and compute rate per 10,000
vehicle_rates <- vehicle_crime %>%
  group_by(District, County, Population) %>%
  summarise(vehicle_crimes = n(), .groups = "drop") %>%
  mutate(vehicle_rate_per_10k = (vehicle_crimes / Population) * 10000)

# Step 3: Prepare data for radar chart format
radar_data <- vehicle_rates %>%
  select(District, vehicle_rate_per_10k) %>%
  column_to_rownames("District") %>%
  t() %>%
  as.data.frame()

# Add required top (max) and bottom (min) rows for fmsb radar chart
radar_data <- rbind(
  max = rep(max(radar_data), ncol(radar_data)),
  min = rep(0, ncol(radar_data)),
  radar_data
)

# Step 4: Plot radar chart
radarchart(radar_data,
           axistype = 1,
           title = "Vehicle Crime Rate per 10,000 (West Yorkshire, March 2023)",
           pcol = "red", pfcol = rgb(1, 0, 0, 0.3), plwd = 2,
           cglcol = "grey", cglty = 1, axislabcol = "black", 
           vlcex = 0.9)



#Pie chart for Robbery rate for any one of two counties (for any specific month and year)
# Filter for Robbery crimes in South Yorkshire, May 2024
robbery_data <- crime_data %>%
  filter(`Crime type` == "Robbery",
         County == "SOUTH YORKSHIRE",
         Month == "2024-05")

# Aggregate and calculate rate per 10,000
robbery_summary <- robbery_data %>%
  group_by(District, Population) %>%
  summarise(robbery_count = n(), .groups = "drop") %>%
  mutate(robbery_rate_per_10k = round((robbery_count / Population) * 10000, 2))

# Create  pie chart 
ggplot(robbery_summary, aes(x = "", y = robbery_rate_per_10k, fill = District)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(robbery_rate_per_10k, " /10k")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5, fontface = "bold") +
  labs(
    title = "Robbery Rate per 10,000 People by District",
    subtitle = "South Yorkshire — May 2024",
    fill = "District"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )


#Line chart for Drug offense rates per 10,000 people for both counties in same diagram for all years.
# Step 1: Filter for drug-related crimes only
drug_crime <- crime_data %>%
  filter(`Crime type` == "Drugs")

# Step 2: Aggregate by Month and County
drug_rate_by_month <- drug_crime %>%
  group_by(Month, County) %>%
  summarise(
    drug_count = n(),
    Population = sum(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    drug_rate_per_10k = (drug_count / Population) * 10000,
    Month = ym(Month)  # Convert to date format for plotting
  )

# Step 3: Plot line chart
ggplot(drug_rate_by_month, aes(x = Month, y = drug_rate_per_10k, color = County)) +
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


