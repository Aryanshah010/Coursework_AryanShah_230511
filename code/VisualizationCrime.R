library(tidyverse)
library(ggplot2)
library(fmsb)
library(scales)

# Load cleaned datasets
crime <- read_csv("Cleaned Data/cleanCrime.csv")
towns <- read_csv("Cleaned Data/Towns.csv")

# Standardize postcode formatting
crime <- crime %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

towns <- towns %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

# Filter for Drug Offences from 2022 to 2024
crime_filtered <- crime %>%
  filter(CrimeType == "Drugs", Year %in% c(2022, 2023, 2024)) %>%
  left_join(
    towns %>%
      select(shortPostcode, District_Town = District, County_Town = County,
             Population2022, Population2023, Population2024),
    by = "shortPostcode"
  )

# Assign population by year
crime_rates <- crime_filtered %>%
  mutate(
    Population = case_when(
      Year == 2022 ~ Population2022,
      Year == 2023 ~ Population2023,
      Year == 2024 ~ Population2024
    )
  ) %>%
  drop_na(Population, District_Town) %>%
  group_by(District_Town, County_Town, Year) %>%
  summarise(
    DrugOffenseCount = n(),
    Population = first(Population),  # Use first valid population (should all be same per group)
    DrugOffenseRate = (DrugOffenseCount / Population) * 10000,
    .groups = "drop"
  )

# Define district groups
south_yorkshire <- c("SHEFFIELD", "BARNSLEY", "ROTHERHAM", "DONCASTER")
west_yorkshire  <- c("LEEDS", "BRADFORD", "WAKEFIELD", "KIRKLEES", "CALDERDALE")

# Classify CountyGroup
crime_rates <- crime_rates %>%
  mutate(
    CountyGroup = case_when(
      toupper(District_Town) %in% south_yorkshire ~ "South Yorkshire",
      toupper(District_Town) %in% west_yorkshire ~ "West Yorkshire",
      TRUE ~ "Other"
    )
  ) %>%
  filter(CountyGroup != "Other")

View(crime_rates)

# South Yorkshire
south_plot <- crime_rates %>%
  filter(County_Town == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = District_Town, y = DrugOffenseRate, fill = District_Town)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21) +
  labs(
    title = "Drug Offense Rate by District - South Yorkshire (2022–2024)",
    x = "District",
    y = "Drug Offense Rate (per 10,000 people)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")

# West Yorkshire
west_plot <- crime_rates %>%
  filter(County_Town == "WEST YORKSHIRE") %>%
  ggplot(aes(x = District_Town, y = DrugOffenseRate, fill = District_Town)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21) +
  labs(
    title = "Drug Offense Rate by District - West Yorkshire (2022–2024)",
    x = "District",
    y = "Drug Offense Rate (per 10,000 people)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")

# Display the plots
print(south_plot)
print(west_plot)

#Radar chart for south yorkshire(2023-06)
selected_year <- 2023
selected_month <- "2023-06"
selected_county <- "SOUTH YORKSHIRE"

crime_filtered <- crime %>%
  filter(CrimeType == "Vehicle crime", Year == selected_year, Month == selected_month, County == selected_county) %>%
  left_join(
    towns %>%
      select(shortPostcode, District_Town = District, County_Town = County,
             Population2023),
    by = "shortPostcode"
  )

crime_rates <- crime_filtered %>%
  mutate(
    Population = case_when(
      Year == 2023 ~ Population2023,
      TRUE ~ NA_real_
    )
  ) %>%
  drop_na(Population, District_Town) %>%
  group_by(District_Town) %>%
  summarise(
    VehicleCrimeCount = n(),
    Population = first(Population),
    VehicleCrimeRate = (VehicleCrimeCount / Population) * 10000,
    .groups = "drop"
  )

# Prepare data for radar chart
# Radar chart needs max, min rows on top
max_rate <- max(crime_rates$VehicleCrimeRate, na.rm = TRUE)
radar_data <- rbind(
  max = rep(max_rate, nrow(crime_rates)),
  min = rep(0, nrow(crime_rates)),
  crime_rates$VehicleCrimeRate
)

colnames(radar_data) <- crime_rates$District_Town
radar_data <- as.data.frame(radar_data)

# Plot radar chart
radarchart(radar_data,
           pcol = rgb(0.2, 0.5, 0.5, 0.9),
           pfcol = rgb(0.2, 0.5, 0.5, 0.5),
           plwd = 2,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, ceiling(max_rate), length.out = 5),
           cglwd = 0.8,
           vlcex = 0.8,
           title = paste("Vehicle Crime Rate per 10,000 by District\n",
                         selected_county, selected_month)
)

# Define South Yorkshire districts
south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "ROTHERHAM", "DONCASTER")

# Parameters
selected_year <- 2024
selected_month <- "2024-06"
selected_county <- "SOUTH YORKSHIRE"

# Filter for Robbery crimes
crime_filtered <- crime %>%
  filter(
    CrimeType == "Robbery",
    Year == selected_year,
    Month == selected_month,
    County == selected_county
  ) %>%
  left_join(
    towns %>%
      select(shortPostcode, District_Town = District, County_Town = County, Population2024),
    by = "shortPostcode"
  ) %>%
  filter(District_Town %in% south_yorkshire_districts)  # <- Filter only South Yorkshire districts

# Compute Robbery Rates
crime_rates <- crime_filtered %>%
  mutate(
    Population = case_when(
      Year == 2024 ~ Population2024,
      TRUE ~ NA_real_
    )
  ) %>%
  drop_na(Population, District_Town) %>%
  group_by(District_Town) %>%
  summarise(
    Population = first(Population),
    RobberyCount = n(),
    .groups = "drop"
  ) %>% 
  mutate(robberyRate = (RobberyCount / Population) * 10000)

# Pie Chart
ggplot(crime_rates, aes(x = "", y = robberyRate, fill = District_Town)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = paste0(round(robberyRate, 2), " /10k")),
            position = position_stack(vjust = 0.5), 
            color = "black", 
            size = 3) +
  coord_polar("y", start = 0) +
  labs(
    title = paste("Robbery Rate by District -", selected_county, "(", selected_month, ")"),
    fill = "District"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )



crime_rates <- crime_rates %>%
  mutate(
    CountyGroup = case_when(
      toupper(County_Town) %in% south_yorkshire ~ "SOUTH YORKSHIRE",
      toupper(County_Town) %in% west_yorkshire ~ "WEST YORKSHIRE",
      TRUE ~ "Other"
    )
  ) %>%
  filter(CountyGroup != "Other")

# Line chart for Drug Offense Rates (South and West Yorkshire, 2022–2024)
line_plot <- crime_rates %>%
  ggplot(aes(x = Year, y = DrugOffenseRate, color = District_Town, linetype = CountyGroup)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Drug Offense Rates per 10,000 People (South and West Yorkshire, 2022–2024)",
    x = "Year",
    y = "Drug Offense Rate (per 10,000 people)",
    color = "District",
    linetype = "County Group"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  scale_color_manual(values = c(
    "SHEFFIELD" = "#1b9e77", "BARNSLEY" = "#d95f02", "ROTHERHAM" = "#7570b3", "DONCASTER" = "#e7298a",  # South Yorkshire
    "LEEDS" = "#66a61e", "BRADFORD" = "#e6ab02", "WAKEFIELD" = "#a6761d", "KIRKLEES" = "#666666", "CALDERDALE" = "#ff7f00"  # West Yorkshire
  )) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Display the plot
print(line_plot)


