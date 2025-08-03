library(tidyverse)

school<-read_csv("Cleaned Data/cleanSchool.csv")

# Define South Yorkshire districts
south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")

# Filter for 2022 and South Yorkshire
school_filtered_sy <- school %>%
  filter(year == 2022, toupper(District) %in% south_yorkshire_districts) %>%
  mutate(District = toupper(District))

# Plot: Boxplot of Attainment 8 score by District
ggplot(school_filtered_sy, aes(x = District, y = attainment_8_score, fill = District)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.8) +
  labs(
    title = "Attainment 8 Scores (2022) - South Yorkshire",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Define West Yorkshire districts
west_yorkshire_districts <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")

# Filter for 2022 and West Yorkshire districts
school_filtered_wy <- school %>%
  filter(year == 2022, toupper(District) %in% west_yorkshire_districts) %>%
  mutate(District = toupper(District))

# Plot: Boxplot of Attainment 8 score by District (West Yorkshire)
ggplot(school_filtered_wy, aes(x = District, y = attainment_8_score, fill = District)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.8) +
  labs(
    title = "Attainment 8 Scores (2022) – West Yorkshire",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

yorkshire_districts <- c(south_yorkshire_districts, west_yorkshire_districts)

# Filter and label counties
school_filtered <- school %>%
  filter(toupper(District) %in% yorkshire_districts, year %in% c(2022, 2023, 2024)) %>%
  mutate(
    District = toupper(District),
    County = case_when(
      District %in% south_yorkshire_districts ~ "South Yorkshire",
      District %in% west_yorkshire_districts ~ "West Yorkshire"
    )
  )

# Summarize Attainment 8 scores
school_summary <- school_filtered %>%
  group_by(County, District, year) %>%
  summarise(AvgAttainment8 = mean(attainment_8_score, na.rm = TRUE), .groups = "drop")

# Split data
south_school <- school_summary %>% filter(County == "South Yorkshire")
west_school <- school_summary %>% filter(County == "West Yorkshire")

# South Yorkshire Plot
p_south_school <- ggplot(south_school, aes(x = year, y = AvgAttainment8, color = District, group = District)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  labs(
    title = "Attainment 8 Trends in South Yorkshire (2022–2024)",
    x = "Year",
    y = "Average Attainment 8 Score",
    color = "District"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# West Yorkshire Plot
p_west_school <- ggplot(west_school, aes(x = year, y = AvgAttainment8, color = District, group = District)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  labs(
    title = "Attainment 8 Trends in West Yorkshire (2022–2024)",
    x = "Year",
    y = "Average Attainment 8 Score",
    color = "District"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Print both plots separately
p_south_school
p_west_school
