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

# Filter for relevant districts and years
school_filtered <- school %>%
  filter(toupper(District) %in% yorkshire_districts, year %in% c(2022, 2023, 2024)) %>%
  mutate(
    District = toupper(District),
    County = case_when(
      District %in% south_yorkshire_districts ~ "South Yorkshire",
      District %in% west_yorkshire_districts ~ "West Yorkshire"
    )
  )

# Group and summarise average Attainment 8 score by District and Year
school_summary <- school_filtered %>%
  group_by(County, District, year) %>%
  summarise(AvgAttainment8 = mean(attainment_8_score, na.rm = TRUE), .groups = "drop")

# Plot line graph
ggplot(school_summary, aes(x = year, y = AvgAttainment8, color = District, group = District)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~County) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  labs(
    title = "Attainment 8 Score Trends (2022–2024) in Yorkshire Districts",
    x = "Year",
    y = "Average Attainment 8 Score",
    color = "District"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
