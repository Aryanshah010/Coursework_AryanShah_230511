library(tidyverse)
library(lubridate)

# Step 1: Load 2021–2022 school performance data (i.e. 2022 scores)
ks4_2022 <- read_csv("Cleaned Data/cleaned_ks4final_2021-2022.csv")

west_yorkshire_towns <- ks4_2022 %>%
  filter(County == "West Yorkshire") %>%
  select(District) %>%
  distinct() %>%
  arrange(District)

# Step 2: Define South Yorkshire districts
#Boxplot for Average attainment 8 score 2022 – South Yorkshire (Variable District and Score)
south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
colnames(ks4_2022)

# Step 3: Filter for South Yorkshire schools only
sy_attainment <- ks4_2022 %>%
  filter(District %in% south_yorkshire_districts) %>%
  select(District, Attainment8) %>%
  drop_na(Attainment8)

# Step 4: Create boxplot
ggplot(sy_attainment, aes(x = District, y = Attainment8, fill = District)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.3, outlier.size = 1.5) +
  labs(
    title = "Distribution of Attainment 8 Scores by District (2022)",
    subtitle = "South Yorkshire",
    x = "District",
    y = "Average Attainment 8 Score"
  ) +
  scale_y_continuous(labels = label_number(suffix = " pts")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )


#Boxplot for Average attainment 8 score 2022 – West Yorkshire (Variable District and Score)

# Step 1: Define West Yorkshire districts
west_yorkshire_districts <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")

# Step 2: Filter and prepare the data
wy_attainment <- ks4_2022 %>%
  filter(District %in% west_yorkshire_districts) %>%
  select(District, Attainment8) %>%
  drop_na(Attainment8)

# Step 3: Create boxplot
ggplot(wy_attainment, aes(x = District, y = Attainment8, fill = District)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.3, outlier.size = 1.5) +
  labs(
    title = "Distribution of Attainment 8 Scores by District (2022)",
    subtitle = "West Yorkshire",
    x = "District",
    y = "Average Attainment 8 Score"
  ) +
  scale_y_continuous(labels = label_number(suffix = " pts")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )


#Line Graph to show the relationship between attainment 8 score 
#and years over multiple districts in South Yorkshire and west Yorkshire

# Step 1: Load the all-years dataset
all_years <- read_csv("Cleaned Data/cleaned_ks4final_all_years.csv")
colnames(all_years)

# Step 2: Define your districts
south_yorkshire <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
west_yorkshire  <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")
target_districts <- c(south_yorkshire, west_yorkshire)

# Step 3: Extract Exam Year (e.g., "2021-2022" -> 2022)
all_years <- all_years %>%
  mutate(ExamYear = as.integer(str_sub(Year, -4, -1)))  


# Step 4: Filter and calculate average Attainment 8 by year and district
attainment_trend <- all_years %>%
  filter(District %in% target_districts) %>%
  group_by(ExamYear, District) %>%
  summarise(
    avg_attainment8 = mean(Attainment8, na.rm = TRUE),
    .groups = "drop"
  )

# Step 5: Line plot showing score trend over years
ggplot(attainment_trend, aes(x = ExamYear, y = avg_attainment8, color = District)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = unique(attainment_trend$ExamYear)) +
  scale_y_continuous(labels = scales::label_number(suffix = " pts", accuracy = 0.1)) +
  labs(
    title = "Average Attainment 8 Scores by District (2022–2024)",
    subtitle = "South and West Yorkshire Districts",
    x = "Exam Year",
    y = "Average Attainment 8 Score",
    color = "District"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )




