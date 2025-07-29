library(tidyverse)
library(scales)


broadband <- read_csv("Cleaned Data/cleanBroadbandPerformance.csv") %>%
  mutate(District = toupper(District), County = toupper(County)) %>%
  group_by(District, County) %>%
  summarise(
    avg_download = mean(AvgDownload, na.rm = TRUE),
    .groups = "drop"
  )

# --- 2. Load and prepare school data for Attainment 8 (2023) ---
school <- read_csv("Cleaned Data/cleanSchool.csv") %>%
  mutate(
    District = toupper(District),
    County   = toupper(County),
    Year     = year
  ) %>%
  filter(Year == 2023) %>%
  group_by(District, County) %>%
  summarise(
    avg_attainment8 = mean(attainment_8_score, na.rm = TRUE),
    .groups = "drop"
  )

# --- 3. Define target districts ---
south_yorkshire <- c("SHEFFIELD", "BARNSLEY", "ROTHERHAM", "DONCASTER")
west_yorkshire  <- c("LEEDS", "BRADFORD", "WAKEFIELD", "KIRKLEES", "CALDERDALE")
yorkshire_districts <- c(south_yorkshire, west_yorkshire)

# --- 4. Filter to only those districts ---
broadband_clean <- broadband %>% filter(District %in% yorkshire_districts)
school_clean    <- school    %>% filter(District %in% yorkshire_districts)

# --- 5. Join datasets by District & County ---
bb_school <- inner_join(
  broadband_clean,
  school_clean,
  by = c("District", "County")
) %>%
  mutate(CountyGroup = case_when(
    District %in% south_yorkshire ~ "South Yorkshire",
    District %in% west_yorkshire  ~ "West Yorkshire"
  ))

# --- 6. Compute correlations per county ---
correlation <- bb_school %>%
  group_by(CountyGroup) %>%
  summarise(
    correlation = cor(avg_download, avg_attainment8, use = "complete.obs")
  )
print("📊 Pearson correlation by County:")
print(correlation)

# --- 7. Fit linear model with interaction ---
model <- lm(avg_attainment8 ~ avg_download * CountyGroup, data = bb_school)
print("📈 Linear model summary:")
summary(model)

# --- 8. Plotting ---
ggplot(bb_school, aes(x = avg_download, y = avg_attainment8, color = CountyGroup)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Average Download Speed vs Attainment 8 Score (2023)",
    x = "Average Download Speed (Mbps)",
    y = "Average Attainment 8 Score",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )
