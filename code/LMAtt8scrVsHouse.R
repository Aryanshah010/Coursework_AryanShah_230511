# Load libraries
library(tidyverse)


south_yorkshire_districts <- c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM")
west_yorkshire_districts  <- c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")

school<-read_csv("Cleaned Data/cleanSchool.csv")

school <- school %>%
  mutate(
    District = toupper(District),   # Standardize case for merging
    Year = year                     # Rename `year` to `Year` for consistency
  )


house_filtered <- house %>%
  filter(Year %in% 2022:2024) %>%
  select(Year, Price, shortPostcode, District, County)

school_filtered <- school %>%
  filter(Year %in% 2022:2024) %>%
  select(Year, attainment_8_score, shortPostcode, District, County)


combined <- inner_join(house_filtered, school_filtered,
                       by = c("shortPostcode", "District", "County", "Year"))


combined <- combined %>%
  mutate(CountyGroup = case_when(
    District %in% south_yorkshire_districts ~ "South Yorkshire",
    District %in% west_yorkshire_districts ~ "West Yorkshire",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(CountyGroup))  # Keep only selected counties

district_avg <- combined %>%
  group_by(CountyGroup, District, Year) %>%
  summarise(
    avg_attainment8 = mean(attainment_8_score, na.rm = TRUE),
    avg_price = mean(Price, na.rm = TRUE),
    .groups = "drop"
  )

correlation <- district_avg %>%
  group_by(CountyGroup) %>%
  summarise(correlation = cor(avg_attainment8, avg_price, use = "complete.obs"))

print(correlation)
cor_test <- cor.test(district_avg$avg_attainment8, district_avg$avg_price)
print(cor_test)

model <- lm(avg_price ~ avg_attainment8, data = district_avg)
summary_model <- summary(model)
print(summary_model)

ggplot(district_avg, aes(x = avg_attainment8, y = avg_price, color = CountyGroup)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "District-Level Average Attainment 8 Score vs House Price (2022–2024)",
    x = "Average Attainment 8 Score",
    y = "Average House Price (GBP)",
    color = "County"
  ) +
  theme_minimal()
