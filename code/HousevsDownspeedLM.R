library(tidyverse)
library(scales)
library(ggplot2)


# 1. Load and clean House Price data
house <- read_csv("Cleaned Data/cleanHousePrices.csv") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(LSOA, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")

# 2. Load and clean Broadband Speed data
broadband <- read_csv("Cleaned Data/cleanBroadbandPerformance.csv") %>%
  mutate(County = toupper(County)) %>%  # Convert County to uppercase
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(LSOA, County) %>%
  summarise(avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE), .groups = "drop")


# 3. Merge datasets using LSOA
merged_df <- inner_join(house, broadband, by = c("LSOA", "County"))

# 4. Correlation test
cor_test <- cor.test(merged_df$avg_price, merged_df$avg_speed)
print(cor_test)

# 5. Linear Model
model <- lm(avg_price ~ avg_speed, data = merged_df)
summary(model)

# 6. Plot
ggplot(merged_df, aes(x = avg_speed, y = avg_price, color = County)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  scale_x_continuous(name = "Average Download Speed (Mbps)", labels = label_number(accuracy = 0.1)) +
  scale_y_log10(name = "Average House Price (£)", labels = label_number(big.mark = ",")) +
  labs(
    title = "House Price vs Download Speed — South & West Yorkshire",
    subtitle = paste0("Pearson r = ", round(cor_test$estimate, 2), 
                      ", p-value = ", signif(cor_test$p.value, 3)),
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
