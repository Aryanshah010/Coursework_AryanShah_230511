library(tidyverse)

house<-read_csv("Cleaned Data/cleanHousePrices.csv")
broadband<-read_csv("Cleaned Data/cleanBroadbandPerformance.csv")

# Step 1: Average house price per shortPostcode
avg_price <- house %>%
  group_by(shortPostcode, District, County) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop")

avg_broadband<- broadband %>%
  group_by(shortPostcode, District,County) %>%
  summarise(AvgDownload = mean(AvgDownload, na.rm = TRUE), .groups = "drop")


# Step 2: Merge house price and broadband data
merged_df <- avg_price %>%
  inner_join(avg_broadband, by = "shortPostcode") %>%
  filter(!is.na(AvgDownload), !is.na(AvgPrice))


# Step 3: Correlation
cor_test <- cor.test(merged_df$AvgPrice, merged_df$AvgDownload)
print(cor_test)

# Step 4: Linear model
model <- lm(AvgPrice ~ AvgDownload, data = merged_df)
summary_model <- summary(model)
print(summary_model)

# Step 5: Scatter plot with regression line
ggplot(merged_df, aes(x = AvgDownload, y = AvgPrice, color = County.x)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, fullrange=TRUE) +
  scale_x_continuous(name = "Average Download Speed (Mbps)", labels = label_number(accuracy = 0.1)) +
  scale_y_log10(name = "Average House Price (£)", labels = label_number(big.mark = ",")) +
  labs(
    title = "House Price vs Download Speed — South & West Yorkshire",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

