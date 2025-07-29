library(ggplot2)
library(tidyverse)

BroadbandPerformance<-read_csv("Cleaned Data/cleanBroadbandPerformance.csv")

broadband_by_town <- final_BroadbandPerformance %>%
  group_by(Town, County) %>%
  summarise(AvgDownload = mean(AvgDownload, na.rm = TRUE), .groups = "drop")

#Barchart
broadband_by_town %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = reorder(Town, AvgDownload), y = AvgDownload, fill = Town)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Average Download Speed per Town (South Yorkshire)",
    x = "Town",
    y = "Avg Download Speed (Mbps)"
  ) +
  theme_minimal()

broadband_by_town %>%
  filter(County == "WEST YORKSHIRE") %>%
  ggplot(aes(x = reorder(Town, AvgDownload), y = AvgDownload, fill = Town)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Average Download Speed per Town (West Yorkshire)",
    x = "Town",
    y = "Avg Download Speed (Mbps)"
  ) +
  theme_minimal()


#Boxplot
final_BroadbandPerformance %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = District, y = AvgDownload)) +
  geom_boxplot(fill = "#56B4E9") +
  labs(
    title = "Download Speed per District (South Yorkshire)",
    x = "District",
    y = "Avg Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

final_BroadbandPerformance %>%
  filter(County == "WEST YORKSHIRE") %>%
  ggplot(aes(x = District, y = AvgDownload)) +
  geom_boxplot(fill = "#D55E00") +
  labs(
    title = "Download Speed per District (West Yorkshire)",
    x = "District",
    y = "Avg Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
