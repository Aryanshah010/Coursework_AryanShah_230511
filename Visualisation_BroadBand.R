library(tidyverse)
library(ggplot2)
library(scales)

# Step 1: Load data
broadband <- read_csv("Cleaned Data/cleanbroadbandPerformance.csv")

# Step 2: Create boxplot 
#Boxplots for average download speed for both counties in separate chart (District vs Speed(Mbps))
ggplot(broadband, aes(x = District, y = `Average download speed (Mbit/s)`, fill = County)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, outlier.alpha = 0.3) +
  facet_wrap(~County, scales = "free_x") +
  scale_y_continuous(labels = label_number(suffix = " Mbps")) +
  labs(
    title = "Distribution of Average Broadband Download Speeds by District",
    subtitle = "Separate Boxplots for South Yorkshire and West Yorkshire",
    x = "District",
    y = "Download Speed (Mbps)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )


#Barchart for both counties (Two barcharts) download speeds (variable Town vs Speed)
# 1. Aggregate to average speed per town
avg_town_speed <- broadband %>%
  group_by(County, District) %>%
  summarise(
    avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Plot with facets (two bar charts in one figure)
ggplot(avg_town_speed, aes(x = reorder(District, avg_speed), y = avg_speed, fill = County)) +
  geom_col(width = 0.7) +
  facet_wrap(~County, scales = "free_y") +
  coord_flip() +
  scale_y_continuous(labels = label_number(suffix = " Mbps")) +
  labs(
    title    = "Average Broadband Download Speeds by Town",
    subtitle = "Separate Charts for South Yorkshire and West Yorkshire",
    x        = "Town",
    y        = "Average Download Speed (Mbps)",
    fill     = "County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position   = "none",
    axis.text.y       = element_text(size = 10),
    plot.title        = element_text(face = "bold"),
    strip.text        = element_text(face = "bold")
  )



