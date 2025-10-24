# Comprehensive Earthquake Data Analysis - July 2021
# USGS Earthquake Data

# Load required libraries
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# ===== DATA LOADING =====
cat("Loading earthquake data...\n")
earthquakes <- read_csv("all_month.csv")
cat("Total earthquakes loaded:", nrow(earthquakes), "\n\n")

# ===== DATA PREPARATION =====
earthquakes <- earthquakes %>%
  mutate(
    datetime = ymd_hms(time),
    date = as.Date(datetime),
    hour = hour(datetime),
    day_of_week = wday(datetime, label = TRUE),
    mag_category = case_when(
      mag < 2.0 ~ "Micro (< 2.0)",
      mag >= 2.0 & mag < 3.0 ~ "Minor (2.0-2.9)",
      mag >= 3.0 & mag < 4.0 ~ "Light (3.0-3.9)",
      mag >= 4.0 & mag < 5.0 ~ "Moderate (4.0-4.9)",
      mag >= 5.0 & mag < 6.0 ~ "Strong (5.0-5.9)",
      mag >= 6.0 & mag < 7.0 ~ "Major (6.0-6.9)",
      mag >= 7.0 ~ "Great (7.0+)"
    ),
    depth_category = case_when(
      depth < 70 ~ "Shallow (< 70 km)",
      depth >= 70 & depth < 300 ~ "Intermediate (70-300 km)",
      depth >= 300 ~ "Deep (>= 300 km)"
    )
  )

# ===== SUMMARY STATISTICS =====
cat("===== SUMMARY STATISTICS =====\n\n")
cat("Date Range:", as.character(min(earthquakes$date)), "to",
    as.character(max(earthquakes$date)), "\n\n")

cat("MAGNITUDE:\n")
cat("  Min:", round(min(earthquakes$mag, na.rm = TRUE), 2), "\n")
cat("  Max:", round(max(earthquakes$mag, na.rm = TRUE), 2), "\n")
cat("  Mean:", round(mean(earthquakes$mag, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(earthquakes$mag, na.rm = TRUE), 2), "\n")
cat("  Std Dev:", round(sd(earthquakes$mag, na.rm = TRUE), 2), "\n\n")

cat("DEPTH:\n")
cat("  Min:", round(min(earthquakes$depth, na.rm = TRUE), 2), "km\n")
cat("  Max:", round(max(earthquakes$depth, na.rm = TRUE), 2), "km\n")
cat("  Mean:", round(mean(earthquakes$depth, na.rm = TRUE), 2), "km\n")
cat("  Median:", round(median(earthquakes$depth, na.rm = TRUE), 2), "km\n\n")

# ===== MAGNITUDE DISTRIBUTION =====
cat("===== MAGNITUDE CATEGORIES =====\n")
mag_table <- earthquakes %>%
  count(mag_category) %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(n))
print(mag_table)
cat("\n")

# ===== SIGNIFICANT EARTHQUAKES =====
cat("===== SIGNIFICANT EARTHQUAKES (Magnitude >= 5.0) =====\n")
strong_quakes <- earthquakes %>%
  filter(mag >= 5.0) %>%
  arrange(desc(mag)) %>%
  select(datetime, mag, depth, place, latitude, longitude)
cat("Count:", nrow(strong_quakes), "\n\n")
print(strong_quakes, n = Inf)
cat("\n")

# ===== TOP REGIONS =====
cat("===== TOP 20 MOST ACTIVE REGIONS =====\n")
top_regions <- earthquakes %>%
  count(place, sort = TRUE) %>%
  head(20)
print(top_regions, n = 20)
cat("\n")

# ===== NETWORK STATISTICS =====
cat("===== SEISMIC NETWORKS =====\n")
network_stats <- earthquakes %>%
  count(net, name = "earthquakes") %>%
  arrange(desc(earthquakes))
print(network_stats, n = 20)
cat("\n")

# ===== DAILY STATISTICS =====
cat("===== DAILY EARTHQUAKE COUNTS =====\n")
daily_stats <- earthquakes %>%
  count(date) %>%
  arrange(desc(n))
cat("Most active day:", as.character(daily_stats$date[1]),
    "with", daily_stats$n[1], "earthquakes\n")
cat("Least active day:", as.character(daily_stats$date[nrow(daily_stats)]),
    "with", daily_stats$n[nrow(daily_stats)], "earthquakes\n")
cat("Average per day:", round(mean(daily_stats$n), 2), "\n\n")

# ===== VISUALIZATIONS =====
cat("Creating visualizations...\n\n")

# 1. Magnitude Distribution
p1 <- ggplot(earthquakes, aes(x = mag)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white") +
  labs(title = "Earthquake Magnitude Distribution - July 2021",
       subtitle = paste(nrow(earthquakes), "earthquakes"),
       x = "Magnitude",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("magnitude_distribution.png", p1, width = 10, height = 6, dpi = 300)

# 2. World Map
world <- ne_countries(returnclass = "sf", scale = "medium")

p2 <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70", size = 0.2) +
  geom_point(data = earthquakes,
             aes(x = longitude, y = latitude, size = mag, color = mag),
             alpha = 0.6) +
  scale_color_viridis_c(option = "plasma", name = "Magnitude") +
  scale_size_continuous(range = c(0.1, 5), name = "Magnitude") +
  labs(title = "Global Earthquake Distribution - July 2021",
       subtitle = paste(nrow(earthquakes), "earthquakes recorded worldwide")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        panel.background = element_rect(fill = "aliceblue"))

ggsave("world_earthquake_map.png", p2, width = 12, height = 7, dpi = 300)

# 3. Depth vs Magnitude
p3 <- ggplot(earthquakes, aes(x = depth, y = mag)) +
  geom_point(alpha = 0.4, color = "darkblue", size = 1) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(title = "Earthquake Depth vs Magnitude",
       x = "Depth (km)",
       y = "Magnitude") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("depth_vs_magnitude.png", p3, width = 10, height = 6, dpi = 300)

# 4. Daily Frequency
p4 <- ggplot(daily_stats, aes(x = date, y = n)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Daily Earthquake Frequency - July 2021",
       x = "Date",
       y = "Number of Earthquakes") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d")

ggsave("daily_frequency.png", p4, width = 10, height = 6, dpi = 300)

# 5. Magnitude Categories
p5 <- ggplot(mag_table, aes(x = reorder(mag_category, n), y = n)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(aes(label = paste0(n, " (", percentage, "%)")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Earthquake Frequency by Magnitude Category",
       x = "Magnitude Category",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))

ggsave("magnitude_categories.png", p5, width = 10, height = 6, dpi = 300)

# 6. Top 15 Regions
top_15_regions <- earthquakes %>%
  count(place, sort = TRUE) %>%
  head(15)

p6 <- ggplot(top_15_regions, aes(x = reorder(place, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 15 Most Active Earthquake Regions",
       x = "Location",
       y = "Number of Earthquakes") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("top_regions.png", p6, width = 10, height = 8, dpi = 300)

# 7. Hourly Pattern
hourly_counts <- earthquakes %>%
  count(hour)

p7 <- ggplot(hourly_counts, aes(x = hour, y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Earthquake Distribution by Hour of Day (UTC)",
       x = "Hour",
       y = "Number of Earthquakes") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  scale_x_continuous(breaks = 0:23)

ggsave("hourly_pattern.png", p7, width = 10, height = 6, dpi = 300)

# 8. Depth Distribution
p8 <- ggplot(earthquakes, aes(x = depth)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "white") +
  labs(title = "Distribution of Earthquake Depths",
       x = "Depth (km)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  scale_y_continuous(labels = comma)

ggsave("depth_distribution.png", p8, width = 10, height = 6, dpi = 300)

# 9. Strong Earthquakes Map
if(nrow(strong_quakes) > 0) {
  p9 <- ggplot() +
    geom_sf(data = world, fill = "gray95", color = "gray70", size = 0.2) +
    geom_point(data = strong_quakes,
               aes(x = longitude, y = latitude, size = mag, color = depth),
               alpha = 0.8) +
    scale_color_viridis_c(option = "magma", name = "Depth (km)") +
    scale_size_continuous(range = c(4, 12), name = "Magnitude") +
    labs(title = "Strong Earthquakes (Magnitude >= 5.0) - July 2021",
         subtitle = paste(nrow(strong_quakes), "significant events")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          panel.background = element_rect(fill = "aliceblue"))

  ggsave("strong_earthquakes_map.png", p9, width = 12, height = 7, dpi = 300)
}

# 10. Day of Week Pattern
dow_counts <- earthquakes %>%
  count(day_of_week)

p10 <- ggplot(dow_counts, aes(x = day_of_week, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Earthquake Distribution by Day of Week",
       x = "Day",
       y = "Number of Earthquakes") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("day_of_week_pattern.png", p10, width = 10, height = 6, dpi = 300)

cat("\n===== ANALYSIS COMPLETE =====\n")
cat("Generated visualizations:\n")
cat("  - magnitude_distribution.png\n")
cat("  - world_earthquake_map.png\n")
cat("  - depth_vs_magnitude.png\n")
cat("  - daily_frequency.png\n")
cat("  - magnitude_categories.png\n")
cat("  - top_regions.png\n")
cat("  - hourly_pattern.png\n")
cat("  - depth_distribution.png\n")
if(nrow(strong_quakes) > 0) cat("  - strong_earthquakes_map.png\n")
cat("  - day_of_week_pattern.png\n")
