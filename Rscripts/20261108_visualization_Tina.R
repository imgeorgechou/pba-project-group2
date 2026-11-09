# ============================================================
# NYC Congestion Pricing & Citi Bike
# Data Visualization
# Period: 2023–2025
# ============================================================

library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)

# Create output folder
dir.create("figures", showWarnings = FALSE)

# ============================================================
# NYC Congestion Pricing & Citi Bike
# Data Visualization (2023–2025)
# ============================================================

library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)

# Load prepared datasets
daily_panel <- readRDS(
  "data_processed/station_daily_panel.rds"
)

peak_panel <- readRDS(
  "data_processed/peak_offpeak_panel.rds"
)

station_locations <- readRDS(
  "data_processed/station_locations.rds"
)

# Create folder for figures
dir.create("figures", showWarnings = FALSE)

# Check loaded datasets
nrow(daily_panel)
nrow(peak_panel)
table(daily_panel$group)

# ============================================================
# Figure 1: Monthly Average Daily Ridership by Group
# ============================================================

# Calculate average daily ridership per station for each month
monthly_ridership <- daily_panel[
  ,
  .(
    avg_daily_ridership = mean(ridership)
  ),
  by = .(year_month, group)
]

# Check results
head(monthly_ridership)
# Plot monthly average daily ridership
fig1 <- ggplot(
  monthly_ridership,
  aes(
    x = year_month,
    y = avg_daily_ridership,
    color = group
  )
) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = as.Date("2025-01-05"),
    linetype = "dashed",
    color = "red"
  ) +
  scale_color_manual(
    values = c(
      "Treatment" = "#8064A2",
      "Control" = "#20A39E"
    )
  ) +
  labs(
    title = "Monthly Average Daily Ridership by Group",
    subtitle = "Citi Bike Stations in CRZ vs. Queens (2023–2025)",
    x = "Month",
    y = "Average Daily Trips per Station",
    color = "Group",
    caption = "Dashed line: Congestion Pricing implementation (Jan 5, 2025)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# Display figure
print(fig1)
# Save Figure 1
ggsave(
  "figures/figure1_monthly_ridership.png",
  plot = fig1,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# Figure 2: Average Daily Ridership by Season
# ============================================================

# Calculate average daily ridership per station by season
seasonal_ridership <- daily_panel[
  ,
  .(
    avg_daily_ridership = mean(ridership)
  ),
  by = .(season, group)
]

# Set season order
seasonal_ridership[, season := factor(
  season,
  levels = c("Spring", "Summer", "Fall", "Winter")
)]

# Check results
seasonal_ridership[order(season, group)]
# Plot seasonal average daily ridership
fig2 <- ggplot(
  seasonal_ridership,
  aes(
    x = season,
    y = avg_daily_ridership,
    fill = group
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Treatment" = "#8064A2",
      "Control" = "#20A39E"
    )
  ) +
  labs(
    title = "Average Daily Ridership by Season",
    subtitle = "Citi Bike Stations in CRZ vs. Queens (2023–2025)",
    x = "Season",
    y = "Average Daily Trips per Station",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# Display figure
print(fig2)
# Save Figure 2
ggsave(
  "figures/figure2_seasonal_ridership.png",
  plot = fig2,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# Figure 3: Geographic Distribution of Citi Bike Stations
# ============================================================

# Keep only stations included in the analysis
analysis_ids <- unique(daily_panel$station_id)

map_stations <- station_locations[
  station_locations$station_id %in% analysis_ids,
]

# Check station counts
table(map_stations$group)
nrow(map_stations)

library(sf)
library(ggplot2)

# Load the official MTA CRZ boundary
crz_boundary <- st_read(
  "geographic_data/MTA_Central_Business_District_Geofence__Beginning_June_2024_20261108.geojson",
  quiet = TRUE
)

# Use the same coordinate system
crz_boundary <- st_transform(crz_boundary, 4326)
map_stations <- st_transform(map_stations, 4326)

# Plot station locations and CRZ boundary
fig3 <- ggplot() +
  geom_sf(
    data = crz_boundary,
    fill = "#8064A2",
    alpha = 0.12,
    color = "#8064A2",
    linewidth = 0.7
  ) +
  geom_sf(
    data = map_stations,
    aes(color = group),
    size = 1.2,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = c(
      "Treatment" = "#8064A2",
      "Control" = "#20A39E"
    )
  ) +
  coord_sf(
    xlim = c(-74.05, -73.85),
    ylim = c(40.67, 40.82),
    expand = FALSE
  ) +
  labs(
    title = "Geographic Distribution of Citi Bike Stations",
    subtitle = "Treatment (CRZ) vs. Control (Queens)",
    color = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.title = element_blank()
  )

# Display Figure 3
print(fig3)

# ============================================================
# Figure 4: Parallel Trends - CRZ vs. Queens
# ============================================================

# Calculate weekly average log ridership per station
weekly_ridership <- daily_panel[
  ,
  .(
    avg_log_ridership = mean(log_ridership)
  ),
  by = .(week, group)
]

# Check results
head(weekly_ridership)

# Plot weekly average log ridership
fig4 <- ggplot(
  weekly_ridership,
  aes(
    x = week,
    y = avg_log_ridership,
    color = group
  )
) +
  geom_line(linewidth = 0.9) +
  geom_vline(
    xintercept = as.Date("2025-01-05"),
    linetype = "dashed",
    color = "red",
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Treatment" = "#8064A2",
      "Control" = "#20A39E"
    )
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Parallel Trends: CRZ vs. Queens",
    subtitle = "Weekly Average Log Ridership (2023–2025)",
    x = "Week",
    y = "Average ln(Daily Trips + 1)",
    color = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display Figure 4
print(fig4)

# Save Figure 4
ggsave(
  "figures/figure4_parallel_trends.png",
  plot = fig4,
  width = 11,
  height = 6,
  dpi = 300
)

