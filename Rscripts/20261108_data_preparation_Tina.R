# Citi Bike Project
# Data Preparation (2023–2025)

library(data.table)

# Step 1: Find all cleaned CSV files
clean_files <- list.files(
  path = "data_processed",
  pattern = "^202[345][0-9]{2}_citibike_clean\\.csv$",
  full.names = TRUE
)

# Check number of files
length(clean_files)

# Check filenames
basename(sort(clean_files))

# Check whether all 36 months are present
expected_files <- paste0(
  rep(2023:2025, each = 12),
  sprintf("%02d", rep(1:12, times = 3)),
  "_citibike_clean.csv"
)

missing_files <- setdiff(
  expected_files,
  basename(clean_files)
)

missing_files

if (length(missing_files) == 0) {
  print("All 36 monthly files are present!")
} else {
  print("Some monthly files are missing.")
}

# Step 2: Extract station information

station_list <- rbindlist(
  lapply(clean_files, function(file) {
    
    # Read only station-related columns
    trips <- fread(
      file,
      select = c(
        "start_station_id",
        "start_station_name",
        "start_lat",
        "start_lng"
      ),
      colClasses = list(
        character = "start_station_id"
      )
    )
    
    # Remove missing station coordinates
    trips <- trips[
      !is.na(start_lat) &
        !is.na(start_lng)
    ]
    
    # Rename columns
    setnames(
      trips,
      c(
        "start_station_id",
        "start_station_name",
        "start_lat",
        "start_lng"
      ),
      c(
        "station_id",
        "station_name",
        "lat",
        "lng"
      )
    )
    
    # Keep one record per station per file
    unique(trips, by = "station_id")
  }),
  use.names = TRUE
)

# Check number of station records
nrow(station_list)

# Check number of unique stations
uniqueN(station_list$station_id)


# Step 3: Create unique station locations

station_locations <- station_list[
  ,
  .(
    station_name = first(station_name),
    lat = median(lat, na.rm = TRUE),
    lng = median(lng, na.rm = TRUE)
  ),
  by = station_id
]

# Check number of unique stations
nrow(station_locations)

# Check missing coordinates
sum(
  !is.finite(station_locations$lat) |
    !is.finite(station_locations$lng)
)

# Check coordinate ranges
summary(station_locations[, .(lat, lng)])

# Step 4: Check abnormal station coordinates

abnormal_stations <- station_locations[
  lat < 40.5 | lat > 41 |
    lng < -74.3 | lng > -73.7 |
    !is.finite(lat) |
    !is.finite(lng)
]

# View abnormal stations
abnormal_stations

# Count abnormal stations
nrow(abnormal_stations)

# Step 5: Exclude LA Metro Demo Stations

station_locations_clean <- station_locations[
  !station_id %in% abnormal_stations$station_id
]

# Check number of stations
nrow(station_locations_clean)

# Check coordinate ranges
summary(station_locations_clean[, .(lat, lng)])

# Step 6: Check station location stability

station_movement <- station_list[
  station_id %in% station_locations_clean$station_id,
  .(
    lat_range = max(lat, na.rm = TRUE) -
      min(lat, na.rm = TRUE),
    lng_range = max(lng, na.rm = TRUE) -
      min(lng, na.rm = TRUE)
  ),
  by = station_id
]

# Check coordinate variation
summary(station_movement[, .(lat_range, lng_range)])

# Find stations with noticeable coordinate changes
station_movement[
  lat_range > 0.001 | lng_range > 0.001
]

# Step 7: Calculate station location changes in meters

library(sf)

# Convert station coordinates to spatial data
station_sf <- st_as_sf(
  station_list[
    station_id %in% station_locations_clean$station_id
  ],
  coords = c("lng", "lat"),
  crs = 4326
)

# Convert to a projected coordinate system (meters)
station_sf <- st_transform(station_sf, 2263)

# Calculate maximum pairwise distance within each station
station_distance <- rbindlist(
  lapply(
    split(station_sf, station_sf$station_id),
    function(x) {
      
      coords <- st_coordinates(x)
      
      max_distance <- if (nrow(coords) < 2) {
        0
      } else {
        max(as.numeric(dist(coords)))
      }
      
      data.table(
        station_id = x$station_id[1],
        max_distance_m = max_distance * 0.3048006096
      )
    }
  )
)

# Summary
summary(station_distance$max_distance_m)

# Stations with location changes over 100 meters
moved_stations <- station_distance[
  max_distance_m > 100
][order(-max_distance_m)]

moved_stations

# Number of stations
nrow(moved_stations)

# Step 8: Examine stations with location changes

moved_station_history <- rbindlist(
  lapply(clean_files, function(file) {
    
    month_id <- substr(basename(file), 1, 6)
    
    trips <- fread(
      file,
      select = c(
        "start_station_id",
        "start_station_name",
        "start_lat",
        "start_lng"
      ),
      colClasses = list(
        character = "start_station_id"
      )
    )
    
    # Keep only stations flagged for review
    trips <- trips[
      start_station_id %in% moved_stations$station_id &
        is.finite(start_lat) &
        is.finite(start_lng)
    ]
    
    # Summarize coordinates by station and month
    result <- trips[
      ,
      .(
        station_name = first(start_station_name),
        lat = median(start_lat),
        lng = median(start_lng),
        trip_count = .N
      ),
      by = start_station_id
    ]
    
    result[, month := month_id]
    
    setnames(
      result,
      "start_station_id",
      "station_id"
    )
    
    return(result)
  }),
  use.names = TRUE
)

# Check HB609 location history
moved_station_history[
  station_id == "HB609"
][order(month)]

# Step 9: Review location history of other stations

# Exclude HB609 from this review
other_moved_stations <- moved_station_history[
  station_id != "HB609"
]

# Check number of stations
uniqueN(other_moved_stations$station_id)

# Calculate monthly coordinate variation
station_location_check <- other_moved_stations[
  ,
  .(
    months_observed = uniqueN(month),
    lat_range = max(lat) - min(lat),
    lng_range = max(lng) - min(lng)
  ),
  by = station_id
]

# View stations with coordinate variation
station_location_check[
  lat_range > 0.001 | lng_range > 0.001
][order(-lat_range)]

# Step 10: Extract end station information

end_station_list <- rbindlist(
  lapply(clean_files, function(file) {
    
    trips <- fread(
      file,
      select = c(
        "end_station_id",
        "end_station_name",
        "end_lat",
        "end_lng"
      ),
      colClasses = list(
        character = "end_station_id"
      )
    )
    
    # Remove missing coordinates
    trips <- trips[
      is.finite(end_lat) &
        is.finite(end_lng)
    ]
    
    # Rename columns
    setnames(
      trips,
      c(
        "end_station_id",
        "end_station_name",
        "end_lat",
        "end_lng"
      ),
      c(
        "station_id",
        "station_name",
        "lat",
        "lng"
      )
    )
    
    # Keep one record per station per file
    unique(trips, by = "station_id")
  }),
  use.names = TRUE
)

# Check number of end station records
nrow(end_station_list)

# Check unique end stations
uniqueN(end_station_list$station_id)

# Step 11: Combine start and end station information

# Combine station records
all_stations <- rbindlist(
  list(station_list, end_station_list),
  use.names = TRUE
)

# Check total station records
nrow(all_stations)

# Check unique station IDs
uniqueN(all_stations$station_id)

# Create one representative location per station
all_station_locations <- all_stations[
  is.finite(lat) & is.finite(lng),
  .(
    station_name = first(station_name),
    lat = median(lat),
    lng = median(lng)
  ),
  by = station_id
]

# Check station count
nrow(all_station_locations)

# Check coordinate ranges
summary(all_station_locations[, .(lat, lng)])

# Step 12: Identify invalid station coordinates

invalid_stations <- all_station_locations[
  lat < 40.5 | lat > 41 |
    lng < -74.3 | lng > -73.7
]

# View invalid stations
invalid_stations

# Count invalid stations
nrow(invalid_stations)

# Step 13: Remove invalid station locations

station_locations_final <- all_station_locations[
  !station_id %in% invalid_stations$station_id
]

# Check remaining stations
nrow(station_locations_final)

# Check coordinate ranges
summary(station_locations_final[, .(lat, lng)])

# Confirm invalid coordinates are removed
sum(
  station_locations_final$lat < 40.5 |
    station_locations_final$lat > 41 |
    station_locations_final$lng < -74.3 |
    station_locations_final$lng > -73.7
)

# Step 14: Convert stations to spatial data

library(sf)

station_sf <- st_as_sf(
  station_locations_final,
  coords = c("lng", "lat"),
  crs = 4326,
  remove = FALSE
)

# Check coordinate system
st_crs(station_sf)

# Check number of stations
nrow(station_sf)

# Step 15: Load NYC Borough Boundaries

library(sf)

boroughs <- st_read(
  "https://data.cityofnewyork.us/resource/gthc-hcne.geojson",
  quiet = TRUE
)

# Convert to the same coordinate system
boroughs <- st_transform(boroughs, 4326)

# Check borough names
boroughs$boroname

# Check number of boroughs
nrow(boroughs)

# Step 16: Identify Queens Stations

# Extract Queens boundary
queens <- boroughs[
  boroughs$boroname == "Queens",
]

# Check which stations are in Queens
station_sf$in_queens <- lengths(
  st_intersects(station_sf, queens)
) > 0

# Count Queens stations
table(station_sf$in_queens)

# Create Queens station list
queens_stations <- station_sf[
  station_sf$in_queens,
]

# Check number of Queens stations
nrow(queens_stations)

# Step 17: Identify Manhattan Stations

# Extract Manhattan boundary
manhattan <- boroughs[
  boroughs$boroname == "Manhattan",
]

# Check which stations are in Manhattan
station_sf$in_manhattan <- lengths(
  st_intersects(station_sf, manhattan)
) > 0

# Count Manhattan stations
table(station_sf$in_manhattan)

# Create Manhattan station list
manhattan_stations <- station_sf[
  station_sf$in_manhattan,
]

# Check number of Manhattan stations
nrow(manhattan_stations)

# Step 18: Check Manhattan station coordinates

# Coordinate ranges
summary(
  manhattan_stations[, c("lat", "lng")]
)

# Check station names
head(
  manhattan_stations[
    , c("station_id", "station_name", "lat", "lng")
  ],
  10
)

# Step 19: Inspect stations near 60th Street

stations_60 <- manhattan_stations[
  grepl(
    "\\b60 St\\b|\\b60th St\\b",
    manhattan_stations$station_name,
    ignore.case = TRUE
  ),
]

# View stations near 60th Street
stations_60[, c("station_id", "station_name", "lat", "lng")]

# Step 20: Load official CRZ boundary

crz <- st_read(
  "geographic_data/MTA_Central_Business_District_Geofence__Beginning_June_2024_20261108.geojson"
)

# Convert to WGS84
crz <- st_transform(crz, 4326)

# Check coordinate system
st_crs(crz)

# Check geometry
st_geometry_type(crz)

# Plot CRZ boundary
plot(st_geometry(crz))

# Step 21: Identify CRZ stations

# Check whether each station is inside the CRZ
station_sf$in_crz <- lengths(
  st_intersects(station_sf, crz)
) > 0

# Count stations inside and outside the CRZ
table(station_sf$in_crz)

# Create treatment group
treatment_stations <- station_sf[
  station_sf$in_crz,
]

# Check treatment group size
nrow(treatment_stations)

# Step 22: Check treatment and control groups

# Check overlap between CRZ and Queens
table(
  in_crz = station_sf$in_crz,
  in_queens = station_sf$in_queens
)

# Step 23: Assign treatment and control groups

station_sf$group <- ifelse(
  station_sf$in_crz,
  "Treatment",
  ifelse(
    station_sf$in_queens,
    "Control",
    "Other"
  )
)

# Check group sizes
table(station_sf$group)

# Step 25: Check station movement by group

# Get stations flagged for movement
moved_ids <- station_movement[
  lat_range > 0.001 | lng_range > 0.001,
  station_id
]

# Find moved stations in treatment and control groups
moved_stations <- station_sf[
  station_sf$station_id %in% moved_ids &
    station_sf$group %in% c("Treatment", "Control"),
]

# Check moved stations by group
table(moved_stations$group)

# View station details
sf::st_drop_geometry(moved_stations)[
  , c("station_id", "station_name", "group")
]

# Step 26: Check movement distances

# View movement statistics for flagged stations
station_movement[
  station_id %in% moved_stations$station_id
]

# Step 27: Calculate maximum station movement distance

library(sf)
library(data.table)

# Select location records for the 7 flagged stations
movement_points <- station_list[
  station_id %in% moved_stations$station_id &
    is.finite(lat) &
    is.finite(lng)
]

# Convert coordinates to a projected CRS (meters)
movement_sf <- st_as_sf(
  movement_points,
  coords = c("lng", "lat"),
  crs = 4326
)

movement_sf <- st_transform(movement_sf, 32618)

# Calculate maximum distance between observed locations
movement_distance <- rbindlist(
  lapply(
    split(movement_sf, movement_sf$station_id),
    function(x) {
      distances <- st_distance(x)
      
      data.table(
        station_id = x$station_id[1],
        max_distance_m = round(max(distances), 1)
      )
    }
  )
)

# View results
movement_distance[order(-max_distance_m)]

# Step 28: Check monthly station locations

# Select monthly records for flagged stations
movement_history_7 <- moved_station_history[
  station_id %in% moved_stations$station_id
]

# Sort by station and month
setorder(movement_history_7, station_id, month)

# View monthly locations
movement_history_7

# Step 29: Summarize station location changes

# Create a location identifier
movement_history_7[, location := paste(
  round(lat, 4),
  round(lng, 4),
  sep = ", "
)]

# Identify consecutive location periods
movement_history_7[, period := rleid(location), by = station_id]

# Summarize each location period
location_periods <- movement_history_7[
  ,
  .(
    start_month = first(month),
    end_month = last(month),
    months = .N,
    total_trips = sum(trip_count),
    lat = median(lat),
    lng = median(lng)
  ),
  by = .(station_id, station_name, period)
]

# View results
print(location_periods, nrows = Inf)

# Step 30: Check monthly station group assignments

# Convert monthly station locations to sf
monthly_sf <- st_as_sf(
  movement_history_7,
  coords = c("lng", "lat"),
  crs = 4326,
  remove = FALSE
)

# Check whether monthly locations fall inside CRZ
monthly_sf$in_crz <- lengths(
  st_intersects(monthly_sf, crz)
) > 0

# Check whether monthly locations fall inside Queens
monthly_sf$in_queens <- lengths(
  st_intersects(monthly_sf, queens)
) > 0

# Assign groups based on monthly locations
monthly_sf$monthly_group <- ifelse(
  monthly_sf$in_crz,
  "Treatment",
  ifelse(monthly_sf$in_queens, "Control", "Other")
)

# Compare monthly groups for each station
table(
  station_id = monthly_sf$station_id,
  monthly_group = monthly_sf$monthly_group
)

# Step 31: Inspect inconsistent station locations

# Find months classified outside the expected group
monthly_sf[
  monthly_sf$station_id == "5114.08" &
    monthly_sf$monthly_group == "Other",
  c("station_id", "station_name", "month",
    "lat", "lng", "trip_count", "monthly_group")
]
# Step 32: Compare early monthly locations

movement_history_7[
  station_id == "5114.08" &
    month %in% c("202304", "202305"),
  .(station_id, station_name, month, lat, lng, trip_count)
]

# Step 33: Inspect April 2023 station coordinates

april_station <- fread(
  "data_processed/202304_citibike_clean.csv",
  select = c(
    "start_station_id",
    "start_station_name",
    "start_lat",
    "start_lng"
  ),
  colClasses = list(character = "start_station_id")
)

# Filter station 5114.08
april_station <- april_station[
  start_station_id == "5114.08"
]

# Count trips by recorded coordinates
april_station[
  ,
  .N,
  by = .(start_lat, start_lng)
][order(-N)]

# Step 34: Create analysis station list

library(data.table)

# Keep treatment and control stations
analysis_stations <- as.data.table(
  sf::st_drop_geometry(station_sf)
)[
  group %in% c("Treatment", "Control"),
  .(station_id, station_name, group)
]

# Check station counts
table(analysis_stations$group)

# Check duplicate station IDs
anyDuplicated(analysis_stations$station_id)

# Check total number of stations
nrow(analysis_stations)

# Step 35: Check station availability by month

library(data.table)

# Get all cleaned monthly files
clean_files <- list.files(
  "data_processed",
  pattern = "^202[345][0-9]{2}_citibike_clean\\.csv$",
  full.names = TRUE
)

# Extract station IDs and month from each file
station_months <- rbindlist(
  lapply(clean_files, function(file) {
    
    trips <- fread(
      file,
      select = c("start_station_id", "end_station_id"),
      colClasses = list(
        character = c("start_station_id", "end_station_id")
      )
    )
    
    month <- substr(basename(file), 1, 6)
    
    ids <- unique(c(
      trips$start_station_id,
      trips$end_station_id
    ))
    
    data.table(
      station_id = ids,
      month = month
    )
  })
)

# Keep treatment and control stations
station_months <- station_months[
  station_id %in% analysis_stations$station_id
]

# Count observed months for each station
station_availability <- station_months[
  ,
  .(months_observed = uniqueN(month)),
  by = station_id
]

# Add group information
station_availability <- merge(
  analysis_stations[, .(station_id, group)],
  station_availability,
  by = "station_id",
  all.x = TRUE
)

station_availability[
  is.na(months_observed),
  months_observed := 0L
]

# Summarize station availability
station_availability[
  ,
  .(
    stations = .N,
    min_months = min(months_observed),
    median_months = as.numeric(median(months_observed)),
    max_months = max(months_observed),
    full_36_months = sum(months_observed == 36)
  ),
  by = group
]
# Step 36: Select stations observed in all 36 months

stable_stations <- merge(
  analysis_stations,
  station_availability[
    months_observed == 36,
    .(station_id)
  ],
  by = "station_id"
)

# Check stable station counts
table(stable_stations$group)

# Check total stations
nrow(stable_stations)
# Step 37: Calculate daily ridership for January 2023

library(data.table)

# Load January 2023 trips
trips <- fread(
  "data_processed/202301_citibike_clean.csv",
  colClasses = list(
    character = c("start_station_id", "end_station_id")
  )
)

# Keep trips starting at stable stations
borrows <- trips[
  start_station_id %in% stable_stations$station_id,
  .(
    date = as.Date(started_at),
    station_id = start_station_id
  )
][
  ,
  .(borrows = .N),
  by = .(date, station_id)
]

# Keep trips ending at stable stations
returns <- trips[
  end_station_id %in% stable_stations$station_id,
  .(
    date = as.Date(ended_at),
    station_id = end_station_id
  )
][
  ,
  .(returns = .N),
  by = .(date, station_id)
]

# Combine daily borrowings and returns
daily_ridership <- merge(
  borrows,
  returns,
  by = c("date", "station_id"),
  all = TRUE
)

# Replace missing counts with zero
daily_ridership[
  is.na(borrows),
  borrows := 0L
]

daily_ridership[
  is.na(returns),
  returns := 0L
]

# Calculate total ridership
daily_ridership[
  ,
  ridership := borrows + returns
]

# Check results
head(daily_ridership)

# Check number of station-day records
nrow(daily_ridership)
# Step 37: Calculate daily ridership for January 2023

library(data.table)

# Load January 2023 trips
trips <- fread(
  "data_processed/202301_citibike_clean.csv",
  select = c(
    "started_at",
    "ended_at",
    "start_station_id",
    "end_station_id"
  ),
  colClasses = list(
    character = c(
      "started_at",
      "ended_at",
      "start_station_id",
      "end_station_id"
    )
  )
)

# Extract dates without timezone conversion
trips[, start_date := as.Date(substr(started_at, 1, 10))]
trips[, end_date := as.Date(substr(ended_at, 1, 10))]

# Count daily borrowings
borrows <- trips[
  start_station_id %in% stable_stations$station_id,
  .(borrows = .N),
  by = .(date = start_date, station_id = start_station_id)
]

# Count daily returns
returns <- trips[
  end_station_id %in% stable_stations$station_id,
  .(returns = .N),
  by = .(date = end_date, station_id = end_station_id)
]

# Combine borrowings and returns
daily_ridership <- merge(
  borrows,
  returns,
  by = c("date", "station_id"),
  all = TRUE
)

# Replace missing values with zero
daily_ridership[is.na(borrows), borrows := 0L]
daily_ridership[is.na(returns), returns := 0L]

# Calculate total ridership
daily_ridership[, ridership := borrows + returns]

# Check results
head(daily_ridership)
range(daily_ridership$date)
nrow(daily_ridership)
# Step 38: Check trips outside January 2023

# Count trips starting before January
trips[
  start_date < as.Date("2023-01-01"),
  .N
]

# Count trips starting after January
trips[
  start_date > as.Date("2023-01-31"),
  .N
]

# Count trips ending outside January
trips[
  end_date < as.Date("2023-01-01") |
    end_date > as.Date("2023-01-31"),
  .N
]

# Step 39: Calculate daily ridership for all 36 months

daily_list <- lapply(clean_files, function(file) {
  
  # Read only required columns as character
  trips <- fread(
    file,
    select = c(
      "started_at", "ended_at",
      "start_station_id", "end_station_id"
    ),
    colClasses = list(
      character = c(
        "started_at", "ended_at",
        "start_station_id", "end_station_id"
      )
    )
  )
  
  # Extract dates without timezone conversion
  trips[, start_date := as.IDate(substr(started_at, 1, 10))]
  trips[, end_date := as.IDate(substr(ended_at, 1, 10))]
  
  # Count daily borrowings
  borrows <- trips[
    start_station_id %in% stable_stations$station_id,
    .(borrows = .N),
    by = .(date = start_date, station_id = start_station_id)
  ]
  
  # Count daily returns
  returns <- trips[
    end_station_id %in% stable_stations$station_id,
    .(returns = .N),
    by = .(date = end_date, station_id = end_station_id)
  ]
  
  # Combine counts
  daily <- merge(
    borrows, returns,
    by = c("date", "station_id"),
    all = TRUE
  )
  
  daily[is.na(borrows), borrows := 0L]
  daily[is.na(returns), returns := 0L]
  
  # Show progress
  print(paste("Completed:", basename(file)))
  
  return(daily)
})

# Combine all monthly results
daily_all <- rbindlist(daily_list)

# Aggregate records across month boundaries
daily_all <- daily_all[
  ,
  .(
    borrows = sum(borrows),
    returns = sum(returns)
  ),
  by = .(date, station_id)
]

# Keep the study period
daily_all <- daily_all[
  date >= as.IDate("2023-01-01") &
    date <= as.IDate("2025-12-31")
]

# Calculate total ridership
daily_all[, ridership := borrows + returns]

# Check results
range(daily_all$date)
uniqueN(daily_all$station_id)
nrow(daily_all)
head(daily_all)
# Step 40: Create a balanced station-day panel

# Create all combinations of stations and dates
all_dates <- seq(
  as.IDate("2023-01-01"),
  as.IDate("2025-12-31"),
  by = "day"
)

balanced_panel <- CJ(
  date = all_dates,
  station_id = stable_stations$station_id
)

# Merge with observed daily ridership
balanced_panel <- merge(
  balanced_panel,
  daily_all,
  by = c("date", "station_id"),
  all.x = TRUE
)

# Identify station-days without observed trips
balanced_panel[, missing_record := is.na(ridership)]

# Replace missing trip counts with zero
balanced_panel[is.na(borrows), borrows := 0L]
balanced_panel[is.na(returns), returns := 0L]
balanced_panel[is.na(ridership), ridership := 0L]

# Check results
nrow(balanced_panel)
uniqueN(balanced_panel$station_id)
range(balanced_panel$date)
balanced_panel[, sum(missing_record)]

# Step 41: Add treatment, policy, and time variables

library(data.table)
library(lubridate)

# Add treatment and control group labels
station_groups <- as.data.table(stable_stations)[
  , .(station_id, group)
]

balanced_panel <- merge(
  balanced_panel,
  station_groups,
  by = "station_id",
  all.x = TRUE
)

# Define treatment and post-policy indicators
balanced_panel[, treatment := as.integer(group == "Treatment")]

balanced_panel[, post := as.integer(
  date >= as.IDate("2025-01-05")
)]

# Create time variables
balanced_panel[, year := year(date)]
balanced_panel[, month := month(date)]
balanced_panel[, year_month := floor_date(as.Date(date), "month")]
balanced_panel[, week := floor_date(as.Date(date), "week", week_start = 1)]

# Define seasons
balanced_panel[, season := fifelse(
  month %in% c(12, 1, 2), "Winter",
  fifelse(
    month %in% c(3, 4, 5), "Spring",
    fifelse(month %in% c(6, 7, 8), "Summer", "Fall")
  )
)]

# Check results
balanced_panel[, .N, by = group]
balanced_panel[, .N, by = .(group, post)]
balanced_panel[, sum(is.na(group))]
head(balanced_panel)

# Step 42: Create log-transformed ridership

balanced_panel[, log_ridership := log1p(ridership)]

# Check results
balanced_panel[
  ,
  .(
    min_ridership = min(ridership),
    max_ridership = max(ridership),
    min_log = min(log_ridership),
    max_log = max(log_ridership),
    missing_log = sum(is.na(log_ridership))
  )
]

# Preview the variables
head(
  balanced_panel[
    ,
    .(date, station_id, group, ridership, log_ridership)
  ]
)
# Step 43: Prepare peak and off-peak ridership data

library(data.table)
library(lubridate)

# Define MTA official peak hours
is_peak_hour <- function(date, hour) {
  
  weekday <- wday(as.Date(date), week_start = 1)
  
  fifelse(
    weekday <= 5,
    hour >= 5 & hour < 21,
    hour >= 9 & hour < 21
  )
}

# Process each cleaned monthly file
peak_list <- lapply(clean_files, function(file) {
  
  trips <- fread(
    file,
    select = c(
      "started_at", "ended_at",
      "start_station_id", "end_station_id"
    ),
    colClasses = list(
      character = c(
        "started_at", "ended_at",
        "start_station_id", "end_station_id"
      )
    )
  )
  
  # Extract local dates and hours
  trips[, start_date := as.IDate(substr(started_at, 1, 10))]
  trips[, end_date := as.IDate(substr(ended_at, 1, 10))]
  
  trips[, start_hour := as.integer(substr(started_at, 12, 13))]
  trips[, end_hour := as.integer(substr(ended_at, 12, 13))]
  
  # Classify trips by official peak hours
  trips[, start_period := fifelse(
    is_peak_hour(start_date, start_hour),
    "Peak", "Off-Peak"
  )]
  
  trips[, end_period := fifelse(
    is_peak_hour(end_date, end_hour),
    "Peak", "Off-Peak"
  )]
  
  # Count borrowings
  borrows <- trips[
    start_station_id %in% stable_stations$station_id,
    .(borrows = .N),
    by = .(
      date = start_date,
      station_id = start_station_id,
      period = start_period
    )
  ]
  
  # Count returns
  returns <- trips[
    end_station_id %in% stable_stations$station_id,
    .(returns = .N),
    by = .(
      date = end_date,
      station_id = end_station_id,
      period = end_period
    )
  ]
  
  # Combine borrowings and returns
  daily <- merge(
    borrows,
    returns,
    by = c("date", "station_id", "period"),
    all = TRUE
  )
  
  daily[is.na(borrows), borrows := 0L]
  daily[is.na(returns), returns := 0L]
  
  print(paste("Completed:", basename(file)))
  
  return(daily)
})

# Combine all months
peak_daily <- rbindlist(peak_list)

# Aggregate across monthly files
peak_daily <- peak_daily[
  ,
  .(
    borrows = sum(borrows),
    returns = sum(returns)
  ),
  by = .(date, station_id, period)
]

# Keep the study period
peak_daily <- peak_daily[
  date >= as.IDate("2023-01-01") &
    date <= as.IDate("2025-12-31")
]

# Calculate ridership
peak_daily[, ridership := borrows + returns]

# Check results
peak_daily[, .N, by = period]

peak_daily[
  ,
  .(total_ridership = sum(ridership)),
  by = period
]

# Check consistency with daily data
peak_daily[, sum(ridership)]
daily_all[, sum(ridership)]
# Step 44: Create a balanced peak/off-peak panel

# Create all station-date-period combinations
peak_panel <- CJ(
  date = all_dates,
  station_id = stable_stations$station_id,
  period = c("Peak", "Off-Peak")
)

# Merge observed ridership
peak_panel <- merge(
  peak_panel,
  peak_daily,
  by = c("date", "station_id", "period"),
  all.x = TRUE
)

# Identify missing observations
peak_panel[, missing_record := is.na(ridership)]

# Replace missing counts with zero
peak_panel[is.na(borrows), borrows := 0L]
peak_panel[is.na(returns), returns := 0L]
peak_panel[is.na(ridership), ridership := 0L]

# Add treatment and control groups
peak_panel <- merge(
  peak_panel,
  station_groups,
  by = "station_id",
  all.x = TRUE
)

# Add policy indicators
peak_panel[, treatment := as.integer(group == "Treatment")]
peak_panel[, post := as.integer(date >= as.IDate("2025-01-05"))]

# Add time variables
peak_panel[, year := year(date)]
peak_panel[, month := month(date)]
peak_panel[, year_month := floor_date(as.Date(date), "month")]
peak_panel[, week := floor_date(as.Date(date), "week", week_start = 1)]

# Add log-transformed ridership
peak_panel[, log_ridership := log1p(ridership)]

# Check results
nrow(peak_panel)
peak_panel[, .N, by = period]
peak_panel[, sum(is.na(group))]
peak_panel[, sum(ridership)]
# Step 45: Save processed datasets

# Create output directory
dir.create("data_processed", showWarnings = FALSE)

# Save daily station-level panel
saveRDS(
  balanced_panel,
  "data_processed/station_daily_panel.rds"
)

# Save peak/off-peak panel
saveRDS(
  peak_panel,
  "data_processed/peak_offpeak_panel.rds"
)

# Save station locations and treatment groups
saveRDS(
  station_sf,
  "data_processed/station_locations.rds"
)

# Verify saved files
file.exists("data_processed/station_daily_panel.rds")
file.exists("data_processed/peak_offpeak_panel.rds")
file.exists("data_processed/station_locations.rds")