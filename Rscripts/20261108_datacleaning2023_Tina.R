# Citi Bike Project
# Data Cleaning 2023
# Date: 2026-11-08

# 2023
# Load packages
library(tidyverse)
library(data.table)
library(lubridate)
library(sf)

# Step 1: Find all 2023 CSV files
files_2023 <- list.files(
  path = "data_raw/2023_citibike",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE)

basename(files_2023)
length(files_2023)

# Step 2: Preview first CSV
trips_preview <- fread(files_2023[1],nrows = 6)

trips_preview
names(trips_preview)
str(trips_preview)

# Step 3: Check file size (MB)
file.info(files_2023[1])$size / 1024^2


# Step 3: Import the first CSV

trips <- fread(
  files_2023[1],
  colClasses = list(
    character = c("start_station_id", "end_station_id")))

# Check dimensions
dim(trips)

# Check station ID types
str(trips[, .(
  start_station_id,
  end_station_id)])


# Step 4: Data quality checks

# Total number of trips
nrow(trips)

# Duplicate ride IDs
sum(duplicated(trips$ride_id))

# Missing station IDs
sum(
  is.na(trips$start_station_id) |
    is.na(trips$end_station_id) |
    trips$start_station_id == "" |
    trips$end_station_id == "",
  na.rm = TRUE
)

# Missing start/end times
sum(
  is.na(trips$started_at) |
    is.na(trips$ended_at)
)


# Calculate trip duration in minutes
trips[, duration_min := as.numeric(
  difftime(ended_at, started_at, units = "mins")
)]

# Summary statistics
summary(trips$duration_min)

# Count invalid durations
sum(
  is.na(trips$duration_min) |
    trips$duration_min < 1 |
    trips$duration_min > 1440
)


# Step 6: Data Cleaning

# Create a copy of the original data
trips_clean <- copy(trips)

# 1. Remove duplicate ride IDs
trips_clean <- unique(
  trips_clean,
  by = "ride_id"
)

# 2. Remove missing station IDs
trips_clean <- trips_clean[
  !is.na(start_station_id) &
    !is.na(end_station_id) &
    start_station_id != "" &
    end_station_id != ""
]

# 3. Remove missing timestamps
trips_clean <- trips_clean[
  !is.na(started_at) &
    !is.na(ended_at)
]

# 4. Remove invalid trip durations
trips_clean <- trips_clean[
  !is.na(duration_min) &
    duration_min >= 1 &
    duration_min <= 1440
]

# Compare before and after cleaning
nrow(trips)
nrow(trips_clean)

# Number of removed records
nrow(trips) - nrow(trips_clean)


# Step 7: Cleaning summary

cleaning_summary <- data.table(
  Stage = c(
    "Raw Data",
    "After Cleaning"
  ),
  Trips = c(
    nrow(trips),
    nrow(trips_clean)
  )
)

cleaning_summary[
  , Percentage := round(
    Trips / nrow(trips) * 100, 2
  )
]

cleaning_summary


# Step 8: Import all January 2023 CSV files

# Find January files
files_jan <- files_2023[
  grepl("202301-", basename(files_2023))
]

# Check filenames
basename(files_jan)

# Read and combine January files
trips_jan <- rbindlist(
  lapply(files_jan, function(file) {
    fread(
      file,
      colClasses = list(
        character = c(
          "start_station_id",
          "end_station_id"
        )
      )
    )
  }),
  use.names = TRUE
)

# Check total records
nrow(trips_jan)

# Check duplicates across files
sum(duplicated(trips_jan$ride_id))


# Step 9: Clean January 2023 data

# Keep the original January data
trips_jan_clean <- copy(trips_jan)

# Calculate trip duration in minutes
trips_jan_clean[, duration_min := as.numeric(
  difftime(ended_at, started_at, units = "mins")
)]

# Remove duplicate ride IDs
trips_jan_clean <- unique(
  trips_jan_clean,
  by = "ride_id"
)

# Remove missing station IDs
trips_jan_clean <- trips_jan_clean[
  !is.na(start_station_id) &
    !is.na(end_station_id) &
    start_station_id != "" &
    end_station_id != ""
]

# Remove missing timestamps
trips_jan_clean <- trips_jan_clean[
  !is.na(started_at) &
    !is.na(ended_at)
]

# Remove invalid trip durations
trips_jan_clean <- trips_jan_clean[
  !is.na(duration_min) &
    duration_min >= 1 &
    duration_min <= 1440
]

# Compare before and after cleaning
nrow(trips_jan)
nrow(trips_jan_clean)
nrow(trips_jan) - nrow(trips_jan_clean)


# Step 10: Save cleaned January data

dir.create(
  "data_processed",
  showWarnings = FALSE
)

fwrite(
  trips_jan_clean,
  "data_processed/202301_citibike_clean.csv"
)


# Step 11: Create a data cleaning function

clean_citibike <- function(data) {
  
  # Calculate trip duration
  data[, duration_min := as.numeric(
    difftime(ended_at, started_at, units = "mins")
  )]
  
  # Remove duplicate ride IDs
  data <- unique(data, by = "ride_id")
  
  # Remove missing station IDs
  data <- data[
    !is.na(start_station_id) &
      !is.na(end_station_id) &
      start_station_id != "" &
      end_station_id != ""
  ]
  
  # Remove missing timestamps
  data <- data[
    !is.na(started_at) &
      !is.na(ended_at)
  ]
  
  # Remove invalid durations
  data <- data[
    !is.na(duration_min) &
      duration_min >= 1 &
      duration_min <= 1440
  ]
  
  return(data)
}


# Step 12: Clean all 2023 monthly data

cleaning_results <- data.table()

for (month in 1:12) {
  
  month_id <- sprintf("2023%02d", month)
  
  # Find CSV files for this month
  month_files <- files_2023[
    startsWith(basename(files_2023), paste0(month_id, "-"))
  ]
  
  # Read and combine all CSVs for this month
  month_data <- rbindlist(
    lapply(month_files, function(file) {
      fread(
        file,
        colClasses = list(
          character = c("start_station_id", "end_station_id")
        )
      )
    }),
    use.names = TRUE
  )
  
  # Count raw trips
  raw_count <- nrow(month_data)
  
  # Clean data
  month_clean <- clean_citibike(month_data)
  
  # Count cleaned trips
  clean_count <- nrow(month_clean)
  
  # Save cleaned monthly data
  output_file <- paste0(
    "data_processed/",
    month_id,
    "_citibike_clean.csv"
  )
  
  fwrite(month_clean, output_file)
  
  # Record cleaning results
  cleaning_results <- rbind(
    cleaning_results,
    data.table(
      Month = month_id,
      Raw_Trips = raw_count,
      Clean_Trips = clean_count,
      Removed_Trips = raw_count - clean_count
    )
  )
  
  # Free memory before next month
  rm(month_data, month_clean)
  gc()
  
  print(paste("Completed:", month_id))
}

# View cleaning summary
cleaning_results





