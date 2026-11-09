
# Citi Bike Project
# Data Cleaning - 2025
# Date: 2026-11-08

#2025
# Step 1: Load packages
library(data.table)


# Step 2: Find all 2025 CSV files
files_2025 <- list.files(
  path = "data_raw/2025_citibike",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Check that all 12 months are present
months_found <- unique(
  substr(basename(files_2025), 1, 6)
)

print(sort(months_found))

expected_months <- sprintf("2025%02d", 1:12)

if (!all(expected_months %in% months_found)) {
  stop("Some 2025 monthly CSV files are missing.")
}

cat("Total CSV files:", length(files_2025), "\n")


# Step 3: Create data cleaning function
clean_citibike <- function(data) {
  
  # Calculate trip duration in minutes
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
  
  # Remove invalid trip durations
  data <- data[
    !is.na(duration_min) &
      duration_min >= 1 &
      duration_min <= 1440
  ]
  
  return(data)
}


# Step 4: Clean all 2025 monthly data

dir.create(
  "data_processed",
  showWarnings = FALSE
)

cleaning_results_2025 <- data.table()

for (month in 1:12) {
  
  month_id <- sprintf("2025%02d", month)
  
  # Find CSV files for this month
  month_files <- files_2025[
    startsWith(
      basename(files_2025),
      paste0(month_id, "-")
    )
  ]
  
  # Check monthly files
  if (length(month_files) == 0) {
    stop(paste("No CSV files found for", month_id))
  }
  
  # Read and combine monthly CSV files
  month_data <- rbindlist(
    lapply(month_files, function(file) {
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
  
  # Record raw data quality
  raw_count <- nrow(month_data)
  
  duplicate_count <- sum(
    duplicated(month_data$ride_id)
  )
  
  missing_station_count <- sum(
    is.na(month_data$start_station_id) |
      is.na(month_data$end_station_id) |
      month_data$start_station_id == "" |
      month_data$end_station_id == ""
  )
  
  # Calculate and check invalid durations
  month_data[, duration_min := as.numeric(
    difftime(ended_at, started_at, units = "mins")
  )]
  
  invalid_duration_count <- sum(
    is.na(month_data$duration_min) |
      month_data$duration_min < 1 |
      month_data$duration_min > 1440
  )
  
  missing_time_count <- sum(
    is.na(month_data$started_at) |
      is.na(month_data$ended_at)
  )
  
  # Clean data
  month_clean <- clean_citibike(month_data)
  
  clean_count <- nrow(month_clean)
  
  # Save cleaned monthly data
  output_file <- paste0(
    "data_processed/",
    month_id,
    "_citibike_clean.csv"
  )
  
  fwrite(month_clean, output_file)
  
  # Record monthly cleaning results
  cleaning_results_2025 <- rbind(
    cleaning_results_2025,
    data.table(
      Month = month_id,
      Raw_Trips = raw_count,
      Clean_Trips = clean_count,
      Removed_Trips = raw_count - clean_count,
      Duplicate_IDs = duplicate_count,
      Missing_Stations = missing_station_count,
      Missing_Timestamps = missing_time_count,
      Invalid_Durations = invalid_duration_count
    )
  )
  
  # Free memory
  rm(month_data, month_clean)
  gc(verbose = FALSE)
  
  print(paste("Completed:", month_id))
}


# Step 5: Review cleaning results
print(cleaning_results_2025)

# Total summary
cleaning_results_2025[
  , .(
    Raw_Trips = sum(Raw_Trips),
    Clean_Trips = sum(Clean_Trips),
    Removed_Trips = sum(Removed_Trips)
  )
]

# Check all 12 cleaned files exist
output_files <- paste0(
  "data_processed/",
  expected_months,
  "_citibike_clean.csv"
)

all(file.exists(output_files))


