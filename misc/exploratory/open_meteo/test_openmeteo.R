library(httr)
library(jsonlite)
library(tidyverse)
library(here)
library(sf)
library(maps)

# Set wokring directory
here::i_am("test_openmeteo.R")

# ------------ LOAD DATA ------------

# Load test bird data - only keep unique station locations
birds <- read_csv("Birdweather.Cardinals.csv") |>
  filter(!is.na(station_lat) & !is.na(station_lon)) |>
  distinct(station_lat, station_lon, .keep_all = TRUE)

# Visualization global bird data
world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
plot(st_geometry(world))
bird_pts <- st_as_sf(
  birds,
  coords = c("station_lon", "station_lat"),
  crs = 4326
)
plot(st_geometry(bird_pts), add = TRUE, col = 'red', pch = 20)

# clean up environment
rm(world, bird_pts)

#################### SPECIFY WHICH SIZE OF DATASET YOU WANT TO RUN

# work with 20 stations
#test_birds <- birds[1:20, ] ######### RUN LINES 41 AND CODE UNDER LINE 162

# work with 5 stations
#test_birds <- birds[1:5, ]  ######### RUN LINES 41 AND CODE UNDER LINE 112

# ------------ API FUNCTION ------------

# 2. Function to fetch data from the Open-Meteo Archive API
fetch_weather <- function(station, lat, lon, start_date, end_date) {
  url <- "https://archive-api.open-meteo.com/v1/archive"

  params <- list(
    latitude = lat,
    longitude = lon,
    start_date = start_date,
    end_date = end_date,
    # Selecting IFS 9km compatible variables
    hourly = "cloud_cover,cloud_cover_low,cloud_cover_mid,cloud_cover_high",
    timezone = "UTC"
  )

  response <- GET(url, query = params)

  # Check for HTTP errors
  if (http_error(response)) {
    stop(sprintf(
      "HTTP Error: %s",
      http_status(response)$message
    ))
  }

  # Parse JSON response
  content <- content(response, "text", encoding = "UTF-8")
  data <- fromJSON(content, flatten = TRUE)

  # Convert to a data frame
  df <- as.data.frame(data$hourly)

  # Get units
  units_df <- as.data.frame(data$hourly_units)

  # add units to df column names
  for (col in names(df)) {
    unit <- units_df[[col]]
    new_col_name <- paste0(col, "_", unit)
    names(df)[names(df) == col] <- new_col_name
  }

  # Get local weather station info
  weather_info <- list(
    data$latitude,
    data$longitude,
    data$utc_offset_seconds,
    data$timezone,
    data$timezone_abbreviation,
    data$elevation
  )
  names(weather_info) <- c(
    "weather_stat_latitude",
    "weather_stat_longitude",
    "weather_stat_utc_offset_seconds",
    "weather_stat_timezone",
    "weather_stat_timezone_abbr",
    "weather_stat_elevation"
  )

  for (info in names(weather_info)) {
    df[[info]] <- weather_info[[info]]
  }

  df <- df |>
    mutate(station = station) |>
    select(station, everything())

  df
}


#------------ FOR SMALL DATASETS: CALL API FUNCTION ------------

# 3. Execute batch pull for March 2023-2024
all_data <- test_birds |>
  transpose() |>
  map_df(function(row) {
    print(paste("Fetching:", row$station))
    result <- fetch_weather(
      row$station,
      row$station_lat,
      row$station_lon,
      "2023-03-01",
      "2024-03-31"
    )
    Sys.sleep(3)
    result
  })


#  -- JOIN DATA --

# Round to the nearest hour
test_rounded_birds <- test_birds |>
  mutate(og_timestamp = timestamp) |>
  mutate(
    timestamp = round_date(as.POSIXct(timestamp, tz = "UTC"), unit = "hour")
  )

# Convert time to readable format
new_data <- all_data |>
  mutate(
    timestamp = as.POSIXct(
      x = time_iso8601,
      format = "%Y-%m-%dT%H:%M",
      tz = "UTC"
    )
  )

# Join bird data to weather data
joined_data <- new_data |>
  left_join(test_rounded_birds, by = c("station", "timestamp"))

# Show only rows with birds with weather data
final_data <- joined_data |>
  filter(!is.na(og_timestamp))

# View results
head(final_data)


# ------------ FOR LARGE DATASETS: CALL API FUNCTION IN BATCHES ------------

# Create a grouping ID for every 5 rows
input_stations <- test_birds |>
  mutate(call_id = row_number()) |>
  mutate(batch_id = (row_number() - 1) %/% 5)

# Create storage directory
dir.create("weather_batches", showWarnings = FALSE)

batch_results <- list()

# Loop through each batch
for (id in unique(input_stations$batch_id)) {
  file_path <- paste0("weather_batches/batch_", id, ".csv")

  batch_results <- list()

  # Skip if already downloaded
  if (file.exists(file_path)) {
    next
  }

  message("Processing batch ", id, "...")

  # Filter data for just this batch
  current_batch <- input_stations |> filter(batch_id == id)

  for (x in seq_len(nrow(current_batch))) {
    row <- current_batch[x, ]

    tryCatch(
      {
        message("Fetching: ", row$station)

        result <- fetch_weather(
          row$station,
          row$station_lat,
          row$station_lon,
          "2023-03-01",
          "2024-03-31"
        )

        batch_results[[x]] <- result
        Sys.sleep(3)
      },
      error = function(e) {
        message("Error with station ", row$station, ": ", e$message)
      }
    )
  }
  if (length(batch_results) > 0) {
    batch_df <- bind_rows(batch_results)
    write.csv(batch_df, file_path, row.names = FALSE)
    message("Successfully saved batch ", id)
  }
}

## -- Join data --

# Combine everything at the very end
all_data <- list.files("weather_batches", full.names = TRUE) |>
  purrr::map_dfr(read.csv)


# Round to the nearest hour
test_rounded_birds <- test_birds |>
  mutate(og_timestamp = timestamp) |>
  mutate(
    timestamp = round_date(as.POSIXct(timestamp, tz = "UTC"), unit = "hour")
  )

# Convert time to readable format
new_data <- all_data |>
  mutate(
    timestamp = as.POSIXct(
      x = time_iso8601,
      format = "%Y-%m-%dT%H:%M",
      tz = "UTC"
    )
  )

# Join bird data to weather data
joined_data <- new_data |>
  left_join(test_rounded_birds, by = c("station", "timestamp"))

# Show only rows with birds with weather data
final_data <- joined_data |>
  filter(!is.na(og_timestamp))

# View results
head(final_data)

write_csv(final_data, "joined_birds_to_openmeteo.csv")
