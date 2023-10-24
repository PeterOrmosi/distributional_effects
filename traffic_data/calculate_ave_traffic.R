# Install CLmapper
library(devtools)
devtools::install_local("S:/Knowledge/25. R packages/CLmapper/CLmapper_0.0.1.tar.gz")

# Load packages
library(CLmapper)
library(dplyr)
library(janitor)

# Load data and clean column names
stations <- read.csv("./data/raw/only_stations4.csv") %>% clean_names()
traffic <- read.csv("./data/raw/traffic.csv") %>% clean_names()

# Add ID of petrol stations
stations$station_id <- as.integer(row.names(stations))
stations <- stations %>%
  select(station_id, everything())

nrow(stations) # 1299 petrol stations
nrow(traffic) # 4460 traffic stations

# Cross join 1299 petrol stations x 4460 traffic stations
stations_traffic_merged <- merge(stations, traffic, by=NULL) %>%
          # Rename columns
          rename(station_lat = lat,
                 station_lon = lon,
                 traffic_lat = latitude,
                 traffic_lon = longitude)

nrow(stations_traffic_merged) # 5793540 pairs

# Calculate straight line distance
distance_df <- straight_line_distance(stations_traffic_merged,
                                      origin_lat = stations_traffic_merged$station_lat,
                                      origin_lon = stations_traffic_merged$station_lon,
                                      destination_lat = stations_traffic_merged$traffic_lat,
                                      destination_lon = stations_traffic_merged$traffic_lon)

distance_df <- distance_df %>%
        # Convert kilometer distance to miles
        mutate(radius_miles = straight_line_dis * 0.621371) %>%
        # Create dummies within M miles, M = [1,2,5]
        mutate(within_1_miles = ifelse(radius_miles <= 1, 1, 0),
               within_2_miles = ifelse(radius_miles <= 2, 1, 0),
               within_5_miles = ifelse(radius_miles <= 5, 1, 0))

# Note: traffic$id == "mrwa_traffic_digest_2018.4461" has missing coordinates
# traffic %>% subset(id == "mrwa_traffic_digest_2018.4461")


# Create a function to calculate mean and sum traffic measures within a specified mile radius
calculate_traffic <- function(distance_df, mile_range) {
  result <- distance_df %>%
    # Filter to traffic stations within M miles
    filter(get(paste0("within_", mile_range, "_miles")) == 1) %>%
    # Compute mean and sum traffic measures by petrol station
    group_by(station_id, full_address) %>%
    summarise(
      ave_traffic_mon_sun = mean(properties_mon_sun, na.rm = TRUE),
      ave_traffic_mon_fri = mean(properties_mon_fri, na.rm = TRUE),
      ave_traffic_sat_sun = mean(properties_sat_sun, na.rm = TRUE),
      ave_traffic_heavy_mon_sun = mean(properties_pct_heavy_mon_sun, na.rm = TRUE),
      ave_traffic_heavy_mon_fri = mean(properties_pct_heavy_mon_fri, na.rm = TRUE),
      ave_traffic_heavy_sat_sun = mean(properties_pct_heavy_sat_sun, na.rm = TRUE),
      
      sum_traffic_mon_sun = sum(properties_mon_sun, na.rm = TRUE),
      sum_traffic_mon_fri = sum(properties_mon_fri, na.rm = TRUE),
      sum_traffic_sat_sun = sum(properties_sat_sun, na.rm = TRUE),
      sum_traffic_heavy_mon_sun = sum(properties_pct_heavy_mon_sun, na.rm = TRUE),
      sum_traffic_heavy_mon_fri = sum(properties_pct_heavy_mon_fri, na.rm = TRUE),
      sum_traffic_heavy_sat_sun = sum(properties_pct_heavy_sat_sun, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Sort rows by station_id
    arrange(station_id) %>%
    # Rename new columns with M miles
    rename_with(~paste0(.x, "_", mile_range, "_miles"), starts_with(c("ave_traffic", "sum_traffic")))
  
  return(result)
}


# Define the mile ranges
mile_ranges <- c(1, 2, 5)

# Create a list to store the results for different mile ranges
results_list <- list()

# Loop through the mile ranges and calculate mean and sum traffic measures
for (mile_range in mile_ranges) {
  result <- calculate_traffic(distance_df, mile_range)
  results_list[[paste0("traffic_", mile_range, "_miles")]] <- result
}

# Access the results for different mile ranges using results_list
traffic_1_miles <- results_list$traffic_1_miles
traffic_2_miles <- results_list$traffic_2_miles
traffic_5_miles <- results_list$traffic_5_miles

# Merge 18 new columns to stations dataframe
stations_final <- merge(stations, traffic_1_miles, by = c("station_id", "full_address"), all = TRUE) %>%
  merge(traffic_2_miles, by = c("station_id", "full_address"), all = TRUE) %>%
  merge(traffic_5_miles, by = c("station_id", "full_address"), all = TRUE)

# Export stations_final to csv
write.csv(stations_final, "./data/processed/ave_sum_traffic_within_radius_21092023.csv", row.names = FALSE)
