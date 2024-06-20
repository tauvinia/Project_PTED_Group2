##1. Packages

## Default repository
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org" 
options(repos=r)
})

## Define function check_pkg to tests if package is already installed and hence 
#only needs loading
check_pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

## Call check_pkg()
check_pkg("pacman")
check_pkg("dplyr")
check_pkg("ggplot2")
check_pkg("readr")
check_pkg("tidyr")
check_pkg("sf")
check_pkg("terra")
check_pkg("tmap")
check_pkg("zoo")
check_pkg("units")
check_pkg("plotly")
check_pkg("patchwork")
check_pkg("gitcreds")
check_pkg("lubridate")
check_pkg("readr")
check_pkg("forcats")
check_pkg("osmdata")
check_pkg("OpenStreetMap")
check_pkg("ggmap")
check_pkg("osmextract")
check_pkg("sfnetworks")

##Trajectory data

#Read data

#We have access to two datasets, a small and a large one. As long as there are 
#sufficient routes in the smaller dataset, we will work with this one.
 
# Small dataset
data <- read_delim("data/combined_data.csv", ",") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) 

# Big dataset
data_1 <- read_delim("data/combined_data_1.csv", ",") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

##2. Separate timestamp into date and time

# Convert the timestamp to POSIXct
data$timestamp <- ymd_hms(data$timestamp)

# Extract date component
data$date <- as.Date(data$timestamp)

# Extract time component
data$time <- format(data$timestamp, format = "%H:%M:%S")

# View the modified dataset
head(data)

##3. Select data within Karlsruhe

#In order to reduce the data set in a first step, we have filtered the data that
#lies within the border of Karlsruhe.

# Extract boundary of Karlsruhe
boundary <- getbb(place_name = "Karlsruhe") %>%
  opq() %>%
  add_osm_feature(key = "boundary",
                  value = "administrative") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  filter(name == "Karlsruhe")

# Select data that is within this boundary
data <- st_filter(data, boundary)

# Plot
ggplot() +
  geom_sf(data = boundary) +
  geom_sf(data = data, color = "darkblue")

#Occasionally, we encountered issues with a general Overpass server error when 
#attempting to access this data. To address this problem, we stored the boundary
#data locally on our computers, ensuring continuous access to this data at any 
#time.

#boundary <- st_read("data/boundary.gpkg")
#data <- st_read("data/small_data.gpkg")

#ggplot() +
geom_sf(data = boundary) +
  geom_sf(data = data, color = "green")

##4. Start and end point of the route

#The starting point for the outbound routes is Kriegsstraße 183, 76135 Karlsruhe,
#Germany. The coordinates of this point are stored in the variable home_point.

#The end point for the return routes is Klosterweg 28, 76131 Karlsruhe, Germany.
#The coordinates of this point are stored in the variable hadiko_point.

# Store start point
home_point <- st_point(c(8.36964749052686, 49.00535930123405)) %>% 
  st_sfc(crs = 4326)

# Store end point
hadiko_point <- st_point(c(8.4230360394263, 49.020260402061204)) %>% 
  st_sfc(crs = 4326)

##5. Extract route

#Separate the drives

# Calculate time differences between consecutive points in seconds
data <- data %>%
  arrange(timestamp) %>%  # Ensure data is sorted by time
  mutate(time_diff = c(NA, diff(timestamp)))

# Identify where the time difference exceeds 300 seconds (5 minutes)
data <- data %>%
  mutate(new_drive = ifelse(is.na(time_diff) | time_diff > 300, 1, 0))

# Assign drive IDs
data <- data %>%
  mutate(drive_id = cumsum(new_drive))

# View the resulting data
print(data)

#Filter the drives that happened between "home" and "hadiko", separate them into
#datasets with ways "home to hadiko" and "hadiko to home", visualize.

## Create function to filter drives
filter_drives <- function(start_point, start_distance, end_point, end_distance, data) {
  
  # Create the buffer around the start/end point
  start_buffer <- st_buffer(start_point, dist = start_distance)
  end_buffer <- st_buffer(end_point, dist = end_distance)
  
  # Check if the first point of each drive is within the start buffer
  first_points_within_start_buffer <- data %>%
    group_by(drive_id) %>%
    slice(1) %>%
    filter(st_within(geom, start_buffer, sparse = FALSE)) %>%
    pull(drive_id)
  
  # Check if the last point of each drive is within the end buffer
  last_points_within_end_buffer <- data %>%
    group_by(drive_id) %>%
    slice(n()) %>%
    filter(st_within(geom, end_buffer, sparse = FALSE)) %>%
    pull(drive_id)
  
  # Filter the original data to keep only rows with drive_id meeting both conditions
  valid_drive_ids <- intersect(first_points_within_start_buffer, last_points_within_end_buffer)
  data_start_end <- data %>%
    filter(drive_id %in% valid_drive_ids)
  print(valid_drive_ids)
  
  # Extract starting points for visualization
  starting_points <- data_start_end %>%
    group_by(drive_id) %>%
    slice(1) %>%
    ungroup()
  # Extract ending points for visualization
  ending_points <- data_start_end %>%
    group_by(drive_id) %>%
    slice(n()) %>%
    ungroup()
  # Visualize the starting/ending points and the start/end buffer as a ring
  plot <- ggplot() +
    geom_sf(data = data_start_end, aes(geometry = geom, colour = drive_id), alpha = 0.5, size = 0.1) +
    geom_sf(data = starting_points, aes(geometry = geom), color = "red", size = 1) +
    geom_sf(data = ending_points, aes(geometry = geom), color = "green", size = 1) +
    geom_sf(data = start_buffer, fill = NA, color = "red", size = 1, linetype = "dashed") +
    geom_sf(data = end_buffer, fill = NA, color = "green", size = 1, linetype = "dashed") +
    coord_sf() +
    theme_minimal() +
    labs(title = "Starting and Ending Points and Start/End Buffer Ring",
         subtitle = "Red points are the starting points, red dashed line is the start buffer ring, the same in green for end. Each drive has own colour")
  print(plot)
  return(data_start_end)
  
}
## Apply function
# on drives starting from home
data_from_home <- filter_drives(home_point, 500, hadiko_point, 500, data)
# on drives starting from Hadiko
data_from_hadiko <- filter_drives(hadiko_point, 500, home_point, 500, data)

#To gain a better overview, we created a bounding box around the relevant data points.

## Create bounding box around data
bbox <- st_bbox(data_from_home) |> 
  st_as_sfc()

#Using group by, we obtained an overview of how many routes there are.

## Group the data by drive_id
# Data from Home
group_data_from_home <- data_from_home %>%
  group_by(drive_id) %>%
  summarize(
    min_timestamp = min(timestamp),
    max_timestamp = max(timestamp)
  )

# Print
print(group_data_from_home)


# Data from Hadiko
group_data_from_hadiko <- data_from_hadiko %>%
  group_by(drive_id) %>%
  summarize(
    min_timestamp = min(timestamp),
    max_timestamp = max(timestamp)
  )

# Print
print(group_data_from_hadiko)

##6. Environmental Features OSM

#To relate the data to the environment, we used OpenStreetMap data. With the two
#functions below, we obtained an overview of the available data.

## Explore features
#available_features()

#available_tags(feature = "highway")

##7.Create Street Network

#By extracting the highways, we created a street network for our bounding box.
#{r fig.width=50, fig.height=40}
# Extract highways
highway <- getbb(place_name = "Karlsruhe") %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf() %>%
  .$osm_lines 

# Convert object to sfnetwork object
street_network <- as_sfnetwork(highway, directed = FALSE) %>% # set as undirected
  st_intersection(bbox)

# Overview
street_network

# Plot
ggplot() +
  geom_sf(data = street_network %>% activate(edges) %>% st_as_sf(), aes(color = highway), size = 3) + 
  geom_sf(data = street_network %>% activate(nodes) %>% st_as_sf()) +
  theme_void()

#We stored the nearest vertices to the start and endpoint of the routes.

# Coordinates of all nodes in the network
vertices_sf <- street_network %>%
  activate(nodes) %>%
  st_as_sf()

# Find the id of the vertex closest to start point
start_vertex <- st_nearest_feature(home_point, vertices_sf)

# Find the id of the vertex closest to end point
end_vertex <- st_nearest_feature(hadiko_point, vertices_sf)

# Print
cat("Start Vertex:", start_vertex, "\n")
cat("End Vertex:", end_vertex)


##8. Green spaces

#To determine the green spaces in the area, parks and forests were extracted 
#from OSM data. Since OSM polygons and multipolygons provided different data, 
#both were extracted and then merged.

# Parks
parks <- getbb(place_name = "Karlsruhe") %>%
  opq() %>%
  add_osm_feature(key = "leisure",
                  value = "park") %>%
  osmdata_sf() %>%
  .$osm_polygons %>%
  st_intersection(bbox)

# Forests Polygons
forest_polys <- getbb(place_name = "Karlsruhe") %>%
  opq() %>%
  add_osm_feature(key = "landuse",
                  value = "forest") %>%
  osmdata_sf() %>%
  .$osm_polygons %>%
  st_intersection(bbox)

# Forests Multipolygons
forest_multipolys <- getbb(place_name = "Karlsruhe") %>%
  opq() %>%
  add_osm_feature(key = "landuse",
                  value = "forest") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  st_make_valid() %>% 
  st_intersection(bbox)

# Merge
green_spaces <- bind_rows(parks, forest_polys, forest_multipolys) |> 
  st_union() |> 
  st_make_valid()

#As the grass polygons mostly intersect with the green space polygons and few 
#data points pass through them, as we can see in the visualization below, we 
#decided not to take this feature into account. We believe the green areas are 
#well covered by the forest and park extractions, so the grass does not need to 
#be considered.

# Trees
trees <- getbb(place_name = "Karlsruhe") %>%
  opq() %>%
  add_osm_feature(key = "natural",
                  value = "tree") %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_intersection(bbox)

# Grass
grass <- getbb(place_name = "Karlsruhe") %>%
  opq() %>%
  add_osm_feature(key = "landuse",
                  value = "grass") %>%
  osmdata_sf() %>%
  .$osm_polygons %>%
  st_intersection(bbox)

##9.Visualization of OSM Features

#To obtain a good overview of the extracted features, we visualized them.

#{r fig.width=50, fig.height=40}
# Plot
ggplot() +
  
  # Edges of street network
  geom_sf(data = street_network %>% activate(edges) %>% st_as_sf(), size = 2, color = "black") +
  
  # Green Spaces
  geom_sf(data = forest_multipolys, fill = "lightgreen") +
  
  # Trees
  geom_sf(data = trees, color = "darkgreen", size = 3) +
  
  # Grass
  geom_sf(data = grass, fill = "green") +
  
  # Movement data
  geom_sf(data = data_from_home, color = "steelblue", size = 1) +
  geom_sf(data = data_from_hadiko, color = "darkred", size = 1) +
  
  # Theme
  theme_void()






