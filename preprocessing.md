

```r
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
```

```
## Loading required package: pacman
```

```r
check_pkg("dplyr")
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
check_pkg("ggplot2")
```

```
## Loading required package: ggplot2
```

```r
check_pkg("readr")
```

```
## Loading required package: readr
```

```r
check_pkg("tidyr")
```

```
## Loading required package: tidyr
```

```r
check_pkg("sf")
```

```
## Loading required package: sf
```

```
## Linking to GEOS 3.11.2, GDAL 3.8.2, PROJ 9.3.1; sf_use_s2() is TRUE
```

```r
check_pkg("terra")
```

```
## Loading required package: terra
```

```
## terra 1.7.71
```

```
## 
## Attaching package: 'terra'
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

```r
check_pkg("tmap")
```

```
## Loading required package: tmap
```

```
## Breaking News: tmap 3.x is retiring. Please test v4, e.g. with
## remotes::install_github('r-tmap/tmap')
```

```r
check_pkg("zoo")
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following object is masked from 'package:terra':
## 
##     time<-
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
check_pkg("units")
```

```
## Loading required package: units
```

```
## udunits database from C:/Users/Tatiana/AppData/Local/R/win-library/4.3/units/share/udunits/udunits2.xml
```

```r
check_pkg("plotly")
```

```
## Loading required package: plotly
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
check_pkg("patchwork")
```

```
## Loading required package: patchwork
```

```
## 
## Attaching package: 'patchwork'
```

```
## The following object is masked from 'package:terra':
## 
##     area
```

```r
check_pkg("gitcreds")
```

```
## Loading required package: gitcreds
```

```r
check_pkg("lubridate")
```

```
## Loading required package: lubridate
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:terra':
## 
##     intersect, union
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
check_pkg("readr")
check_pkg("forcats")
```

```
## Loading required package: forcats
```

```r
check_pkg("osmdata")
```

```
## Loading required package: osmdata
```

```
## Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright
```

```r
check_pkg("OpenStreetMap")
```

```
## Loading required package: OpenStreetMap
```

```r
check_pkg("ggmap")
```

```
## Loading required package: ggmap
```

```
## ℹ Google's Terms of Service: <https://mapsplatform.google.com>
##   Stadia Maps' Terms of Service: <https://stadiamaps.com/terms-of-service/>
##   OpenStreetMap's Tile Usage Policy: <https://operations.osmfoundation.org/policies/tiles/>
## ℹ Please cite ggmap if you use it! Use `citation("ggmap")` for details.
## 
## Attaching package: 'ggmap'
## 
## 
## The following object is masked from 'package:plotly':
## 
##     wind
## 
## 
## The following object is masked from 'package:terra':
## 
##     inset
```

```r
check_pkg("osmextract")
```

```
## Loading required package: osmextract
## Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright.
## Check the package website, https://docs.ropensci.org/osmextract/, for more details.
```

```r
check_pkg("sfnetworks")
```

```
## Loading required package: sfnetworks
```

```r
##Trajectory data

#Read data

#We have access to two datasets, a small and a large one. As long as there are 
#sufficient routes in the smaller dataset, we will work with this one.
 
# Small dataset
data <- read_delim("data/combined_data.csv", ",") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) 
```

```
## Error: 'data/combined_data.csv' does not exist in current working directory ('C:/Users/Tatiana/Desktop/ZHAW/Patterns and Trends in Environmental Data/Project/Project_final/Project_PTED_Group2').
```

```r
# Big dataset
data_1 <- read_delim("data/combined_data_1.csv", ",") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
```

```
## Error: 'data/combined_data_1.csv' does not exist in current working directory ('C:/Users/Tatiana/Desktop/ZHAW/Patterns and Trends in Environmental Data/Project/Project_final/Project_PTED_Group2').
```

```r
##2. Separate timestamp into date and time

# Convert the timestamp to POSIXct
data$timestamp <- ymd_hms(data$timestamp)
```

```
## Error in data$timestamp: object of type 'closure' is not subsettable
```

```r
# Extract date component
data$date <- as.Date(data$timestamp)
```

```
## Error in data$timestamp: object of type 'closure' is not subsettable
```

```r
# Extract time component
data$time <- format(data$timestamp, format = "%H:%M:%S")
```

```
## Error in data$timestamp: object of type 'closure' is not subsettable
```

```r
# View the modified dataset
head(data)
```

```
##                                                                             
## 1 function (..., list = character(), package = NULL, lib.loc = NULL,        
## 2     verbose = getOption("verbose"), envir = .GlobalEnv, overwrite = TRUE) 
## 3 {                                                                         
## 4     fileExt <- function(x) {                                              
## 5         db <- grepl("\\\\.[^.]+\\\\.(gz|bz2|xz)$", x)                     
## 6         ans <- sub(".*\\\\.", "", x)
```

```r
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
```

```
## Error in check_for_error(paste0(doc)): General overpass server error; returned:
## The data included in this document is from www.openstreetmap.org. The data is made available under ODbL. runtime error: Query timed out in "recurse" at line 7 after 53 seconds.
```

```r
# Select data that is within this boundary
data <- st_filter(data, boundary)
```

```
## Error in UseMethod("st_filter"): no applicable method for 'st_filter' applied to an object of class "function"
```

```r
# Plot
ggplot() +
  geom_sf(data = boundary) +
  geom_sf(data = data, color = "darkblue")
```

```
## Error in eval(expr, envir, enclos): object 'boundary' not found
```

```r
#Occasionally, we encountered issues with a general Overpass server error when 
#attempting to access this data. To address this problem, we stored the boundary
#data locally on our computers, ensuring continuous access to this data at any 
#time.

#boundary <- st_read("data/boundary.gpkg")
#data <- st_read("data/small_data.gpkg")

#ggplot() +
geom_sf(data = boundary) +
  geom_sf(data = data, color = "green")
```

```
## Error in eval(expr, envir, enclos): object 'boundary' not found
```

```r
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
```

```
## Error in UseMethod("arrange"): no applicable method for 'arrange' applied to an object of class "function"
```

```r
# Identify where the time difference exceeds 300 seconds (5 minutes)
data <- data %>%
  mutate(new_drive = ifelse(is.na(time_diff) | time_diff > 300, 1, 0))
```

```
## Error in UseMethod("mutate"): no applicable method for 'mutate' applied to an object of class "function"
```

```r
# Assign drive IDs
data <- data %>%
  mutate(drive_id = cumsum(new_drive))
```

```
## Error in UseMethod("mutate"): no applicable method for 'mutate' applied to an object of class "function"
```

```r
# View the resulting data
print(data)
```

```
## function (..., list = character(), package = NULL, lib.loc = NULL, 
##     verbose = getOption("verbose"), envir = .GlobalEnv, overwrite = TRUE) 
## {
##     fileExt <- function(x) {
##         db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
##         ans <- sub(".*\\.", "", x)
##         ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
##             x[db])
##         ans
##     }
##     my_read_table <- function(...) {
##         lcc <- Sys.getlocale("LC_COLLATE")
##         on.exit(Sys.setlocale("LC_COLLATE", lcc))
##         Sys.setlocale("LC_COLLATE", "C")
##         read.table(...)
##     }
##     stopifnot(is.character(list))
##     names <- c(as.character(substitute(list(...))[-1L]), list)
##     if (!is.null(package)) {
##         if (!is.character(package)) 
##             stop("'package' must be a character vector or NULL")
##     }
##     paths <- find.package(package, lib.loc, verbose = verbose)
##     if (is.null(lib.loc)) 
##         paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
##             paths)
##     paths <- unique(normalizePath(paths[file.exists(paths)]))
##     paths <- paths[dir.exists(file.path(paths, "data"))]
##     dataExts <- tools:::.make_file_exts("data")
##     if (length(names) == 0L) {
##         db <- matrix(character(), nrow = 0L, ncol = 4L)
##         for (path in paths) {
##             entries <- NULL
##             packageName <- if (file_test("-f", file.path(path, 
##                 "DESCRIPTION"))) 
##                 basename(path)
##             else "."
##             if (file_test("-f", INDEX <- file.path(path, "Meta", 
##                 "data.rds"))) {
##                 entries <- readRDS(INDEX)
##             }
##             else {
##                 dataDir <- file.path(path, "data")
##                 entries <- tools::list_files_with_type(dataDir, 
##                   "data")
##                 if (length(entries)) {
##                   entries <- unique(tools::file_path_sans_ext(basename(entries)))
##                   entries <- cbind(entries, "")
##                 }
##             }
##             if (NROW(entries)) {
##                 if (is.matrix(entries) && ncol(entries) == 2L) 
##                   db <- rbind(db, cbind(packageName, dirname(path), 
##                     entries))
##                 else warning(gettextf("data index for package %s is invalid and will be ignored", 
##                   sQuote(packageName)), domain = NA, call. = FALSE)
##             }
##         }
##         colnames(db) <- c("Package", "LibPath", "Item", "Title")
##         footer <- if (missing(package)) 
##             paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
##                 "\n", "to list the data sets in all *available* packages.")
##         else NULL
##         y <- list(title = "Data sets", header = NULL, results = db, 
##             footer = footer)
##         class(y) <- "packageIQR"
##         return(y)
##     }
##     paths <- file.path(paths, "data")
##     for (name in names) {
##         found <- FALSE
##         for (p in paths) {
##             tmp_env <- if (overwrite) 
##                 envir
##             else new.env()
##             if (file_test("-f", file.path(p, "Rdata.rds"))) {
##                 rds <- readRDS(file.path(p, "Rdata.rds"))
##                 if (name %in% names(rds)) {
##                   found <- TRUE
##                   if (verbose) 
##                     message(sprintf("name=%s:\t found in Rdata.rds", 
##                       name), domain = NA)
##                   thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
##                   thispkg <- sub("_.*$", "", thispkg)
##                   thispkg <- paste0("package:", thispkg)
##                   objs <- rds[[name]]
##                   lazyLoad(file.path(p, "Rdata"), envir = tmp_env, 
##                     filter = function(x) x %in% objs)
##                   break
##                 }
##                 else if (verbose) 
##                   message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
##                     name, paste(names(rds), collapse = ",")), 
##                     domain = NA)
##             }
##             if (file_test("-f", file.path(p, "Rdata.zip"))) {
##                 warning("zipped data found for package ", sQuote(basename(dirname(p))), 
##                   ".\nThat is defunct, so please re-install the package.", 
##                   domain = NA)
##                 if (file_test("-f", fp <- file.path(p, "filelist"))) 
##                   files <- file.path(p, scan(fp, what = "", quiet = TRUE))
##                 else {
##                   warning(gettextf("file 'filelist' is missing for directory %s", 
##                     sQuote(p)), domain = NA)
##                   next
##                 }
##             }
##             else {
##                 files <- list.files(p, full.names = TRUE)
##             }
##             files <- files[grep(name, files, fixed = TRUE)]
##             if (length(files) > 1L) {
##                 o <- match(fileExt(files), dataExts, nomatch = 100L)
##                 paths0 <- dirname(files)
##                 paths0 <- factor(paths0, levels = unique(paths0))
##                 files <- files[order(paths0, o)]
##             }
##             if (length(files)) {
##                 for (file in files) {
##                   if (verbose) 
##                     message("name=", name, ":\t file= ...", .Platform$file.sep, 
##                       basename(file), "::\t", appendLF = FALSE, 
##                       domain = NA)
##                   ext <- fileExt(file)
##                   if (basename(file) != paste0(name, ".", ext)) 
##                     found <- FALSE
##                   else {
##                     found <- TRUE
##                     zfile <- file
##                     zipname <- file.path(dirname(file), "Rdata.zip")
##                     if (file.exists(zipname)) {
##                       Rdatadir <- tempfile("Rdata")
##                       dir.create(Rdatadir, showWarnings = FALSE)
##                       topic <- basename(file)
##                       rc <- .External(C_unzip, zipname, topic, 
##                         Rdatadir, FALSE, TRUE, FALSE, FALSE)
##                       if (rc == 0L) 
##                         zfile <- file.path(Rdatadir, topic)
##                     }
##                     if (zfile != file) 
##                       on.exit(unlink(zfile))
##                     switch(ext, R = , r = {
##                       library("utils")
##                       sys.source(zfile, chdir = TRUE, envir = tmp_env)
##                     }, RData = , rdata = , rda = load(zfile, 
##                       envir = tmp_env), TXT = , txt = , tab = , 
##                       tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
##                       txt.bz2 = , txt.xz = assign(name, my_read_table(zfile, 
##                         header = TRUE, as.is = FALSE), envir = tmp_env), 
##                       CSV = , csv = , csv.gz = , csv.bz2 = , 
##                       csv.xz = assign(name, my_read_table(zfile, 
##                         header = TRUE, sep = ";", as.is = FALSE), 
##                         envir = tmp_env), found <- FALSE)
##                   }
##                   if (found) 
##                     break
##                 }
##                 if (verbose) 
##                   message(if (!found) 
##                     "*NOT* ", "found", domain = NA)
##             }
##             if (found) 
##                 break
##         }
##         if (!found) {
##             warning(gettextf("data set %s not found", sQuote(name)), 
##                 domain = NA)
##         }
##         else if (!overwrite) {
##             for (o in ls(envir = tmp_env, all.names = TRUE)) {
##                 if (exists(o, envir = envir, inherits = FALSE)) 
##                   warning(gettextf("an object named %s already exists and will not be overwritten", 
##                     sQuote(o)))
##                 else assign(o, get(o, envir = tmp_env, inherits = FALSE), 
##                   envir = envir)
##             }
##             rm(tmp_env)
##         }
##     }
##     invisible(names)
## }
## <bytecode: 0x000001cb93b0b4d8>
## <environment: namespace:utils>
```

```r
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
```

```
## Error in UseMethod("group_by"): no applicable method for 'group_by' applied to an object of class "function"
```

```r
# on drives starting from Hadiko
data_from_hadiko <- filter_drives(hadiko_point, 500, home_point, 500, data)
```

```
## Error in UseMethod("group_by"): no applicable method for 'group_by' applied to an object of class "function"
```

```r
#To gain a better overview, we created a bounding box around the relevant data points.

## Create bounding box around data
bbox <- st_bbox(data_from_home) |> 
  st_as_sfc()
```

```
## Error in eval(expr, envir, enclos): object 'data_from_home' not found
```

```r
#Using group by, we obtained an overview of how many routes there are.

## Group the data by drive_id
# Data from Home
group_data_from_home <- data_from_home %>%
  group_by(drive_id) %>%
  summarize(
    min_timestamp = min(timestamp),
    max_timestamp = max(timestamp)
  )
```

```
## Error in eval(expr, envir, enclos): object 'data_from_home' not found
```

```r
# Print
print(group_data_from_home)
```

```
## Error in eval(expr, envir, enclos): object 'group_data_from_home' not found
```

```r
# Data from Hadiko
group_data_from_hadiko <- data_from_hadiko %>%
  group_by(drive_id) %>%
  summarize(
    min_timestamp = min(timestamp),
    max_timestamp = max(timestamp)
  )
```

```
## Error in eval(expr, envir, enclos): object 'data_from_hadiko' not found
```

```r
# Print
print(group_data_from_hadiko)
```

```
## Error in eval(expr, envir, enclos): object 'group_data_from_hadiko' not found
```

```r
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
```

```
## Error: object 'bbox' not found
```

```r
# Overview
street_network
```

```
## Error in eval(expr, envir, enclos): object 'street_network' not found
```

```r
# Plot
ggplot() +
  geom_sf(data = street_network %>% activate(edges) %>% st_as_sf(), aes(color = highway), size = 3) + 
  geom_sf(data = street_network %>% activate(nodes) %>% st_as_sf()) +
  theme_void()
```

```
## Error in eval(expr, envir, enclos): object 'street_network' not found
```

```r
#We stored the nearest vertices to the start and endpoint of the routes.

# Coordinates of all nodes in the network
vertices_sf <- street_network %>%
  activate(nodes) %>%
  st_as_sf()
```

```
## Error in eval(expr, envir, enclos): object 'street_network' not found
```

```r
# Find the id of the vertex closest to start point
start_vertex <- st_nearest_feature(home_point, vertices_sf)
```

```
## Error in eval(expr, envir, enclos): object 'vertices_sf' not found
```

```r
# Find the id of the vertex closest to end point
end_vertex <- st_nearest_feature(hadiko_point, vertices_sf)
```

```
## Error in eval(expr, envir, enclos): object 'vertices_sf' not found
```

```r
# Print
cat("Start Vertex:", start_vertex, "\n")
```

```
## Error in eval(expr, envir, enclos): object 'start_vertex' not found
```

```r
cat("End Vertex:", end_vertex)
```

```
## Error in eval(expr, envir, enclos): object 'end_vertex' not found
```

```r
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
```

```
## Error: object 'bbox' not found
```

```r
# Forests Polygons
forest_polys <- getbb(place_name = "Karlsruhe") %>%
  opq() %>%
  add_osm_feature(key = "landuse",
                  value = "forest") %>%
  osmdata_sf() %>%
  .$osm_polygons %>%
  st_intersection(bbox)
```

```
## Error: object 'bbox' not found
```

```r
# Forests Multipolygons
forest_multipolys <- getbb(place_name = "Karlsruhe") %>%
  opq() %>%
  add_osm_feature(key = "landuse",
                  value = "forest") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  st_make_valid() %>% 
  st_intersection(bbox)
```

```
## Error: object 'bbox' not found
```

```r
# Merge
green_spaces <- bind_rows(parks, forest_polys, forest_multipolys) |> 
  st_union() |> 
  st_make_valid()
```

```
## Error in eval(expr, envir, enclos): object 'parks' not found
```

```r
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
```

```
## Error: object 'bbox' not found
```

```r
# Grass
grass <- getbb(place_name = "Karlsruhe") %>%
  opq() %>%
  add_osm_feature(key = "landuse",
                  value = "grass") %>%
  osmdata_sf() %>%
  .$osm_polygons %>%
  st_intersection(bbox)
```

```
## Error: object 'bbox' not found
```

```r
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
```

```
## Error in eval(expr, envir, enclos): object 'street_network' not found
```

