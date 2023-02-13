library(ggplot2)
library(igraph)
library(ggraph)
library(sf)
library(dplyr)
library(tidyr)
library(R6)
library(gridExtra)
library(stringr)
library(zeallot)

# Define paths of the system
ROOT_PATH = getwd()
DATA_PATH = file.path(ROOT_PATH, 'data')
ORIGINAL_DATA_PATH = file.path(DATA_PATH, 'originals')
GIS_DATA_PATH = file.path(DATA_PATH, "gis_data")
RESMED_DATA_PATH = file.path(DATA_PATH, "res_med")
INPUT_DATA_2020_2021 <- file.path(RESMED_DATA_PATH, "2020_2021")
INPUT_DATA_2020_2022 <- file.path(RESMED_DATA_PATH, "2020_2022")
INPUT_DATA_PATH <- INPUT_DATA_2020_2022
RECEIVER_COOR_CRS <- "+proj=longlat +datum=WGS84 +no_defs"


# Path to shapefiles
coast_path <- file.path(GIS_DATA_PATH, "coast/coast_cat_fr.shp")
river_path <- file.path(GIS_DATA_PATH, "coast/coast_cat_fr_rivers_3.shp")
port_path <- file.path(GIS_DATA_PATH, "coast/coast_cat_fr_port_3.shp")
river_muga_path <- file.path(GIS_DATA_PATH, "coast/riu_muga.shp")
receiver_loc_path <- file.path(INPUT_DATA_PATH, "resmed_receiver_locations.csv")
lubina_detection_path <- file.path(INPUT_DATA_PATH, "lubina_detections.csv")

# Read shapefiels
coast <- st_read(coast_path)
river <- st_read(river_path)
port <- st_read(port_path)
river_muga <- st_read(river_muga_path)

# Read data and prepare matrix of detections
locs = prepare_gis_receiver_locations(receiver_loc_path, coast)
data <- read.csv(lubina_detection_path)
data <- data %>% mutate(station=ifelse(station == "",  station_folder, station))

# Set the boundaries of the data analsyis
start <- "2020-09-01 00:00"
end <- "2022-04-27 00:00"
granularity <- 'months'
