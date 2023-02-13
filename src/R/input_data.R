source("src/R/utils.R")

rbind_metric <-
  function(metric_data,
           monthly_metric_data_vector,
           individual_name,
           month_name) {
    df <-
      data.frame(metric_name = monthly_metric_data_vector,
                 ind_name = individual_name,
                 period = month_name)
    metric_data <-
      rbind(metric_data, df) %>% mutate_if(is.numeric, round, 2)
    return(metric_data)

  }

INPUT_DATA_PATH <- INPUT_DATA_2020_2022
OUTPUT_DATA_PATH <- OUTPUTPUT_METRICS_2020_2022
GENERATE_COORD_GRAPHS <- FALSE
HEAT_MAP_ORDER = TRUE
suffix <- paste(ifelse(HEAT_MAP_ORDER, "_heatmaps_sorted.pdf", "_heatmaps.pdf"))

coast_path <- file.path(GIS_DATA_PATH, "coast/coast_cat_fr.shp")
river_path <- file.path(GIS_DATA_PATH, "coast/coast_cat_fr_rivers_3.shp")
port_path <- file.path(GIS_DATA_PATH, "coast/coast_cat_fr_port_3.shp")
river_muga_path <- file.path(GIS_DATA_PATH, "coast/riu_muga.shp")
receiver_loc_path <- file.path(INPUT_DATA_PATH, "resmed_receiver_locations.csv")
fish_database_path <- file.path(INPUT_DATA_PATH, "resmed_fish_database.csv")
lubina_detection_path <- file.path(INPUT_DATA_PATH, "lubina_detections.csv")
OUTPUT <- file.path(OUTPUT_DATA_PATH, "centrality")

coast <- st_read(coast_path)
river <- st_read(river_path)
port <- st_read(port_path)
river_muga <- st_read(river_muga_path)

locs = prepare_gis_receiver_locations(receiver_loc_path, coast)
data <- read.csv(lubina_detection_path)
data <- data %>% mutate(station=ifelse(station == "",  station_folder, station))


paper_type <- "a4r"
paper_height <- 8.268
paper_width <- 11.693

# start <- "2020-02-01 00:00"
# end <- "2021-07-31 23:59"

start <- "2020-09-01 00:00"
end <- "2022-04-27 00:00"
granularity <- 'months'
