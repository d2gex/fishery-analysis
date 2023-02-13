ROOT_PATH = getwd()
DATA_PATH = file.path(ROOT_PATH, 'data')
SRC_PATH = file.path(ROOT_PATH, 'R')
OUTPUTS_PATH = file.path(ROOT_PATH, 'outputs')
ORIGINAL_DATA_PATH = file.path(DATA_PATH, 'originals')
GIS_DATA_PATH = file.path(DATA_PATH, "gis_data")
RESMED_DATA_PATH = file.path(DATA_PATH, "res_med")
INPUT_DATA_2020_2021 <- file.path(RESMED_DATA_PATH, "2020_2021")
INPUT_DATA_2020_2022 <- file.path(RESMED_DATA_PATH, "2020_2022")
OUTPUT_METRICS_2020_2021 <- file.path(OUTPUTS_PATH, "metrics", "2020_2021")
OUTPUTPUT_METRICS_2020_2022 <- file.path(OUTPUTS_PATH, "metrics", "2020_2022")
RECEIVER_COOR_CRS <- "+proj=longlat +datum=WGS84 +no_defs"
STATION_TYPE_COLORS <- list(
  'green' = 'B',
  'black' = 'G',
  'blue' = 'M',
  'red' = 'C'
)

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