source("R/config.R")


prepare_gis_receiver_locations <- function(locations_path, coast) {
  #'Read the locations of all hydrophones and add prepare it to follow on
  #'the same projection as a given one.
  #'@param locations_path A string
  #'@return A dataframe

  locs <- read.csv2(locations_path)
  locs <- locs %>%
    dplyr::select(station, Longitude, Latitude) %>%
    filter(!duplicated(station)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"),
             crs = RECEIVER_COOR_CRS) %>%   # from EPSG:4326 (longlat) ...
    st_transform(locs, crs = st_crs(coast))     # to UTM 31N / WGS84
  return (locs)
}


fetch_species <- function(data, id_species) {
  #' Fetches all rows for a particular species
  #' @param data A dataframe
  #' @param id_species A string
  #' @return A dataframe
  return (data %>%
            filter(startsWith(ind_name, id_species)))
}

fetch_individual <- function(data, id_individual) {
  #' Fetches all rows for an individual from a given dataframe
  #' @param data A dataframe
  #' @param id_individual A string
  #' @return A dataframe
  return (data %>% filter(ind_name == id_individual))
}

fetch_individuals <- function(data, id_indivudals, grouped = TRUE) {
  #' If grouped == True, it returns a dataframe that contains only the passed
  #' individual. Otherwise a named list of dataframes where each item in the list
  #' represent an individual
  if (grouped) {
    return (data %>% filter(ind_name %in% id_indivudals))
  }
  data_views <- lapply(id_indivudals, function(individual) {
    data %>% filter(ind_name == individual)
  })
  names(data_views) <- id_indivudals
  return(data_views)
}


correct_bbox <- function(bb, minhalfsize = 2000) {
  #' Corrects size of the bounding box of a graph so that its no wider and
  #' taller than twice a given parameter. The frame is always returned as a
  #' square.
  #' @param bb A list of numbes
  #' @param minhalfsize A number
  #' @return List of numbers

  # arg bb is bounding box from directed graph
  #     minhalfsize (meters) defines minimum box size
  # returns corrected bbox (minimum size if too small, extra margin added)

  width <- bb$xmax - bb$xmin
  height <- bb$ymax - bb$ymin
  meanx <- mean(c(bb$xmin, bb$xmax))
  meany <- mean(c(bb$ymin, bb$ymax))

  # correct bounding box (add margin, define minimum box size)
  if (max(width, height) <= minhalfsize) {
    bb$xmin <- meanx - minhalfsize
    bb$xmax <- meanx + minhalfsize
    bb$ymin <- meany - minhalfsize
    bb$ymax <- meany + minhalfsize
  } else {
    if (width > height) {
      bb$ymin <- meany - width / 2
      bb$ymax <- meany + width / 2
    } else {
      bb$xmin <- meanx - height / 2
      bb$xmax <- meanx + height / 2
    }
  }

  return(bb)
}

adjust_coord_bb <- function (coords, min_half_size) {
  #' @param coords data_frame with coordinates
  #' @param min_half_size integer with half of the size of the bounding box
  #'
  # get actual bounding box from graph
  bb <- coords %>%
    summarise(
      xmin = min(X),
      xmax = max(X),
      ymin = min(Y),
      ymax = max(Y)
    )

  # resize bounding box
  return(correct_bbox(bb, min_half_size))
}

get_graph_coordinates <- function (g, locs) {
  graph_order_node_names <- vertex_attr(g)$name
  coords <-
    data.frame(station = locs$station, st_coordinates(locs)) %>%
    filter(station %in% graph_order_node_names)
  coords <- coords[match(graph_order_node_names, coords$station),]
  return (coords)

}

plots_to_pdf <-
  function(plot_objects,
           filename,
           paper,
           height,
           width) {
    #' Creates a pdf with a list of plotted objects
    #' @param all_graph_plots A list of plots
    #' @param filename A string
    #' @param paper A string
    #' @param height A integer
    #' @param width A integer
    #' @return NULL

    pdf(filename,
        paper = paper,
        height = height,
        width = width)

    for (i in seq_along(plot_objects)) {
      p_object <- plot_objects[[i]]
      if (('ggplot' %in% class(p_object)) |
          ('igraph' %in% class(p_object))) {
        print(p_object)
      }
      else {
        draw(p_object)
      }
    }

    dev.off()
  }



get_interval_date_criteria <- function(granularity) {
  if (granularity == 'months') {
    interval_criteria <- "%Y-%m"
  } else if (granularity == 'weeks') {
    interval_criteria <- "%Y-%V"
  }
  else if (granularity == 'days') {
    interval_criteria <- "%Y-%m-%d"
  }
  else {
    interval_criteria <- "%Y-%m-%d-%H"
  }
  return(interval_criteria)

}

format_month_year <- function(month_year_name) {
  human_readable_date <- unlist(str_split(month_year_name, "-"))
  return (paste(month.abb[strtoi(human_readable_date[2], base = 10)],
                human_readable_date[1]))

}

get_lower_month_str <- function(date) {
  return(str_to_lower(format(as.POSIXct(date), "%B")))
}


set_extreme_vals_to_zero <- function (data) {
  data[sapply(data, is.na)] <- 0
  data[sapply(data, is.nan)] <- 0
  data[sapply(data, is.infinite)] <- 0
  return(data)
}

format_start_end <- function(date_) {
  date_ <- substr(date_, 1, 10)
  tokens <- strsplit(date_, "-", fixed = TRUE)
  return(paste(tokens[[1]][3], tokens[[1]][2], tokens[[1]][1], sep = "-"))
}
