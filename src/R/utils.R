source("R/config.R")

prepare_gis_receiver_locations <- function(locations_path, coast) {
    #'Read the locations of all hydrophones and set up projection
    #'@param locations_path A string
    #'@return A dataframe

  locs <- read.csv2(locations_path)
  locs <- locs %>%
    dplyr::select(station, Longitude, Latitude) %>%
    filter(!duplicated(station)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"),
             crs = RECEIVER_COOR_CRS) %>%   # from EPSG:4326 (longlat) ...
    st_transform(locs, crs = st_crs(coast))     # to UTM 31N / WGS84
  return(locs)
}

fetch_species <- function(data, id_species) {
    #' Fetches all rows for a particular species
    #' @param data A dataframe
    #' @param id_species A string
    #' @return A dataframe
  return(data %>%
           filter(startsWith(ind_name, id_species)))
}

fetch_individual <- function(data, id_individual) {
    #' Fetches all rows for an individual from a given dataframe
    #' @param data A dataframe
    #' @param id_individual A string
    #' @return A dataframe
  return(data %>% filter(ind_name == id_individual))
}

fetch_individuals <- function(data, id_indivudals, grouped = TRUE) {
    #' If grouped == True, it returns a dataframe that contains only the passed
    #' individual. Otherwise a named list of dataframes where each item in the list
    #' represent an individual
  if (grouped) {
    return(data %>% filter(ind_name %in% id_indivudals))
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

adjust_coord_bb <- function(coords, min_half_size) {
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

get_graph_coordinates <- function(g, locs) {
  graph_order_node_names <- vertex_attr(g)$name
  coords <-
    data.frame(station = locs$station, st_coordinates(locs)) %>%
      filter(station %in% graph_order_node_names)
  coords <- coords[match(graph_order_node_names, coords$station),]
  return(coords)

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
  return(paste(month.abb[strtoi(human_readable_date[2], base = 10)],
               human_readable_date[1]))

}

get_lower_month_str <- function(date) {
  return(str_to_lower(format(as.POSIXct(date), "%B")))
}


set_extreme_vals_to_zero <- function(data) {
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

slide_data_by_date_interval <-
  function(data,
           start,
           end,
           granularity = 'hours',
           as_list = TRUE) {
        #' @description Slice a dataframe into a list with as many dataframes as the granularity
        #' dictates
        #' @param data dataframe with a 'date_time' column
        #' @param start string YYYY-dd-mm HH:mm:SS
        #' @param end  string YYYY-dd-mm HH:mm:SS
        #' @param granularity string that takes values: months, weeks, days or hours
        #' @return a list of dataframes

    # Narrow down the interval
    data = data %>%
      dplyr::filter(date_time >= start & date_time <= end)

    # filter by the right criteria_interval
    interval_criteria <- get_interval_date_criteria(granularity)
    # Get unique interval criteria
    data <- data %>%
      dplyr::mutate(date_interval = format(as.POSIXct(data$date_time), interval_criteria))
    interval_criteria <- unique(data$date_interval)

    # Generate list of dataframes according to criteria
    data_views <- lapply(interval_criteria, function(x) {
      data %>%
        dplyr::filter(date_interval == x) %>%
        select(-date_interval)
    })

    names(data_views) <- interval_criteria

    if (as_list) {
      return(data_views)
    }

    return(bind_rows(data_views, .id = "period"))

  }

build_graph_matrix <- function(data,
                               loop_or_transition = FALSE,
                               keep_duplicated_edges = FALSE,
                               keep_columns = NULL) {
    #' @description  Creates a dataframe ready to be interpreted by an igraph:
    #' a) loops and simple, b) loops and full, c) no_loops an simple, d) no loops
    #' and full
    #' @param data A dataframe
    #' @param loop_or_transition boolean parameter that indicates if we want an
    #' only-loop graph or only-transition-graph
    #' @param keep_duplicated_edges boolean indicating whether we keep duplicated
    #' edges or not
    #' @param keep_ind_name boolean indicating whether individual name es kept
    #' @return A single dataframe
  data <- data %>%
    mutate(from = c(NA, station[-n()]), to = c(station[-1], NA))

  columns <- c(c('from', 'to'), c(keep_columns))

  if (!loop_or_transition) {
    data <- data %>% filter(from != to)
  }
  else {
    data <- data %>% filter(from == to)
  }

  if (keep_duplicated_edges) {
    data <- data %>% select(!!columns)
  }
  else {
    data <- data %>%
      group_by_at(columns) %>%
      summarise(n = n(), .groups = "drop") %>%
      select(-n)

  }
  return(data)
}

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