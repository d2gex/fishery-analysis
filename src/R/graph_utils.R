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
    mutate(from = c(NA, station[-n()]), to  = c(station[-1], NA))

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
    data <- data %>% group_by_at(columns) %>%
      summarise(n = n(), .groups = "drop") %>%
      select(-n)

  }

  return(data)

}
