source("R/utils.R")
library(dplyr)

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
      return (data_views)
    }

    return(bind_rows(data_views,  .id = "period"))

  }
