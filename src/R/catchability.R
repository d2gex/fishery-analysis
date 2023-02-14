library(R6)
library(dplyr)


calculate_100_area <- function(haul_distance,
                               door_distance,
                               wing_distance) {
  total_area <- door_distance * haul_distance # outer rectangle
  wing_area <- wing_distance * haul_distance # inner rectangle
  flank_areat_a_100 <-
    (total_area - wing_area) / 4 # 1/4 of outer - inner
  total_100_area <- wing_area + flank_areat_a_100
  return (round (total_100_area, 3))
  
}

calculate_abundance <- function(area, num_fish, to_km = FALSE) {
  m2_in_km2 <- 10 ** 6
  if (to_km) {
    abundance <- (num_fish / area) * m2_in_km2
  }
  else {
    abundance <- (num_fish / area)
  }
  return (round(abundance, 4))
}

select_working_data <-
  function(data,
           keep_cols,
           years) {
    dfs <- lapply(years, function (year) {
      df_common_cols <-
        data %>%
        filter(Year == year) %>%
        select(all_of(keep_cols))
      
      
      df_num_fish <-
        data %>%
        filter(Year == year) %>%
        group_by(haul.id) %>%
        summarise(num_fish = sum(HLNoAtLngt), .groups = 'drop')
      
      return (unique(merge(
        df_common_cols,
        df_num_fish,
        by = c('haul.id')
      )))
    })
    return (dfs)
  }

calculate_abundance_by_haul_km2 <- function(data, wing_distance) {
  m2_in_km2 <- 10 ** 6
  data <-
    data %>%
    mutate(area_100_m2 = calculate_100_area(Distance, DoorSpread, wing_distance)) %>%
    mutate(area_100_km2 = round(area_100_m2 / m2_in_km2, 4)) %>%
    mutate(abundance_km2 = calculate_abundance(area_100_m2, num_fish, to_km = TRUE)) %>%
    select (haul.id,
            StatRec,
            HaulLat,
            HaulLong,
            num_fish,
            area_100_km2,
            abundance_km2) %>%
    arrange(-abundance_km2) %>%
    return(data)
}

calculate_abundance_and_num_hauls_by_ices_km2 <- function(data) {
  # Calculate the total abundance per ices square
  ices_summary <- data %>% 
    group_by(StatRec) %>%
    summarise(
      num_hauls = n(),
      total_area = sum(area_100_km2),
      total_abundance = sum(abundance_km2) / num_hauls
    )
  
  # Get the atitude and longitude as the first row from each long-lat-statrec group
  data <-
    data %>%
    select (StatRec, HaulLat, HaulLong) %>%
    group_by(StatRec) %>%
    filter(row_number() == 1)
  
  return (
    merge(
      data,
      ices_summary,
      by = c('StatRec'),
      all.y = TRUE,
      all.x = FALSE
    ) %>%
      arrange(-total_abundance)
  )
}
