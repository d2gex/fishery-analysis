source("R/input_data.R")
# Generate Loops plot
only_loops_g <- build_graph_matrix(
  data,
  loop_or_transition = TRUE,
  keep_duplicated_edges = TRUE,
  keep_columns = c('ind_name', 'date_time')
) %>%
  select(from, ind_name, date_time) %>%
  rename(station = from) %>%
  mutate(zone = case_when(
    grepl("M", station, fixed = TRUE) ~ 'M',
    grepl("G", station, fixed = TRUE) ~ 'G',
    grepl("C", station, fixed = TRUE) ~ 'C',
    grepl("B", station, fixed = TRUE) ~ 'B',
  )) %>%
  group_by(zone) %>%
  summarise(detections = n()) %>%
  mutate(percentage = round ((detections / sum(detections)) * 100, 2)) %>%
  arrange(desc(percentage)) %>%
  mutate(cum = cumsum(percentage))


# Generate transition plots

no_loops_g <- build_graph_matrix(
  data,
  loop_or_transition = FALSE,
  keep_duplicated_edges = TRUE,
  keep_columns = c('ind_name', 'date_time')
) %>%
  select(from, to, ind_name, date_time) %>%
  mutate(
    zone_interaction = case_when(
      # Medes
      grepl("M", from, fixed = TRUE) &
        grepl("M", to, fixed = TRUE) ~ 'M-M',
      grepl("M", from, fixed = TRUE) &
        grepl("G", to, fixed = TRUE) ~ 'M-G',
      grepl("G", from, fixed = TRUE) &
        grepl("M", to, fixed = TRUE) ~ 'M-G',
      grepl("M", from, fixed = TRUE) &
        grepl("C", to, fixed = TRUE) ~ 'M-C',
      grepl("C", from, fixed = TRUE) &
        grepl("M", to, fixed = TRUE) ~ 'M-C',
      grepl("M", from, fixed = TRUE) &
        grepl("B", to, fixed = TRUE) ~ 'M-B',
      grepl("B", from, fixed = TRUE) &
        grepl("M", to, fixed = TRUE) ~ 'M-B',

      # G
      grepl("G", from, fixed = TRUE) &
        grepl("G", to, fixed = TRUE) ~ 'G-G',
      grepl("G", from, fixed = TRUE) &
        grepl("C", to, fixed = TRUE) ~ 'G-C',
      grepl("C", from, fixed = TRUE) &
        grepl("G", to, fixed = TRUE) ~ 'G-C',
      grepl("G", from, fixed = TRUE) &
        grepl("B", to, fixed = TRUE) ~ 'G-B',
      grepl("B", from, fixed = TRUE) &
        grepl("B", to, fixed = TRUE) ~ 'G-B',

      # C
      grepl("C", from, fixed = TRUE) &
        grepl("C", to, fixed = TRUE) ~ 'C-C',
      grepl("C", from, fixed = TRUE) &
        grepl("B", to, fixed = TRUE) ~ 'C-B',
      grepl("B", from, fixed = TRUE) &
        grepl("C", to, fixed = TRUE) ~ 'C-B',

      # B
      grepl("B", from, fixed = TRUE) &
        grepl("B", to, fixed = TRUE) ~ 'B-B'
    )
  ) %>%
  group_by(zone_interaction) %>%
  summarise(detections = n()) %>%
  mutate(percentage = round ((detections / sum(detections)) * 100, 2)) %>%
  arrange(desc(percentage)) %>%
  mutate(cum = cumsum(percentage))

#-------------------------------------------
# Draw sustain detections line plot
#-------------------------------------------

detections <- slide_data_by_date_interval (data,
                                           start,
                                           end,
                                           granularity = 'months',
                                           as_list = FALSE)

detections <- detections %>%
  select(period) %>%
  group_by(period) %>%
  summarise(detections = n()) %>%
  arrange(period)

detections$month_order = 1:nrow(detections)
detections <- detections %>%
  mutate(percentage = round ((detections / sum(detections)) * 100, 2)) %>%
  arrange(desc(percentage)) %>%
  mutate(cum = cumsum(percentage))

det_to_represent <- detections[1:nrow(detections) - 1, ] %>%
  mutate(months =
           paste(str_sub(period, 3, 4),
                 month.abb[as.numeric(as.character(str_sub(period, 6, 7)))],
                 sep = "-"))


detections_time_plot <-
  ggplot(det_to_represent, aes(x = reorder(months, month_order),
                               y = detections,
                               group = 1)) +
  ggtitle("B) Monthly detections") +
  geom_line(color = "lightblue") +
  geom_point(color = "violetred") +
  theme_bw() +
  xlab("Months") +
  ylab("Num Detections") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(size = 8, hjust = 0.5, face = "bold")
  )

#-----------------------------------------------------
# Draw residency and transition graphs by zones
#----------------------------------------------------
# --> Get coordinates of representative marine reserves
zone_stations <- locs %>%
  filter(station == 'M08' |
           station == 'G06' |
           station == 'C18' |
           station == 'B17') %>%
  mutate(station = str_replace(station, "M08", "M")) %>%
  mutate(station = str_replace(station, "G06", "G")) %>%
  mutate(station = str_replace(station, "C18", "C")) %>%
  mutate(station = str_replace(station, "B17", "B"))

# --> get % for node size for loops and within station movements

node_size_df <- no_loops_g %>%
  filter(
    if_any(zone_interaction,  ~ str_detect(., "M-M")) |
      if_any(zone_interaction,  ~ str_detect(., "G-G")) |
      if_any(zone_interaction,  ~ str_detect(., "C-C")) |
      if_any(zone_interaction,  ~ str_detect(., "B-B"))
  ) %>%
  mutate(zone_interaction = str_replace(zone_interaction, "M-M", "M")) %>%
  mutate(zone_interaction = str_replace(zone_interaction, "G-G", "G")) %>%
  mutate(zone_interaction = str_replace(zone_interaction, "C-C", "C")) %>%
  mutate(zone_interaction = str_replace(zone_interaction, "B-B", "B")) %>%
  select(zone_interaction, detections) %>%
  rename(zone = zone_interaction)

node_size_df[nrow(node_size_df) + 1,] = list("B", 1)

# Merge loops and within movements
zone_loops <- only_loops_g %>% select(zone, detections)
node_size_df <- merge(node_size_df, zone_loops, all = TRUE)

# Calculate % in the dataframe
node_size_df <- node_size_df %>%
  group_by(zone) %>%
  summarise(detections = sum(detections)) %>%
  mutate(percentage = round ((detections / sum(detections)) * 100, 2)) %>%
  arrange(desc(percentage)) %>%
  mutate(cum = cumsum(percentage))

# --> get % edges weight
edge_weight_df <- no_loops_g %>%
  filter(
    !if_any(zone_interaction,  ~ str_detect(., "M-M")) &
      !if_any(zone_interaction,  ~ str_detect(., "G-G")) &
      !if_any(zone_interaction,  ~ str_detect(., "C-C")) &
      !if_any(zone_interaction,  ~ str_detect(., "B-B"))
  ) %>%
  select(zone_interaction, detections) %>%
  rename(zone = zone_interaction) %>%
  group_by(zone) %>%
  summarise(detections = sum(detections)) %>%
  mutate(percentage = round ((detections / sum(detections)) * 100, 2)) %>%
  arrange(desc(percentage)) %>%
  mutate(cum = cumsum(percentage))

# Assign node sizes and edge weights

# --> Create the mobility graph between zones in the same exact order as the
# descending edge weight order
zone_graph <- make_undirected_graph(c("M", "G", "M", "C", "G", "C",
                                      "M", "B", "C", "B", "G", "B"))

# --> Get coordinates of graph and bounding box
coords_simple_g <-
  get_graph_coordinates(zone_graph, zone_stations)
bb_simple_g <-
  adjust_coord_bb(coords_simple_g, min_half_size = 2000)
bb_simple_g$ymin[1] <- bb_simple_g$ymin[1] - 2000


V(zone_graph)$size <- node_size_df$percentage
V(zone_graph)$name <- c("Illes Medes", "Badia de Roses", "Cap de Creus", "Banyuls")
E(zone_graph)$weight <- edge_weight_df$percentage

# Draw finally the graph


zone_graph_plot <- ggraph(zone_graph,
                          layout = "manual",
                          x = coords_simple_g$X,
                          y = coords_simple_g$Y) +

  ggtitle("A) (Within-zone and loops) and between-zone movements") +
  geom_sf(data = coast, col = NA) +
  geom_sf(data = river, col = "#38afcd") +
  geom_sf(data = river_muga, col = "#38afcd") +
  geom_sf(data = port, col = "black") +


  coord_sf(
    xlim = c(bb_simple_g$xmin, bb_simple_g$xmax),
    ylim = c(bb_simple_g$ymin, bb_simple_g$ymax)
  ) +

  geom_edge_link(
    color = '#F8C471',
    aes(
      width = E(zone_graph)$weight / 5,
      label = paste(E(zone_graph)$weight, "%")
    ),
    angle_calc = "along",
    label_push = unit(5, "mm")
  ) +

  # Node settings
  # --> We need a legend
  geom_node_point(aes(colour = V(zone_graph)$name),
                  size = V(zone_graph)$size / 6) +

  geom_node_label(
    aes(
      colour = V(zone_graph)$name,
      label = paste0(V(zone_graph)$name,
                     " - ",
                     V(zone_graph)$size,
                     "%")
    ),
    label.size = 0.15,
    size = 3,
    nudge_x = 4,
    nudge_y = -3,
    repel = TRUE,
    max.overlaps = Inf,
    box.padding = unit(1.5, "lines")
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
    plot.title = element_text(size = 8, hjust = 0.5, face = "bold")
  ) +
  xlab(NULL) +
  ylab(NULL)


to_path <-
  file.path(OUTPUT,
            paste("presentation_connectivity.pdf",
                  sep = ""))

# plots_to_pdf(list(zone_graph_plot),
#              to_path,
#              paper_type,
#              paper_height,
#              paper_width)

