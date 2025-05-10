###--------------------------------------------------------------------------------------------------------------------------------
### extract_network_descriptive_stats(): 
###--------------------------------------------------------------------------------------------------------------------------------
extract_network_descriptive_stats <- function(network_list) {
  # Initialize an empty dataframe to store results
  network_stats <- data.frame()
  # Process each network
  for (team_name in names(network_list)) {
    g <- network_list[[team_name]]
    # Skip if not a valid graph
    if (!is_igraph(g)) {
      warning(paste("Skipping", team_name, "- not a valid igraph object"))
      next
    }
    # Extract team name and match ID from the full string
    match_id <- as.numeric(gsub("match_(\\d+)_.+", "\\1", team_name))
    match_team  <- gsub("match_\\d+_(.+)_passes", "\\1", team_name)
    # Number of nodes, edges and passes
    n_nodes <- vcount(g)
    n_edges <- ecount(g)
    edge_weights <- E(g)$weight
    total_passes <- sum(edge_weights) # may include: avg_passes <- mean(edge_weights), median_passes <- median(edge_weights)
    # Statistics used by Clemente et al.
    density <- edge_density(g)
    diameter <- diameter(g, directed = TRUE)
    avg_transitivity <- mean(transitivity(g, type = "local"), na.rm = TRUE)
    # Other network statistics
    reciprocity <- reciprocity(g)
    g_undirected <- as_undirected(g, mode = "collapse", edge.attr.comb = list(weight = "sum", "ignore"))
    avg_transitivity_barrat <- mean(transitivity(g_undirected, type = "barrat"), na.rm = TRUE)
    # Add to results dataframe
    team_stats <- data.frame(
      # Basic info
      match_id = match_id,
      team_name = match_team,
      nodes = n_nodes,
      edges = n_edges,
      total_passes = total_passes,
      # Statistics used by Clemente et al.
      density = density,
      diameter = diameter,
      avg_transitivity = avg_transitivity,
      # Other statistics
      avg_transitivity_barrat = avg_transitivity_barrat,
      reciprocity = reciprocity
      )
    network_stats <- rbind(network_stats, team_stats)
  }
  # Sort by match_id and team_name
  network_stats <- network_stats[order(network_stats$match_id, network_stats$team_name), ]
  return(network_stats)
}

###--------------------------------------------------------------------------------------------------------------------------------
### aggregate_network_calc(): 
###--------------------------------------------------------------------------------------------------------------------------------
aggregate_network_calc <- function(data) {
  aggregated_network_stats <- data %>%
    mutate(team_name = sub("match_\\d+_(.*)", "\\1", team_name)) %>%
    group_by(team_name) %>%
    summarize(
      # Basic info
      matches_played = n(),
      nodes = mean(nodes, na.rm = TRUE),
      edges = mean(edges, na.rm = TRUE),
      passes = mean(total_passes, na.rm = TRUE),
      # Statistics used by Clemente et al.
      density = mean(density, na.rm = TRUE),
      diameter = mean(diameter, na.rm = TRUE),
      transitivity = mean(avg_transitivity, na.rm = TRUE),
      # Other statistics
      transitivity_barrat = mean(avg_transitivity_barrat, na.rm = TRUE),
      reciprocity = mean(reciprocity, na.rm = TRUE),
      .groups = "drop"
      #mean_degree = mean(avg_degree, na.rm = TRUE),
      #mean_strenght = mean(avg_weighted_degree, na.rm = TRUE),
      #median_strenght = mean(median_pass_weight, na.rm = TRUE),
      #top_player = names(which.max(table(top_player_pagerank))), # Find the mode (most frequent player)
    )
  return(aggregated_network_stats)
}