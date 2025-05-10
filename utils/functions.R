##############################################################
########## Helper function to get World Cup matches ##########
##############################################################
get_wc_matches <- function(competition_name, year) {
  comp <- FreeCompetitions()
  matches <- FreeMatches(comp)
  wc_matches <- matches %>%
    filter(grepl(competition_name, competition.competition_name)) %>%
    filter(grepl(year, season.season_name))
  return(wc_matches)
}

###########################################################################
########## Helper function to process data for a team in a match ##########
###########################################################################
process_team_passes <- function(match_data, team_name) {
  team_passes <- match_data %>%
    filter(grepl(team_name, team.name)) %>%
    filter(grepl("Pass", type.name)) %>%
    dplyr::select(period, timestamp, player.id, player.name, position.name, pass.recipient.id, pass.recipient.name) 
  team_summary <- team_passes %>%
    group_by(player.id, player.name, pass.recipient.id, pass.recipient.name) %>%
    summarise(pass_count = n(), .groups = "drop") %>%
    na.omit() %>%
    mutate(weight = pass_count/sum(pass_count))
  return(list(
    passes = team_passes,
    summary = team_summary
  ))
}

##################################################################
########## Return data for all matches of a competition ##########
##################################################################
process_all_matches <- function(competition = "FIFA World Cup", year = "2022", save_rds = TRUE) {
  wc_matches <- get_wc_matches(competition, year)  # get data for all matches
  all_team_summaries <- list()                     # make a list to store all team summaries
  # Process each match
  for (i in 1:nrow(wc_matches)) {
    cat(sprintf("Processing match %d of %d...\n", i, nrow(wc_matches)))
    match_data <- get.matchFree(wc_matches[i,])    # get match data
    team_names <- unique(match_data$team.name)     # get names of teams in match
    # Process each team
    for (team_name in team_names) {
      cat(sprintf("  Processing team: %s\n", team_name))
      match_id <- i
      team_name_formatted <- gsub("[^a-zA-Z0-9]", "_", team_name)  # get row number (specific match) and team name to store file
      team_data <- process_team_passes(match_data, team_name)      # make df of passing data
      # Save CSV
      csv_filename <- sprintf("results/passing_df/match_%s_%s_passes.csv", match_id, team_name_formatted)
      write.csv(team_data$summary, csv_filename)
      # Save RDS if requested
      if (save_rds) {
        rds_filename <- sprintf("results/rds/match_%s_%s_passes.rds", match_id, team_name_formatted)
        saveRDS(team_data$summary, rds_filename)
      }
      # Store in list with a unique key combining match_id and team_name
      key <- sprintf("match_%s_%s", match_id, team_name_formatted)
      all_team_summaries[[key]] <- team_data$summary
    }
  }
  # Save the complete list of all team summaries
  if (save_rds) {
    saveRDS(all_team_summaries, sprintf("results/rds/all_team_summaries.rds"))
  }
  cat("All matches processed successfully!\n")
}

##################################################################################
########## Read all RDS files in a directory and store them as named df ##########
##################################################################################
read_all_rds_files <- function(file_path = "results/rds/") {
  rds_files <- list.files(path = file_path, pattern = "\\.rds$", full.names = TRUE)
  if (length(rds_files) == 0) {
    warning("No RDS files found in the specified directory.")
    return(invisible(NULL))
  }
  result_list <- list() # list to store results
  # Process each RDS file
  for (file in rds_files) {
    base_name <- tools::file_path_sans_ext(basename(file)) # extract name without .rds extension
    df <- readRDS(file)                                    # read .rds file
    result_list[[base_name]] <- df                         # store in results_list with name as key
    
    # Also assign to the global environment if needed
    # (commented out by default as it's generally not recommended practice)
    assign(base_name, df, envir = .GlobalEnv)
    
    cat(sprintf("Loaded: %s\n", base_name))
  }
  cat(sprintf("Loaded %d RDS files successfully.\n", length(rds_files)))
  return(result_list)
}

# Function to create a directed weighted network from passing data
create_pass_network <- function(pass_data, use_ids = FALSE, plot_network = FALSE, 
                                layout_type = "lgl", node_size_factor = 5, edge_width_factor = 5) {
  # Define edges: determine whether to use player IDs or names.
  if (use_ids) {
    edges <- pass_data[, c("player.id", "pass.recipient.id", "weight")]
    colnames(edges) <- c("from", "to", "weight")
  } else {
    edges <- pass_data[, c("player.name", "pass.recipient.name", "weight")]
    colnames(edges) <- c("from", "to", "weight")
  }
  g <- graph_from_data_frame(edges, directed = TRUE) # make graph from edge list
  # Calculate node metrics
  V(g)$in_strength <- strength(g, mode = "in", weights = E(g)$weight)
  V(g)$out_strength <- strength(g, mode = "out", weights = E(g)$weight)
  V(g)$total_strength <- strength(g, mode = "total", weights = E(g)$weight)
  # Layout for plotting
  if (plot_network) {
    if (layout_type == "fr") {
      layout <- layout_with_fr(g)
    } else if (layout_type == "kk") {
      layout <- layout_with_kk(g)
    } else if (layout_type == "circle") {
      layout <- layout_in_circle(g)
    } else if (layout_type == "lgl") {
      layout <- layout_with_lgl(g)
    } else {
      layout <- layout_nicely(g)
    }
    # Plot
    plot(g, 
         layout = layout,
         #vertex.size = V(g)$total_strength * node_size_factor,
         vertex.size = node_size_factor,
         vertex.label.cex = 0.8,
         vertex.label.dist = 0.5,
         vertex.label.color = "black",
         edge.width = E(g)$weight * edge_width_factor,
         edge.arrow.size = 0.005,
         edge.curved = 0.2,
         main = "Passing Network")
  }
  return(g)
}

#################################################################################
#################### Create networks for all teams in a list ####################
#################################################################################
create_all_networks <- function(all_teams_data, use_ids = FALSE) {
  # Create a list to store all networks
  all_networks <- list()
  # Process each team dataframe
  for (team_name in names(all_teams_data)) {
    # Skip all_team_summaries if it exists in the list
    if (team_name == "all_team_summaries") next
    # Create the network
    team_network <- create_pass_network(all_teams_data[[team_name]], use_ids = use_ids)
    # Store in the list
    all_networks[[team_name]] <- team_network
    cat(sprintf("Created network for: %s\n", team_name))
  }
  cat(sprintf("Created %d team networks successfully.\n", length(all_networks)))
  # Return the list of networks
  return(all_networks)
}
###################################################################################
############## Extract comprehensive network statistics for all teams #############
###################################################################################
extract_network_stats <- function(network_list, save_csv = TRUE, file_path = "results/network_stats.csv") {
  # Initialize an empty dataframe to store results
  network_stats <- data.frame()
  # Process each network
  for (team_name in names(network_list)) {
    g <- network_list[[team_name]]
    # Skip if not a valid graph
    if (!is.igraph(g)) {
      warning(paste("Skipping", team_name, "- not a valid igraph object"))
      next
    }
    # Extract team name and match ID from the full string
    match_id <- as.numeric(gsub("match_(\\d+)_.+", "\\1", team_name))
    short_name <- gsub("match_\\d+_(.+)_passes", "\\1", team_name)
    # Calculate network metrics
    n_nodes <- vcount(g)
    n_edges <- ecount(g)
    density <- edge_density(g)
    reciprocity <- reciprocity(g)
    transitivity <- transitivity(g, type = "global")
    # Handle cases where certain metrics might fail
    tryCatch({
      diameter <- diameter(g, weights = NA)
    }, error = function(e) {
      diameter <- NA
    })
    # Calculate centralization metrics
    degree_cent <- centralization.degree(g)$centralization
    betweenness_cent <- centralization.betweenness(g)$centralization
    tryCatch({
      closeness_cent <- centralization.closeness(g)$centralization
    }, error = function(e) {
      closeness_cent <- NA
    })
    # Calculate average metrics
    avg_degree <- mean(degree(g))
    avg_weighted_degree <- mean(strength(g, weights = E(g)$weight))
    avg_path_length <- mean_distance(g, directed = TRUE)
    # Get edge weight statistics
    edge_weights <- E(g)$weight
    total_weight <- sum(edge_weights)
    mean_weight <- mean(edge_weights)
    median_weight <- median(edge_weights)
    max_weight <- max(edge_weights)
    # Calculate node-level metrics and find key players
    page_ranks <- page_rank(g)$vector
    top_player_pagerank <- names(page_ranks)[which.max(page_ranks)]
    pagerank_value <- max(page_ranks)
    betweenness_values <- betweenness(g)
    top_player_betweenness <- names(betweenness_values)[which.max(betweenness_values)]
    betweenness_value <- max(betweenness_values)
    # Add to results dataframe
    team_stats <- data.frame(
      match_id = match_id,
      team_name = short_name,
      nodes = n_nodes,
      edges = n_edges,
      density = density,
      reciprocity = reciprocity,
      transitivity = transitivity,
      diameter = diameter,
      avg_degree = avg_degree,
      avg_weighted_degree = avg_weighted_degree,
      avg_path_length = avg_path_length,
      degree_centralization = degree_cent,
      betweenness_centralization = betweenness_cent,
      closeness_centralization = closeness_cent,
      total_passes = total_weight,
      mean_pass_weight = mean_weight,
      median_pass_weight = median_weight,
      max_pass_weight = max_weight,
      top_player_pagerank = top_player_pagerank,
      pagerank_value = pagerank_value,
      top_player_betweenness = top_player_betweenness,
      betweenness_value = betweenness_value,
      stringsAsFactors = FALSE
    )
    # Bind to the main results dataframe
    network_stats <- rbind(network_stats, team_stats)
  }
  # Sort by match_id and team_name
  network_stats <- network_stats[order(network_stats$match_id, network_stats$team_name), ]
  # Save as CSV if requested
  if (save_csv && nrow(network_stats) > 0) {
    # Create directory if it doesn't exist
    dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
    write.csv(network_stats, file = file_path, row.names = FALSE)
    cat(sprintf("Network statistics saved to: %s\n", file_path))
  }
  return(network_stats)
}

##############################################################################
###################### Analyze a single network ##############################
##############################################################################
analyze_network <- function(g, print_results = TRUE) {
  # Network properties
  n_nodes <- vcount(g)
  n_edges <- ecount(g)
  density <- edge_density(g)
  reciprocity <- reciprocity(g)
  transitivity <- transitivity(g)
  diameter <- diameter(g)
  # Centrality measures
  degree_cent <- degree(g)
  in_degree_cent <- degree(g, mode = "in")
  out_degree_cent <- degree(g, mode = "out")
  betweenness_cent <- betweenness(g)
  closeness_cent <- closeness(g)
  eigen_cent <- eigen_centrality(g)$vector
  page_rank <- page_rank(g)$vector
  # Node metrics: store in df
  node_metrics <- data.frame(
    node = V(g)$name,
    degree = degree_cent,
    in_degree = in_degree_cent,
    out_degree = out_degree_cent,
    betweenness = betweenness_cent,
    closeness = closeness_cent,
    eigen_centrality = eigen_cent,
    page_rank = page_rank,
    in_strength = V(g)$in_strength,
    out_strength = V(g)$out_strength,
    total_strength = V(g)$total_strength,
    stringsAsFactors = FALSE
  )
  # Order by total strength (descending)
  node_metrics <- node_metrics[order(-node_metrics$total_strength), ]
  # Print results if requested
  if (print_results) {
    cat("Network Summary:\n")
    cat(sprintf("Number of nodes (players): %d\n", n_nodes))
    cat(sprintf("Number of edges (passes): %d\n", n_edges))
    cat(sprintf("Network density: %.4f\n", density))
    cat(sprintf("Reciprocity: %.4f\n", reciprocity))
    cat(sprintf("Transitivity (clustering): %.4f\n", transitivity))
    cat(sprintf("Diameter: %.4f\n", diameter))
    cat("\nTop 5 players by total passing strength:\n")
    print(head(node_metrics[, c("node", "total_strength", "in_strength", "out_strength")], 5))
    cat("\nTop 5 players by PageRank centrality:\n")
    print(head(node_metrics[order(-node_metrics$page_rank), c("node", "page_rank")], 5))
  }
  # Return a list with all results
  results <- list(
    summary = list(
      n_nodes = n_nodes,
      n_edges = n_edges,
      density = density,
      reciprocity = reciprocity,
      transitivity = transitivity,
      diameter = diameter
    ),
    node_metrics = node_metrics
  )
  return(results)
}

##############################################################################
############# Average team statistics for all competition ####################
##############################################################################
aggregate_network_calc <- function(data) {
aggregated_network_stats <- data %>%
  mutate(team_name = sub("match_\\d+_(.*)", "\\1", team_name)) %>%
  group_by(team_name) %>%
  summarize(
    matches = n(),
    players = mean(nodes, na.rm = TRUE),
    passes = mean(edges, na.rm = TRUE),
    density = mean(density, na.rm = TRUE),
    transitivity = mean(transitivity, na.rm = TRUE),
    mean_degree = mean(avg_degree, na.rm = TRUE),
    mean_strenght = mean(avg_weighted_degree, na.rm = TRUE),
    diameter = mean(diameter, na.rm = TRUE),
    reciprocity = mean(reciprocity, na.rm = TRUE),
    median_strenght = mean(median_pass_weight, na.rm = TRUE),
    top_player = names(which.max(table(top_player_pagerank))), # Find the mode (most frequent player)
    .groups = "drop"
  )
return(aggregated_network_stats)
}

##############################################################################
################# Plot two variables + color variable ########################
##############################################################################
plot_three_variables <- function(df, x_var, y_var, color_var) {
  # Convert variable names to strings if they aren't already
  x_var_name <- deparse(substitute(x_var))
  y_var_name <- deparse(substitute(y_var))
  color_var_name <- deparse(substitute(color_var))
  
  # Create scatter plot with trend line
  ggplot(data = df, aes(x = .data[[x_var_name]], 
                        y = .data[[y_var_name]], 
                        color = .data[[color_var_name]])) +
    # Add scatter points
    geom_point(size = 3, alpha = 0.8) +
    # Add trend line
    geom_smooth(method = "lm", formula = y ~ x, color = "darkgrey", 
                se = TRUE, linetype = "dashed") +
    # Add text labels for points (assuming team_name exists)
    geom_text(aes(label = team_name), hjust = -0.2, vjust = 0.5, 
              size = 3, check_overlap = TRUE) +
    # Set color scale
    scale_color_viridis_c(name = tools::toTitleCase(color_var_name), option = "plasma") +
    # Add proper labels based on function arguments
    labs(
      x = tools::toTitleCase(x_var_name),
      y = tools::toTitleCase(y_var_name)
    ) +
    # Apply clean theme
    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
}