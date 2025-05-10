###--------------------------------------------------------------------------------------------------------------------------------
### create_all_pass_networks(): 
###--------------------------------------------------------------------------------------------------------------------------------
create_all_pass_networks <- function(all_teams_edge_list, use_ids = FALSE, weight = "pass_count") {
  # Create a list to store all networks
  all_pass_networks <- list()
  # Process each team dataframe
  for (team_name in names(all_teams_edge_list)) {
    team_pass_network <- create_pass_network(all_teams_edge_list[[team_name]], use_ids = use_ids, weight = weight)
    # Store in the list
    all_pass_networks[[team_name]] <- team_pass_network
    cat(sprintf("Created network for: %s\n", team_name))
  }
  cat(sprintf("Created %d team networks successfully.\n", length(all_pass_networks)))
  # Return the list of networks
  return(all_pass_networks)
}

###--------------------------------------------------------------------------------------------------------------------------------
### create_pass_network(): 
###--------------------------------------------------------------------------------------------------------------------------------
create_pass_network <- function(pass_data, use_ids = FALSE, weight = "pass_count") {
  # Define edges: determine whether to use player IDs or names.
  if (use_ids) {
    edges <- pass_data[, c("player.id", "pass.recipient.id", weight)]
    colnames(edges) <- c("from", "to", "weight")
  } else {
    edges <- pass_data[, c("player.name", "pass.recipient.name", weight)]
    colnames(edges) <- c("from", "to", "weight")
  }
  # Create graph. Use simplify to to remove multi-edges
  g <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL)
  #g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, 
                #edge.attr.comb = list(weight = "sum", "ignore"))
  
  # Calculate node metrics
  V(g)$in_strength <- strength(g, mode = "in", weights = E(g)$weight)
  V(g)$out_strength <- strength(g, mode = "out", weights = E(g)$weight)
  V(g)$total_strength <- strength(g, mode = "total", weights = E(g)$weight)
  return(g)
}