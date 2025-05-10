#------------------------------------------------------------------------------------------------------------------------
# plot_network
#------------------------------------------------------------------------------------------------------------------------
plot_network <- function(network, layout_type = "lgl", node_size_factor = 5, edge_width_factor = 100, title = title) {
  # Create the layout matrix first
  if (layout_type == "lgl") {
    layout <- igraph::layout_with_lgl(network)
  } else if (layout_type == "fr") {
    layout <- igraph::layout_with_fr(network)
  } else if (layout_type == "kk") {
    layout <- igraph::layout_with_kk(network)
  } else {
    layout <- igraph::layout_with_lgl(network)  # Default fallback
  }
  plot(network, 
       layout = layout,
       #vertex.size = V(g)$total_strength * node_size_factor,
       vertex.size = node_size_factor,
       vertex.label.cex = 0.8,
       vertex.label.dist = 0.5,
       vertex.label.color = "black",
       edge.width = E(network)$weight * edge_width_factor,
       edge.arrow.size = 0.005,
       edge.curved = 0.2,
       main = title)
  return(network)
}

###--------------------------------------------------------------------------------------------------------------------------------
### plot_three_variables()
###--------------------------------------------------------------------------------------------------------------------------------
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


###--------------------------------------------------------------------------------------------------------------------------------
### plot_predictors()
###--------------------------------------------------------------------------------------------------------------------------------
plot_predictors <- function(data, predictors, target_var, grouping_var = NULL, 
                            ncol = 2, title = "Predictors vs Target", 
                            add_correlation = TRUE) {
  
  # Create correlation matrix if requested
  if (add_correlation) {
    correlation_matrix <- cor(data[, c(predictors, target_var)])
    print(correlation_matrix)
  }
  # Create a combined data frame for all predictors
  plot_data <- data.frame()
  
  for (predictor in predictors) {
    # Extract data for each predictor
    temp_data <- data %>%
      select(all_of(c(predictor, target_var, grouping_var))) %>%
      rename(predictor_value = all_of(predictor)) %>%
      mutate(predictor_name = predictor,
             correlation = round(cor(data[[predictor]], data[[target_var]]), 3))
    
    plot_data <- rbind(plot_data, temp_data)
  }
  
  # Create faceted plot
  p <- ggplot(plot_data, aes(x = predictor_value, y = .data[[target_var]])) +
    geom_point(aes_string(color = grouping_var), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "darkgrey") +
    facet_wrap(~ predictor_name, scales = "free_x", ncol = ncol) +
    theme_minimal() +
    labs(title = title,
         x = "Predictor Value",
         y = target_var) +
    theme(legend.position = "bottom")
  
  # Add correlation values to facet labels if requested
  if (add_correlation) {
    p <- p + facet_wrap(~ paste0(predictor_name, "\nr = ", correlation), 
                        scales = "free_x", ncol = ncol)
  }
  
  return(p)
}