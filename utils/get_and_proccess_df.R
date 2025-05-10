###--------------------------------------------------------------------------------------------------------------------------------
### process_all_matches(): 
###--------------------------------------------------------------------------------------------------------------------------------
process_all_matches <- function(competition = "FIFA World Cup", year = "2022", type = "Pass", save_rds = TRUE) {
  # Get match data and make a summary list to store all data
  matches <- get_matches(competition, year)        
  all_team_edge_list <- list()                     
  # Process each match and get team names
  for (i in 1:nrow(matches)) {
    cat(sprintf("Processing match %d of %d...\n", i, nrow(matches)))
    match_data <- get.matchFree(matches[i,])       # function from StatsBombR to get match data
    team_names <- unique(match_data$team.name)     
    # Process each team
    for (team_name in team_names) {
      cat(sprintf("  Processing team: %s\n", team_name))
      match_id <- i
      team_name_formatted <- gsub("[^a-zA-Z0-9]", "_", team_name)  # get row number (specific match) and team name to store file
      team_data <- process_type(match_data, team_name, type)       # make df of passing data
      # Save CSV
      #csv_filename <- sprintf("results/passing_df/match_%s_%s_edge_list.csv", match_id, team_name_formatted)
      #write.csv(team_data$edge_list, csv_filename)
      # Save RDS if requested
      if (save_rds) {
        rds_edge_list <- sprintf("results/rds/edge_list/match_%s_%s_edge_list.rds", match_id, team_name_formatted)
        saveRDS(team_data$edge_list, rds_edge_list)
        rds_filtered <- sprintf("results/rds/filtered/match_%s_%s_filtered.rds", match_id, team_name_formatted)
        saveRDS(team_data$filtered, rds_filtered)
      }
      # Store in list with a unique key combining match_id and team_name
      key <- sprintf("match_%s_%s", match_id, team_name_formatted)
      all_team_edge_list[[key]] <- team_data$edge_list
    }
  }
  # Save the complete list of all team summaries
  if (save_rds) {
    saveRDS(all_team_edge_list, sprintf("results/rds/edge_list/all_team_edge_list.rds"))
  }
  cat("All matches processed successfully!\n")
}

###--------------------------------------------------------------------------------------------------------------------------------
### get_matches(): returns all the matches of the passed competition at year specified
###--------------------------------------------------------------------------------------------------------------------------------
get_matches <- function(competition_name, year) {
  comp <- FreeCompetitions()
  matches <- FreeMatches(comp)
  matches <- matches %>%
    filter(grepl(competition_name, competition.competition_name)) %>%
    filter(grepl(year, season.season_name))
  return(matches)
}

###--------------------------------------------------------------------------------------------------------------------------------
### process_type(): 
###--------------------------------------------------------------------------------------------------------------------------------
process_type <- function(match_data, team_name, type = "Pass") {
  team_type <- match_data %>%
    filter(grepl(team_name, team.name)) %>%
    filter(grepl(type, type.name)) %>%
    dplyr::select(period, timestamp, player.id, player.name, position.name, pass.recipient.id, pass.recipient.name) %>%
    na.omit()
  team_edge_list <- team_type %>%
    group_by(player.id, player.name, pass.recipient.id, pass.recipient.name) %>%
    summarise(pass_count = n(), .groups = "drop") %>%
    na.omit() %>%
    mutate(pass_count = as.numeric(pass_count),
           frequency = pass_count/sum(pass_count))
  return(list(
    filtered  = team_type,
    edge_list = team_edge_list
  ))
}

###--------------------------------------------------------------------------------------------------------------------------------
### read_all_rds(): Read all RDS files in a directory and store them as named df 
###--------------------------------------------------------------------------------------------------------------------------------
read_all_rds <- function(type = "edge_list") {
  if(type == "edge_list"){
    file_path = "results/rds/edge_list/"
  }
  else if(type == "filtered"){
    file_path = "results/rds/filtered/"
  }
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