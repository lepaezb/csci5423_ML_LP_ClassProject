#------------------------------------------------------------------------------------------------------------------------
# evaluate_regression_model()
#------------------------------------------------------------------------------------------------------------------------
evaluate_regression_model <- function(formula, data) {
  model <- lm(formula, data = data)
  summary_stats <- summary(model)
  
  # Extract key metrics
  r_squared <- summary_stats$r.squared
  adj_r_squared <- summary_stats$adj.r.squared
  f_stat <- summary_stats$fstatistic[1]
  p_value <- pf(summary_stats$fstatistic[1], 
                summary_stats$fstatistic[2], 
                summary_stats$fstatistic[3], 
                lower.tail = FALSE)
  
  # Display results
  cat("\nModel:", deparse(formula), "\n")
  cat("R-squared:", r_squared, "\n")
  cat("Adjusted R-squared:", adj_r_squared, "\n")
  cat("F-statistic:", f_stat, "on", summary_stats$fstatistic[2], "and", 
      summary_stats$fstatistic[3], "DF, p-value:", p_value, "\n\n")
  
  # Print coefficients with significance
  print(summary_stats$coefficients)
  
  # Return the model
  return(model)
}

#------------------------------------------------------------------------------------------------------------------------
# create_model_summary_table()
#------------------------------------------------------------------------------------------------------------------------
create_model_summary_table <- function(formula, data, output_path) {
  # Run anova and Linear regression analysis
  anova_model <- aov(formula, data = data)
  lm_model <- lm(formula, data = data)
  
  # Extract anova info
  anova_summary <- summary(anova_model)
  anova_df <- data.frame(
    Term = rownames(anova_summary[[1]]),
    F_value = anova_summary[[1]]$`F value`,
    P_value = anova_summary[[1]]$`Pr(>F)`,
    stringsAsFactors = FALSE
  )
  anova_df <- anova_df[-nrow(anova_df), ]  # Remove residuals row
  
  # Extract relevant information from linear model
  lm_summary <- summary(lm_model)
  lm_df <- data.frame(
    Term = rownames(coef(lm_summary)),
    Estimate = coef(lm_summary)[, 1],
    Std_Error = coef(lm_summary)[, 2],
    t_value = coef(lm_summary)[, 3],
    P_value = coef(lm_summary)[, 4],
    stringsAsFactors = FALSE
  )
  
  # Get overall model statistics
  r_squared <- lm_summary$r.squared
  adj_r_squared <- lm_summary$adj.r.squared
  f_statistic <- lm_summary$fstatistic[1]
  f_df1 <- lm_summary$fstatistic[2]
  f_df2 <- lm_summary$fstatistic[3]
  model_p_value <- pf(f_statistic, f_df1, f_df2, lower.tail = FALSE)
  
  # Create significance stars
  anova_df$Significance <- ifelse(anova_df$P_value < 0.001, "***",
                                  ifelse(anova_df$P_value < 0.01, "**",
                                         ifelse(anova_df$P_value < 0.05, "*",
                                                ifelse(anova_df$P_value < 0.1, ".", ""))))
  
  lm_df$Significance <- ifelse(lm_df$P_value < 0.001, "***",
                               ifelse(lm_df$P_value < 0.01, "**",
                                      ifelse(lm_df$P_value < 0.05, "*",
                                             ifelse(lm_df$P_value < 0.1, ".", ""))))
  
  # Create the comprehensive table
  # First, prepare the output
  sink(output_path)
  
  cat("## MODEL SUMMARY\n\n")
  cat("Formula:", deparse(formula), "\n\n")
  cat("R-squared:", round(r_squared, 4), "\n")
  cat("Adjusted R-squared:", round(adj_r_squared, 4), "\n")
  cat("F-statistic:", round(f_statistic, 4), "on", f_df1, "and", f_df2, "DF\n")
  cat("Model p-value:", format.pval(model_p_value, digits = 4), "\n\n")
  
  cat("## ANOVA RESULTS\n\n")
  print(anova_df, row.names = FALSE)
  
  cat("\n\n## REGRESSION COEFFICIENTS\n\n")
  # Format the numbers for better display
  lm_df_formatted <- lm_df
  lm_df_formatted$Estimate <- round(lm_df_formatted$Estimate, 4)
  lm_df_formatted$Std_Error <- round(lm_df_formatted$Std_Error, 4)
  lm_df_formatted$t_value <- round(lm_df_formatted$t_value, 4)
  lm_df_formatted$P_value <- format.pval(lm_df_formatted$P_value, digits = 4)
  print(lm_df_formatted, row.names = FALSE)
  
  cat("\n\nSignificance codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  
  sink()
}