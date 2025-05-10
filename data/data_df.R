rankings_df <- data.frame(
  Rank = 1:32,
  Team = c("Argentina", "France", "Croatia", "Morocco", "Netherlands", "England", 
           "Brazil", "Portugal", "Japan", "Senegal", "Australia", "Switzerland", 
           "Spain", "United_States", "Poland", "South_Korea", "Germany", "Ecuador", 
           "Cameroon", "Uruguay", "Tunisia", "Mexico", "Belgium", "Ghana", 
           "Saudi_Arabia", "Iran", "Costa_Rica", "Denmark", "Serbia", "Wales", 
           "Canada", "Qatar"),
  Stage_Reached = c("Champion", "Runner-up", "Third Place", "Fourth Place", 
                    rep("Quarterfinals", 4), rep("Rd of 16", 8), rep("Groups", 16)),
  Record = c("4-1-2", "5-1-1", "2-1-4", "3-2-2", "3-0-2", "3-1-1", "3-1-1", "3-2-0",
             "2-1-1", "2-2-0", "2-2-0", "2-2-0", "1-1-2", "1-1-2", "1-2-1", "1-2-1",
             "1-1-1", "1-1-1", "1-1-1", "1-1-1", "1-1-1", "1-1-1", "1-1-1", "1-2-0",
             "1-2-0", "1-2-0", "1-2-0", "0-2-1", "0-2-1", "0-2-1", "0-3-0", "0-3-0"),
  Goal_Difference = c(7, 8, 1, 1, 6, 9, 5, 6, 1, -2, -2, -4, 6, -1, -2, -3, 1, 1, 0, 0, 0, -1, -1, -2, -2, -3, -8, -2, -3, -5, -5, -6),
  Goals_For = c(15, 16, 8, 6, 10, 13, 8, 12, 5, 5, 4, 5, 9, 3, 3, 5, 6, 4, 4, 2, 1, 2, 1, 5, 3, 4, 3, 1, 5, 1, 2, 1),
  Goals_Against = c(8, 8, 7, 5, 4, 4, 3, 6, 4, 7, 6, 9, 3, 4, 5, 8, 5, 3, 4, 2, 1, 3, 2, 7, 5, 7, 11, 3, 8, 6, 7, 7),
  FIFA_Rank = c(3, 4, 12, 22, 8, 5, 1, 9, 24, 18, 38, 15, 7, 16, 26, 28, 11, 44, 43, 14, 30, 13, 2, 61, 51, 20, 31, 10, 21, 19, 41, 50)
)
rankings_df$performance_score <- max(rankings_df$Rank) + 1 - rankings_df$Rank
rankings_df$Stage_Reached <- factor(rankings_df$Stage_Reached, 
                                levels = c("Groups", "Rd of 16", "Quarterfinals", 
                                           "Fourth Place", "Third Place", "Runner-up", "Champion"), 
                                ordered = TRUE)