library(stringr)
library(dplyr)

#import chess data as txt file
chess_data <- readLines("/Users/alliewrubel/Desktop/tournamentinfo.txt")

#remove the header lines
chess_data <- chess_data[-(1:3)]
print(chess_data)


#removes the "-" lines separating the rows of player data
chess_data <- chess_data[!grepl("^-+$", chess_data)]
print(chess_data)

chess_dataFrame <- data.frame(
  PlayerID = numeric(),
  PlayerName = character(),
  State = character(),
  TotalPoints = numeric(),
  PreRating = numeric(),
  GameCount = character(),
  AvgOpponentRating = numeric(),
  stringsAsFactors = FALSE
)

#sequentially move through file and add player ID's dynamically
start_index <- 1

#capturing all player names
for (i in seq(1, length(chess_data), by = 2)) {
  
  # Get player info from the current line
  player_info <- chess_data[i]
  
  playerID <- start_index
  
  # Extract the names of players
  player_names <- paste(unlist(str_extract(player_info, "(?<=\\|)\\s*[A-Z\\s]+(?=\\s*\\|)")))
  player_names <- trimws(player_names)
  
  # Add PlayerID and PlayerName to the data frame, with placeholders for other columns
  chess_dataFrame <- rbind(chess_dataFrame, data.frame(
    PlayerID = playerID,
    PlayerName = player_names,
    State = NA,  # Placeholder
    TotalPoints = NA,  # Placeholder
    PreRating = NA,  # Placeholder
    GameCount = NA,  # Placeholder
    AvgOpponentRating = NA,  # Placeholder
    stringsAsFactors = FALSE
  ))
  
  # Increment player ID
  start_index <- start_index + 1
  
}

View(chess_dataFrame)

#loop through and extract the states
start_index <- 1

for (i in seq(1, length(chess_data),by=2)) {
  
  #split player info into lines
  player_info <- chess_data[i]
  player_rating_info <- chess_data[i+1]
  
  #extracts the states, replace NA with "na" for easier differentiation
  player_state <- paste(str_extract(player_rating_info, "^\\s*[A-Z]{2}\\b"))
  player_state <- trimws(player_state)
  
  chess_dataFrame$State[start_index] <- player_state
  start_index <- start_index + 1
  
}

View(chess_dataFrame)

#loop through and extract

start_index <- 1

for (i in seq(1, length(chess_data),by=2)) {
  
  #split player info into lines
  player_info <- chess_data[i]
  player_rating_info <- chess_data[i+1]
  
  #player total points
  total_points <- paste(unlist(str_extract_all(player_info, "(?<=|)\\d\\.\\d")))
  
  chess_dataFrame$TotalPoints[start_index] <- total_points
  start_index <- start_index + 1

}

View(chess_dataFrame)

start_index <- 1

for (i in seq(1, length(chess_data),by=2)) {
  
  #split player info into lines
  player_info <- chess_data[i]
  player_rating_info <- chess_data[i+1]
  
  #pre-ratings for players
  player_rating <- paste(unlist(str_extract_all(player_rating_info, "(?<=R:)\\s*(\\d+)(?=\\s*->|\\w)")))
  player_rating <- trimws(player_rating)
  
  chess_dataFrame$PreRating[start_index] <- as.numeric(player_rating)
  
  start_index <- start_index + 1
}

View(chess_dataFrame)

start_index <- 1

for (i in seq(1, length(chess_data),by=2)) {
  
  #split player info into lines
  player_info <- chess_data[i]
  player_rating_info <- chess_data[i+1]
  
  #game outcomes + opponent ID
  games_played <- paste(unlist(str_extract_all(player_info, "\\b(W|L|D){1}\\s*\\d+")), collapse =", ")
  
  chess_dataFrame$GameCount[start_index] <- games_played
  
  start_index <- start_index + 1
}

View(chess_dataFrame)

#calculate avg opponent rating
for(i in 1:nrow(chess_dataFrame)){
  game_count_str <- chess_dataFrame$GameCount[i]
  
  total_games <- str_count(game_count_str, "[A-Z]")
  
  opponent_ids <- as.numeric(unlist(str_extract_all(game_count_str, "\\d+")))
  print(opponent_ids)
  
  total_opponent_rating <- 0
  index <- 1
  for(opponent_id in opponent_ids){
    opponent_rating <- chess_dataFrame$PreRating[opponent_id]
    
    total_opponent_rating <- total_opponent_rating + opponent_rating
    
    cat("Opponent ID:", opponent_id, " | Rating:",opponent_rating, "\n")
  }
  
  avg_op_rating <- total_opponent_rating / total_games
  cat("Average Opponent Rating:", avg_op_rating, "\n")
  
  chess_dataFrame$AvgOpponentRating[i] = avg_op_rating
}

View(chess_dataFrame)

#subset the dataframe
selected_columns <- chess_dataFrame[, c("PlayerName", "State", "TotalPoints", "PreRating", "AvgOpponentRating")]
View(selected_columns)

#write to a csv
write.csv(selected_columns, file = "chess_tournament_selected_columns.csv", row.names = FALSE)
