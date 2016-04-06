
#' Actually get the data
#'
#' This function gets the data for nba players
#' @return A Data Frame containing the NBA Data
#' @examples
#' do_get_nba_data()
do_get_nba_data <- function() {
  rows <- c("SEASON_ID", "PLAYER_ID", "PLAYER_NAME", "TEAM_ABBREVIATION",
            "TEAM_NAME", "GAME_ID", "GAME_DATE", "MATCHUP", "WL", "MIN", "FGM",
            "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT",
            "OREB", "DREB", "REB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "PLUS_MINUS",
            "VIDEO_AVAILABLE")

  data <- jsonlite::fromJSON("https://s3-eu-west-1.amazonaws.com/nba-processed/processed-2015_16_players.json")
  colnames(data) <- rows
  data <- as.data.frame(data)

  data$GAME_DATE <- as.Date(data$GAME_DATE)

  for (i in 10:29) {
    data[,i] <- as.numeric(as.character(data[,i]))
  }

  data$points = data$PTS + 0.5*data$FG3M + 1.25*data$REB + 1.5*data$AST + 2*data$STL + 2*data$BLK - 0.5*data$TOV
  
  data
}

#' Get NBA Game Data
#'
#' This function allows you to get player by player game data from NBA Games
#' @return A Data Frame containing the NBA Data
#' @export
#' @examples
#' get_nba_data()
get_nba_data <- memoise::memoise(do_get_nba_data)

