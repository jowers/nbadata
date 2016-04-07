#' Get a Player's previous points
#'
#' This function returns a dataframe containing the points a player scored in a match
#' alongside the points he scored in the previous x matches
#' @return A dataframe containing the points data
#' @param num_games how many games to provide the previous points for
#' @export
#' @examples
#' get_last_x_points(6)
get_last_x_points <- function(num_games) {
  df <- dplyr::arrange(get_nba_data(), PLAYER_NAME, desc(GAME_DATE))
    
  fields <- c("PLAYER_NAME", "PLAYER_ID", "GAME_DATE", "points")
  
  for (i in 1:num_games) {
    j <- i + 1
    df[,paste("PREV_PTS", i, sep="_")] <- zoo::rollapply(df$points, j, FUN=function(z) {mean(z[j:j])},fill = NA, align="left")
    df$PREV_PLAYER <- zoo::rollapply(df$PLAYER_ID, j, FUN=function(z) {z[j:j]},fill = NA, align="left")
    
    df$PLAYER_ID <- as.numeric(df$PLAYER_ID)
    df$PREV_PLAYER <- as.numeric(df$PREV_PLAYER)
    df[is.na(df$PREV_PLAYER),c("PREV_PLAYER")] <- "None"
    df[df$PLAYER_ID != df$PREV_PLAYER, paste("PREV_PTS", i, sep="_")] <- NA
    fields <- append(fields, paste("PREV_PTS", i, sep="_"))
  }
  df[,fields]
}