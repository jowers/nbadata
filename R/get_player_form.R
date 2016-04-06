
#' Get Player form data
#'
#' This function allows you to get the form of all players for the period requested
#' @return A plyr object containing the form data
#' @param since date from which to look at a players form
#' @param min_games how many games should a player have in the period to be included
#' @param min_av_points what av points should a player have in the period to be included
#' @export
#' @examples
#' get_player_form("2016-03-01", 6, 20)
get_player_form <- function(since, min_games = 6, min_av_points = 20) {
  df <- get_nba_data()
  by_player <- dplyr::group_by(df[df$GAME_DATE > since,], PLAYER_NAME, TEAM_NAME)
  mins_to_points <- dplyr::summarise(by_player,
                              games = n(),
                              av_points = mean(points),
                              correlation = cor(points, MIN))
  
  form <- dplyr::summarise(by_player,
                    games = n(),
                    av_points = mean(points),
                    correlation = cor(points, as.numeric(GAME_DATE)))
  form <- form[form$games >= min_games  & form$av_points > min_av_points,]
  form
}
