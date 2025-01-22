extract_players_from_team <- function(played_minutes, team_name) {
  players_of_team <- played_minutes |>
    dplyr::filter(team == team_name, minutes > 10) |>
    dplyr::pull(player) |>
    unique()
  return(players_of_team)
}
