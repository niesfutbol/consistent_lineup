extract_players_from_team <- function(played_minutes, team_name) {
  players_of_team <- played_minutes |>
    dplyr::filter(team == team_name, minutes > 10) |>
    dplyr::pull(player) |>
    unique()
  return(players_of_team)
}

extract_players_from_match <- function(played_minutes, match_id) {
  players_of_match <- played_minutes |>
    dplyr::filter(match == match_id, minutes > 10) |>
    dplyr::pull(player) |>
    unique()
  return(players_of_match)
}
