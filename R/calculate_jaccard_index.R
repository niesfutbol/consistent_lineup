extract_players_from_team <- function(played_minutes, team_name) {
  players_of_team <- .filter_players_from_team_with_more_than_10_minutes(played_minutes, team_name) |>
    .pull_unique_players()
}

.filter_players_from_team_with_more_than_10_minutes <- function(played_minutes, team_name) {
  players_of_team <- played_minutes |>
    dplyr::filter(team == team_name, minutes > 10)
  return(players_of_team)
}

.pull_unique_players <- function(players_of_team) {
  unique_players_of_team <- players_of_team |>
    dplyr::pull(player) |>
    unique()
  return(unique_players_of_team)
}

extract_players_from_match <- function(played_minutes, match_id) {
  players_of_match <- played_minutes |>
    dplyr::filter(match == match_id, minutes > 10) |>
    .pull_unique_players()
}
