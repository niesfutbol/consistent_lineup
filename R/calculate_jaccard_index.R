JaccardIndexCalculator <- R6::R6Class("JaccardIndexCalculator",
  public = list(
    data = NULL,
    data_team = NULL,
    initialize = function() {
    },
    set_data = function(data) {
      self$data <- data
    },
    set_team = function(team_name) {
      self$data_team <- .filter_players_from_team_with_more_than_10_minutes(self$data, team_name)
    },
    pull_unique_players = function() {
      .pull_unique_players(self$data_team)
    },
    pull_players_of_match = function(match_id) {
      players_of_match <- self$data_team |>
        dplyr::filter(match == match_id) |>
        .pull_unique_players()
    },
    pull_repeated_players = function(match_id_1, match_id_2) {
      players_of_match_1 <- self$data_team |>
        dplyr::filter(match == match_id_1) |>
        .pull_unique_players()
      players_of_match_2 <- self$data_team |>
        dplyr::filter(match == match_id_2) |>
        .pull_unique_players()
      repeated_players <- intersect(players_of_match_1, players_of_match_2)
      return(repeated_players)
    }
  )
)

extract_players_from_team <- function(played_minutes, team_name) {
  players_of_team <- .filter_players_from_team_with_more_than_10_minutes(played_minutes, team_name) |>
    .pull_unique_players()
}

extract_players_from_match <- function(played_minutes, match_id) {
  players_of_match <- played_minutes |>
    dplyr::filter(match == match_id, minutes > 10) |>
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
