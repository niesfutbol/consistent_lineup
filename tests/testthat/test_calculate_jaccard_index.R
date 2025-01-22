describe("Extract players from game", {
  it("players that played more than ten minutes", {
    played_minutes <- readr::read_csv("/workdir/tests/data/played_minutes_xolos.csv", show_col_types = FALSE)
    players_of_xolos <- extract_players_from_team(played_minutes, "Club Tijuana")
    n_obtained_players <- length(players_of_xolos)
    n_expected_players <- 21
    expect_equal(n_obtained_players, n_expected_players)
  })
})
