played_minutes <- readr::read_csv("/workdir/tests/data/played_minutes_xolos.csv", show_col_types = FALSE)

describe("Class calculator of Jaccard index", {
  it("Creata an instance of the class", {
    jaccard_calculator <- JaccardIndexCalculator$new()
    expect_true(is(jaccard_calculator, "JaccardIndexCalculator"))
  })
})

describe("Extract players from game", {
  it("players that played more than ten minutes", {
    players_of_xolos <- extract_players_from_team(played_minutes, "Club Tijuana")
    n_obtained_players <- length(players_of_xolos)
    n_expected_players <- 21
    expect_equal(n_obtained_players, n_expected_players)
  })
})

describe("Extract player from un match", {
  it("players that played more than ten minutes", {
    players_of_xolos <- extract_players_from_match(played_minutes, 1323484)
    n_obtained_players <- length(players_of_xolos)
    n_expected_players <- 29
    expect_equal(n_obtained_players, n_expected_players)
  })
})
