played_minutes <- readr::read_csv("/workdir/tests/data/played_minutes_xolos.csv", show_col_types = FALSE)

describe("Class calculator of Jaccard index", {
  jaccard_calculator <- JaccardIndexCalculator$new()
  jaccard_calculator$set_data(played_minutes)
  it("Creata an instance of the class", {
    expect_true(is(jaccard_calculator, "JaccardIndexCalculator"))
  })
  it("Set up the data", {
    expect_equal(jaccard_calculator$data, played_minutes)
  })
  it("Set up team", {
    jaccard_calculator$set_team("Club Tijuana")
    n_obtained_players <- nrow(jaccard_calculator$data_team)
    n_expected_players <- 31
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
