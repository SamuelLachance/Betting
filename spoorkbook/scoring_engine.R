# Unified +EV scoring engine
# Only teams passing fade filters AND showing positive expected value are recommended.

SCORING_CONFIG <- list(
  min_edge = 0.03,
  min_ev = 0.001,
  high_edge = 0.08,
  medium_edge = 0.05
)

american_to_probability <- function(american_odds) {
  ifelse(is.na(american_odds), NA_real_,
         ifelse(american_odds > 0, 100 / (american_odds + 100),
                -american_odds / (-american_odds + 100)))
}

calc_ev <- function(win_prob, american_odds, stake = 1) {
  ifelse(
    is.na(win_prob) | is.na(american_odds),
    NA_real_,
    ifelse(
      american_odds > 0,
      win_prob * (stake * american_odds / 100) - (1 - win_prob) * stake,
      win_prob * (stake * 100 / abs(american_odds)) - (1 - win_prob) * stake
    )
  )
}

calc_edge <- function(win_prob, american_odds) {
  win_prob - american_to_probability(american_odds)
}

pick_confidence <- function(edge) {
  case_when(
    edge >= SCORING_CONFIG$high_edge ~ "HIGH",
    edge >= SCORING_CONFIG$medium_edge ~ "MEDIUM",
    edge >= SCORING_CONFIG$min_edge ~ "LOW",
    TRUE ~ "PASS"
  )
}

get_dual_fade_games <- function(fade_result) {
  if (is.null(fade_result) || is.null(fade_result$game_recommendations)) {
    return(character())
  }

  fade_result$game_recommendations %>%
    filter(dual_fade) %>%
    mutate(game_key = mapply(game_key, team_a, team_b, USE.NAMES = FALSE)) %>%
    pull(game_key) %>%
    unique()
}

get_fade_block_reason <- function(team, game_key, faded_teams, dual_fade_games) {
  if (team %in% faded_teams) {
    return("spoorkbook_fade")
  }
  if (game_key %in% dual_fade_games) {
    return("dual_fade_game")
  }
  NA_character_
}

prepare_game_sides <- function(games_df, odds_h2h) {
  home_odds <- odds_h2h %>%
    filter(bet == home_team) %>%
    select(home_team, away_team, start_time, dk_price_home = dk_price, fd_price_home = fd_price)

  away_odds <- odds_h2h %>%
    filter(bet == away_team) %>%
    select(home_team, away_team, dk_price_away = dk_price, fd_price_away = fd_price)

  games_wide <- games_df %>%
    left_join(home_odds, by = c("home_team", "away_team")) %>%
    left_join(away_odds, by = c("home_team", "away_team"))

  home_sides <- games_wide %>%
    transmute(
      home_team,
      away_team,
      start_time,
      team = home_team,
      opponent = away_team,
      model_prob = home_prob,
      dk_price = dk_price_home,
      fd_price = fd_price_home
    )

  away_sides <- games_wide %>%
    transmute(
      home_team,
      away_team,
      start_time,
      team = away_team,
      opponent = home_team,
      model_prob = away_prob,
      dk_price = dk_price_away,
      fd_price = fd_price_away
    )

  bind_rows(home_sides, away_sides) %>%
    filter(!is.na(model_prob), !is.na(dk_price) | !is.na(fd_price))
}

score_plus_ev_picks <- function(
  sides_df,
  fade_result = NULL,
  min_edge = SCORING_CONFIG$min_edge,
  min_ev = SCORING_CONFIG$min_ev
) {
  faded_teams <- if (!is.null(fade_result)) {
    fade_result$fade_index$team[fade_result$fade_index$is_fade]
  } else {
    character()
  }

  dual_fade_games <- if (!is.null(fade_result)) {
    get_dual_fade_games(fade_result)
  } else {
    character()
  }

  sides_df %>%
    mutate(
      game_key = mapply(game_key, team, opponent, USE.NAMES = FALSE),
      dk_edge = calc_edge(model_prob, dk_price),
      fd_edge = calc_edge(model_prob, fd_price),
      dk_ev = calc_ev(model_prob, dk_price),
      fd_ev = calc_ev(model_prob, fd_price),
      best_book = case_when(
        !is.na(dk_ev) & !is.na(fd_ev) & dk_ev >= fd_ev ~ "DraftKings",
        !is.na(fd_ev) ~ "FanDuel",
        !is.na(dk_ev) ~ "DraftKings",
        TRUE ~ NA_character_
      ),
      best_price = case_when(
        best_book == "DraftKings" ~ dk_price,
        best_book == "FanDuel" ~ fd_price,
        TRUE ~ NA_real_
      ),
      best_edge = case_when(
        best_book == "DraftKings" ~ dk_edge,
        best_book == "FanDuel" ~ fd_edge,
        TRUE ~ NA_real_
      ),
      best_ev = case_when(
        best_book == "DraftKings" ~ dk_ev,
        best_book == "FanDuel" ~ fd_ev,
        TRUE ~ NA_real_
      ),
      block_reason = mapply(
        get_fade_block_reason,
        team,
        game_key,
        MoreArgs = list(
          faded_teams = faded_teams,
          dual_fade_games = dual_fade_games
        ),
        USE.NAMES = FALSE
      ),
      eligible = is.na(block_reason) & !is.na(best_edge) & !is.na(best_ev) &
        best_edge >= min_edge & best_ev > min_ev
    ) %>%
    filter(eligible) %>%
    mutate(confidence = pick_confidence(best_edge)) %>%
    select(-eligible) %>%
    arrange(desc(best_ev))
}
