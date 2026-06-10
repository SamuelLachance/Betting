# Spoorkbook fade signal engine
# Interprets BOOK NEEDS (FADE PLAYS) and SQUARE TOP POSITION (FADE) reports.

SIGNAL_WEIGHTS <- list(
  book_needs_fade = -100L,
  square_fade = -80L,
  dual_fade_game = -200L,
  single_sided_fade = -100L,
  confirmed_fade = -150L
)

normalize_team <- function(team, sport = "MLB") {
  if (length(team) == 0) return(character())
  if (length(sport) == 1L) sport <- rep(sport, length(team))

  mapply(normalize_team_single, team, sport, USE.NAMES = FALSE)
}

normalize_team_single <- function(team, sport = "MLB") {
  if (is.na(team) || team == "") return(NA_character_)

  cleaned <- toupper(trimws(team))
  cleaned <- gsub("\\s+", " ", cleaned)

  mlb_map <- c(
    "ARIZONA" = "Arizona Diamondbacks", "DIAMONDBACKS" = "Arizona Diamondbacks", "DBACKS" = "Arizona Diamondbacks",
    "ATLANTA" = "Atlanta Braves", "BRAVES" = "Atlanta Braves",
    "BALTIMORE" = "Baltimore Orioles", "ORIOLES" = "Baltimore Orioles",
    "BOSTON" = "Boston Red Sox", "RED SOX" = "Boston Red Sox",
    "CHICAGO WHITE SOX" = "Chicago White Sox", "WHITE SOX" = "Chicago White Sox",
    "CHICAGO CUBS" = "Chicago Cubs", "CUBS" = "Chicago Cubs",
    "CINCINNATI" = "Cincinnati Reds", "REDS" = "Cincinnati Reds",
    "CLEVELAND" = "Cleveland Guardians", "GUARDIANS" = "Cleveland Guardians",
    "COLORADO" = "Colorado Rockies", "ROCKIES" = "Colorado Rockies",
    "DETROIT" = "Detroit Tigers", "TIGERS" = "Detroit Tigers",
    "HOUSTON" = "Houston Astros", "ASTROS" = "Houston Astros",
    "KANSAS CITY" = "Kansas City Royals", "ROYALS" = "Kansas City Royals",
    "LA ANGELS" = "Los Angeles Angels", "LOS ANGELES ANGELS" = "Los Angeles Angels", "ANGELS" = "Los Angeles Angels",
    "LA DODGERS" = "Los Angeles Dodgers", "LOS ANGELES DODGERS" = "Los Angeles Dodgers", "DODGERS" = "Los Angeles Dodgers",
    "MIAMI" = "Miami Marlins", "MARLINS" = "Miami Marlins",
    "MILWAUKEE" = "Milwaukee Brewers", "BREWERS" = "Milwaukee Brewers",
    "MINNESOTA" = "Minnesota Twins", "TWINS" = "Minnesota Twins",
    "NY YANKEES" = "New York Yankees", "NEW YORK YANKEES" = "New York Yankees", "YANKEES" = "New York Yankees",
    "NY METS" = "New York Mets", "NEW YORK METS" = "New York Mets", "METS" = "New York Mets",
    "OAKLAND" = "Oakland Athletics", "ATHLETICS" = "Oakland Athletics", "A'S" = "Oakland Athletics",
    "PHILADELPHIA" = "Philadelphia Phillies", "PHILLIES" = "Philadelphia Phillies",
    "PITTSBURGH" = "Pittsburgh Pirates", "PIRATES" = "Pittsburgh Pirates",
    "SAN DIEGO" = "San Diego Padres", "PADRES" = "San Diego Padres",
    "SAN FRANCISCO" = "San Francisco Giants", "GIANTS" = "San Francisco Giants",
    "SEATTLE" = "Seattle Mariners", "MARINERS" = "Seattle Mariners",
    "ST LOUIS" = "St. Louis Cardinals", "ST. LOUIS" = "St. Louis Cardinals", "CARDINALS" = "St. Louis Cardinals",
    "TAMPA BAY" = "Tampa Bay Rays", "RAYS" = "Tampa Bay Rays",
    "TEXAS" = "Texas Rangers", "RANGERS" = "Texas Rangers",
    "TORONTO" = "Toronto Blue Jays", "BLUE JAYS" = "Toronto Blue Jays",
    "WASHINGTON" = "Washington Nationals", "NATIONALS" = "Washington Nationals"
  )

  wnba_map <- c(
    "ATLANTA" = "Atlanta Dream", "DREAM" = "Atlanta Dream",
    "CHICAGO" = "Chicago Sky", "SKY" = "Chicago Sky",
    "CONNECTICUT" = "Connecticut Sun", "SUN" = "Connecticut Sun",
    "DALLAS" = "Dallas Wings", "WINGS" = "Dallas Wings",
    "GOLDEN STATE" = "Golden State Valkyries", "VALKYRIES" = "Golden State Valkyries",
    "INDIANA" = "Indiana Fever", "FEVER" = "Indiana Fever",
    "LAS VEGAS" = "Las Vegas Aces", "ACES" = "Las Vegas Aces",
    "LOS ANGELES" = "Los Angeles Sparks", "LA SPARKS" = "Los Angeles Sparks", "SPARKS" = "Los Angeles Sparks",
    "MINNESOTA" = "Minnesota Lynx", "LYNX" = "Minnesota Lynx",
    "NEW YORK" = "New York Liberty", "LIBERTY" = "New York Liberty",
    "PHOENIX" = "Phoenix Mercury", "MERCURY" = "Phoenix Mercury",
    "SEATTLE" = "Seattle Storm", "STORM" = "Seattle Storm",
    "WASHINGTON" = "Washington Mystics", "MYSTICS" = "Washington Mystics"
  )

  lookup <- if (toupper(sport) == "WNBA") wnba_map else mlb_map

  if (cleaned %in% names(lookup)) {
    return(unname(lookup[[cleaned]]))
  }

  for (key in names(lookup)) {
    if (grepl(key, cleaned, fixed = TRUE) || grepl(cleaned, key, fixed = TRUE)) {
      return(unname(lookup[[key]]))
    }
  }

  cleaned
}

parse_price <- function(price) {
  vapply(price, function(p) {
    if (is.na(p) || p == "") return(NA_real_)
    as.numeric(gsub("[^0-9+-]", "", as.character(p)))
  }, numeric(1))
}

build_fade_signals <- function(report_df) {
  book_needs <- report_df %>%
    filter(!is.na(book_needs_team), book_needs_team != "") %>%
    transmute(
      sport = sport,
      team_raw = book_needs_team,
      team = normalize_team(book_needs_team, sport),
      opponent_raw = book_needs_opponent,
      opponent = normalize_team(book_needs_opponent, sport),
      report_price = parse_price(book_needs_price),
      signal_type = "book_needs_fade",
      signal_weight = SIGNAL_WEIGHTS$book_needs_fade
    )

  square_fades <- report_df %>%
    filter(!is.na(square_team), square_team != "") %>%
    transmute(
      sport = sport,
      team_raw = square_team,
      team = normalize_team(square_team, sport),
      opponent_raw = square_opponent,
      opponent = normalize_team(square_opponent, sport),
      report_price = parse_price(square_price),
      signal_type = "square_fade",
      signal_weight = SIGNAL_WEIGHTS$square_fade
    )

  bind_rows(book_needs, square_fades)
}

game_key <- function(team_a, team_b) {
  paste(sort(c(team_a, team_b)), collapse = " vs ")
}

score_fade_plays <- function(report_df, odds_df = NULL) {
  signals <- build_fade_signals(report_df)

  fade_index <- signals %>%
    group_by(sport, team) %>%
    summarise(
      fade_score = sum(signal_weight),
      signals = paste(unique(signal_type), collapse = " + "),
      opponents = paste(unique(na.omit(opponent)), collapse = " / "),
      report_prices = paste(na.omit(report_price), collapse = " / "),
      .groups = "drop"
    ) %>%
    mutate(is_fade = fade_score <= SIGNAL_WEIGHTS$square_fade)

  game_signals <- report_df %>%
    filter(!is.na(book_needs_team), book_needs_team != "") %>%
    mutate(
      book_needs_price = book_needs_price,
      team_a = normalize_team(book_needs_team, sport),
      team_b = normalize_team(book_needs_opponent, sport),
      game_id = ifelse(
        !is.na(team_b) & team_b != "",
        mapply(game_key, team_a, team_b, USE.NAMES = FALSE),
        paste0(team_a, " (solo)")
      ),
      has_square_pair = !is.na(square_team) & square_team != "",
      dual_fade = has_square_pair
    )

  recommendations <- game_signals %>%
    rowwise() %>%
    mutate(
      book_needs_fade_team = team_a,
      square_fade_team = if (has_square_pair) normalize_team(square_team, sport) else NA_character_,
      fade_teams = paste(na.omit(c(book_needs_fade_team, square_fade_team)), collapse = " + "),
      play_type = case_when(
        dual_fade ~ "NO PLAY",
        !has_square_pair & !is.na(book_needs_fade_team) ~ "FADE ONLY",
        TRUE ~ "NO PLAY"
      ),
      recommended_side = case_when(
        dual_fade ~ "NONE — both sides faded",
        play_type == "FADE ONLY" ~ paste0("FADE ", book_needs_fade_team, " — do NOT bet this team"),
        TRUE ~ "NONE"
      ),
      action = case_when(
        dual_fade ~ paste0("Skip ML: fade ", fade_teams),
        play_type == "FADE ONLY" ~ paste0(
          "Avoid ", book_needs_fade_team,
          ". No automatic play on ",
          ifelse(is.na(team_b) | team_b == "", "opponent", team_b),
          "."
        ),
        TRUE ~ "No action"
      ),
      confidence = case_when(
        dual_fade ~ "HIGH",
        play_type == "FADE ONLY" ~ "HIGH",
        TRUE ~ "LOW"
      )
    ) %>%
    ungroup()

  recommendations <- join_game_odds(recommendations, odds_df)

  list(
    fade_index = fade_index,
    game_recommendations = recommendations,
    all_signals = signals
  )
}

get_faded_teams <- function(fade_result, teams_lookup = NULL) {
  faded_full <- fade_result$fade_index$team[fade_result$fade_index$is_fade]

  faded_short <- if (!is.null(teams_lookup)) {
    teams_lookup$team_wo_city[teams_lookup$team_with_city %in% faded_full]
  } else {
    character()
  }

  list(full = unique(faded_full), short = unique(faded_short))
}

is_team_faded <- function(team, fade_result) {
  team %in% fade_result$fade_index$team[fade_result$fade_index$is_fade]
}

filter_non_fade_picks <- function(picks_df, fade_result, team_col = "bet") {
  faded_teams <- fade_result$fade_index$team[fade_result$fade_index$is_fade]
  picks_df %>%
    filter(!.data[[team_col]] %in% faded_teams)
}

join_game_odds <- function(recommendations, odds_df) {
  if (is.null(odds_df) || nrow(odds_df) == 0) {
    return(recommendations)
  }

  odds_direct <- odds_df %>%
    transmute(
      team_a = home_team,
      team_b = away_team,
      dk_price_home,
      dk_price_away,
      fd_price_home,
      fd_price_away,
      start_time
    )

  odds_flipped <- odds_df %>%
    transmute(
      team_a = away_team,
      team_b = home_team,
      dk_price_home = dk_price_away,
      dk_price_away = dk_price_home,
      fd_price_home = fd_price_away,
      fd_price_away = fd_price_home,
      start_time
    )

  odds_bidirectional <- bind_rows(odds_direct, odds_flipped) %>%
    distinct(team_a, team_b, .keep_all = TRUE)

  recommendations %>%
    left_join(odds_bidirectional, by = c("team_a", "team_b")) %>%
    mutate(
      fade_team_price_dk = case_when(
        book_needs_fade_team == team_a ~ dk_price_home,
        book_needs_fade_team == team_b ~ dk_price_away,
        TRUE ~ NA_real_
      ),
      fade_team_price_fd = case_when(
        book_needs_fade_team == team_a ~ fd_price_home,
        book_needs_fade_team == team_b ~ fd_price_away,
        TRUE ~ NA_real_
      )
    )
}
