library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)

source("spoorkbook/fade_algorithm.R")
source("spoorkbook/report_reader.R")

american_to_probability <- function(american_odds) {
  ifelse(is.na(american_odds), NA,
         ifelse(american_odds > 0, 100 / (american_odds + 100),
                -american_odds / (-american_odds + 100)))
}

fetch_mlb_odds <- function(api_key) {
  base <- "https://api.the-odds-api.com"
  sport <- "baseball_mlb"
  markets <- "h2h"
  endpoint <- paste0(
    "/v4/sports/", sport,
    "/odds/?apiKey=", api_key,
    "&regions=us&markets=", markets,
    "&bookmakers=draftkings,fanduel&oddsFormat=american"
  )

  response <- GET(paste0(base, endpoint))
  if (http_error(response)) {
    warning("Odds API request failed; running fade logic without live lines.")
    return(data.frame())
  }

  today <- Sys.Date()
  now <- Sys.time()

  content <- fromJSON(content(response, "text")) %>%
    unnest(cols = c(bookmakers)) %>%
    unnest(cols = c(markets), names_sep = "_") %>%
    unnest(cols = c(markets_outcomes), names_sep = "_") %>%
    mutate(commence_time = ymd_hms(commence_time)) %>%
    filter(commence_time < today + 1, commence_time >= now) %>%
    filter(markets_key == "h2h")

  if (nrow(content) == 0) return(data.frame())

  content %>%
    select(id, home_team, away_team, commence_time, title,
           markets_outcomes_name, markets_outcomes_price) %>%
    pivot_wider(names_from = title, values_from = markets_outcomes_price) %>%
    group_by(home_team, away_team, commence_time) %>%
    summarise(
      dk_price_home = DraftKings[markets_outcomes_name == home_team][1],
      dk_price_away = DraftKings[markets_outcomes_name == away_team][1],
      fd_price_home = FanDuel[markets_outcomes_name == home_team][1],
      fd_price_away = FanDuel[markets_outcomes_name == away_team][1],
      start_time = str_remove(
        format(with_tz(commence_time, tzone = "America/New_York"), "%I:%M %p"),
        "^0+"
      ),
      .groups = "drop"
    )
}

report <- read_spoorkbook_report()

api <- Sys.getenv("ODDS_API_KEY", unset = "d72d888a7e2831439aa64a8ac1525f71")
odds_df <- tryCatch(fetch_mlb_odds(api), error = function(e) data.frame())

fade_result <- score_fade_plays(report, odds_df)
recs <- fade_result$game_recommendations

output_df <- recs %>%
  transmute(
    sport,
    game = ifelse(
      is.na(team_b) | team_b == "",
      paste0(team_a, " (solo fade)"),
      paste0(team_a, " vs ", team_b)
    ),
    start_time = if ("start_time" %in% names(recs)) coalesce(start_time, "—") else "—",
    fade_target = book_needs_fade_team,
    fade_price_report = parse_price(book_needs_price),
    fade_price_dk = if ("fade_team_price_dk" %in% names(recs)) fade_team_price_dk else NA_real_,
    square_fade = square_fade_team,
    play_type,
    recommendation = recommended_side,
    action,
    confidence
  )

fade_index_df <- fade_result$fade_index %>%
  filter(is_fade) %>%
  transmute(
    sport,
    team,
    fade_score,
    signals,
    opponents,
    report_prices,
    never_bet = TRUE
  )

if (nrow(output_df) == 0) {
  stop("No fade plays found in Spoorkbook report.")
}

write.csv(output_df, "Spoorkbook_Fade_Plays.csv", row.names = FALSE)
write.csv(fade_index_df, "Spoorkbook_Fade_Index.csv", row.names = FALSE)

if (requireNamespace("gt", quietly = TRUE) && requireNamespace("webshot2", quietly = TRUE)) {
  fade_table <- output_df %>%
    gt() %>%
    tab_header(
      title = md("**Spoorkbook Fade Plays**"),
      subtitle = md("BOOK NEEDS + SQUARE TOP POSITION — teams listed are **fades**, not plays")
    ) %>%
    cols_label(
      sport = "Sport",
      game = "Game",
      start_time = "Time",
      fade_target = "Fade Team",
      fade_price_report = "Report Line",
      fade_price_dk = "DK Line",
      square_fade = "Square Fade",
      play_type = "Type",
      recommendation = "Signal",
      action = "Action",
      confidence = "Conf."
    ) %>%
    fmt_number(columns = c(fade_price_report, fade_price_dk), decimals = 0, force_sign = TRUE) %>%
    tab_style(
      style = cell_fill(color = "#ffcccc"),
      locations = cells_body(columns = fade_target)
    ) %>%
    tab_style(
      style = cell_fill(color = "#ffe6cc"),
      locations = cells_body(columns = play_type, rows = play_type == "FADE ONLY")
    ) %>%
    tab_style(
      style = cell_fill(color = "#e6e6e6"),
      locations = cells_body(columns = play_type, rows = play_type == "NO PLAY")
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_source_note(
      source_note = md(
        paste0(
          "Rules: (1) Teams in **BOOK NEEDS** = fade, never bet them. ",
          "(2) Teams in **SQUARE TOP POSITION** = fade. ",
          "(3) Solo book-needs fade (e.g. Boston) = **FADE ONLY**, no auto-play on opponent. ",
          "(4) Both sides faded = skip the game."
        )
      )
    )

  gtsave(fade_table, filename = "Spoorkbook_Fade_Plays.png", expand = 100, vheight = 120, vwidth = 1400)
  message("Generated Spoorkbook_Fade_Plays.png")
} else {
  message("gt/webshot2 not available — skipped PNG output (CSV written).")
}

message("Generated Spoorkbook_Fade_Plays.csv and Spoorkbook_Fade_Index.csv")
message("Fade targets (NEVER bet these teams):")
for (team in fade_index_df$team) {
  message("  - ", team, " (", fade_index_df$signals[fade_index_df$team == team][1], ")")
}
