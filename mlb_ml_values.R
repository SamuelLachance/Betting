library(httr)
library(jsonlite)
library(tidyr)
library(xml2)
library(dplyr)
library(gt)
library(png)
library(webshot2)
library(lubridate)
library(stringr)
library(readr)

source("spoorkbook/fade_algorithm.R")
source("spoorkbook/report_reader.R")
source("spoorkbook/scoring_engine.R")

api <- "d72d888a7e2831439aa64a8ac1525f71"
base <- "https://api.the-odds-api.com"
sport <- "baseball_mlb"
markets <- "h2h,totals" #,btts,draw_no_bet"
endpoint <- paste0("/v4/sports/", sport, "/odds/?apiKey=", api, "&regions=us&markets=", markets, "&bookmakers=draftkings,fanduel&oddsFormat=american")

url <- paste0(base, endpoint)

today <- Sys.Date()
time <- Sys.time()

# Make the GET request
response <- GET(url)

# Check the response status
content <- fromJSON(content(response, "text")) %>%
  unnest(., cols = c(bookmakers)) %>%
  unnest(., cols = c(markets), names_sep = "_") %>%
  unnest(., cols = c(markets_outcomes), names_sep = "_") %>%
  mutate(commence_time = ymd_hms(commence_time)) %>%
  filter(commence_time < today+1, commence_time >= time)

american_to_probability <- function(american_odds) {
  ifelse(is.na(american_odds), NA,
         ifelse(american_odds > 0, 100 / (american_odds + 100),
                -american_odds / (-american_odds + 100)))
}

prices_totals <- content %>% filter(markets_key == "totals") %>%
  select(id, home_team, away_team, commence_time, title, markets_outcomes_name, markets_outcomes_point, markets_outcomes_price) %>%
  pivot_wider(names_from = title, values_from = markets_outcomes_price) %>%
  mutate(dk_prob = american_to_probability(DraftKings),
         fd_prob = american_to_probability(FanDuel),
         bet = paste0(markets_outcomes_name, " ", markets_outcomes_point),
         start_time = format(with_tz(ymd_hms(commence_time, tz = "UTC"), tzone = "America/New_York"), "%b %d, %I:%M")) %>%
  select(-c(markets_outcomes_name, markets_outcomes_point, commence_time, id))

prices_h2h <- content %>% filter(markets_key == "h2h") %>%
  select(id, home_team, away_team, commence_time, title, markets_outcomes_name, markets_outcomes_price) %>%
  pivot_wider(names_from = title, values_from = markets_outcomes_price) %>%
  rename(dk_price = DraftKings, fd_price = FanDuel) %>%
  mutate(dk_prob = american_to_probability(dk_price),
         fd_prob = american_to_probability(fd_price),
         bet = markets_outcomes_name,
         start_time = str_remove(format(with_tz(ymd_hms(commence_time, tz = "UTC"), tzone = "America/New_York"), "%I:%M"), "^0+")) %>%
  select(-c(markets_outcomes_name, commence_time, id))


#ballpark pal
bp_games_url <- "https://ballparkpal.com/index.php"

# Read in the HTML from the webpage
bp_games_webpage <- read_html(bp_games_url)

# game list

game_list <- list()


get_game_hrefs <- function(bp_games_webpage) {
  game_hrefs <- list()

  for (i in 1:30) {
    tryCatch({
      # This code may generate an error
      value <- xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(bp_games_webpage, 2), 5), i), 3), 2), 1))[["href"]]
      game_hrefs[[i]] <- value
    }, error = function(e) {
      # If an error occurs, store NA
      game_hrefs[[i]] <- NA
    })
  }

  return(game_hrefs)
}

game_hrefs <- get_game_hrefs(bp_games_webpage)

filtered_game_hrefs <- as.vector(unlist(game_hrefs))

bp_game_df <- data.frame(
  home_team = character(),
  away_team = character(),
  home_total = numeric(),
  away_total = numeric(),
  game_total = numeric(),
  home_prob = numeric(),
  away_prob = numeric(),
  yrfi_prob = numeric(),
  nrfi_prob = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(filtered_game_hrefs)){
  url <- filtered_game_hrefs[i]
  #home_team
  home_team_xpath <- "/html/body/div/div[2]/div/div[2]/p/text()"
  home_team <- xml_text(xml_find_all(read_html(url), home_team_xpath))

  #away_team
  away_team_xpath <- "/html/body/div/div[2]/div/div[1]/p/text()"
  away_team <- xml_text(xml_find_all(read_html(url), away_team_xpath))

  #home_score
  home_total_xpath <- "/html/body/div/div[3]/div/div[3]/p"
  home_total <- as.numeric(xml_text(xml_find_all(read_html(url), home_total_xpath)))
  home_total <- ifelse(length(home_total) == 0, NA, home_total)


  #away_score
  away_total_xpath <- "/html/body/div/div[3]/div/div[1]/p"
  away_total <- as.numeric(xml_text(xml_find_all(read_html(url), away_total_xpath)))
  away_total <- ifelse(length(away_total) == 0, NA, away_total)

  #home_prob
  home_prob_xpath <- "/html/body/div/div[4]/div/div[3]/p"
  home_prob <- as.numeric(gsub("([0-9.]+)%.*", "\\1", xml_text(xml_find_all(read_html(url), home_prob_xpath))))/100
  home_prob <- ifelse(length(home_prob) == 0, NA, home_prob)

  #away_prob
  away_prob_xpath <- "/html/body/div/div[4]/div/div[1]/p"
  away_prob <- as.numeric(gsub(".*\\s([0-9.]+)%.*", "\\1", xml_text(xml_find_all(read_html(url), away_prob_xpath))))/100
  away_prob <- ifelse(length(away_prob) == 0, NA, away_prob)

  #runs first inning
  yrfi_xpath <- "/html/body/div/div[11]/div[3]/p/text()[1]"
  yrfi_prob <- as.numeric(gsub("([0-9.]+)%.*", "\\1", xml_text(xml_find_all(read_html(url), yrfi_xpath))))/100
  nrfi_xpath <- "/html/body/div/div[11]/div[3]/p/text()[2]"
  nrfi_prob <- as.numeric(gsub("([0-9.]+)%.*", "\\1", xml_text(xml_find_all(read_html(url), nrfi_xpath))))/100
  yrfi_prob <- ifelse(length(yrfi_prob) == 0, NA, yrfi_prob)
  nrfi_prob <- ifelse(length(nrfi_prob) == 0, NA, nrfi_prob)

  new_row <- data.frame(
    home_team = home_team,
    away_team = away_team,
    home_total = home_total,
    away_total = away_total,
    game_total = home_total + away_total,
    home_prob = home_prob,
    away_prob = away_prob,
    yrfi_prob = yrfi_prob,
    nrfi_prob = nrfi_prob
  )

  bp_game_df <- rbind(bp_game_df, new_row)

  #return(bp_game_df)

}

team_names_with_city <- c(
  "Arizona Diamondbacks",
  "Atlanta Braves",
  "Baltimore Orioles",
  "Boston Red Sox",
  "Chicago White Sox",
  "Chicago Cubs",
  "Cincinnati Reds",
  "Cleveland Guardians",
  "Colorado Rockies",
  "Detroit Tigers",
  "Houston Astros",
  "Kansas City Royals",
  "Los Angeles Angels",
  "Los Angeles Dodgers",
  "Miami Marlins",
  "Milwaukee Brewers",
  "Minnesota Twins",
  "New York Yankees",
  "New York Mets",
  "Oakland Athletics",
  "Philadelphia Phillies",
  "Pittsburgh Pirates",
  "San Diego Padres",
  "San Francisco Giants",
  "Seattle Mariners",
  "St. Louis Cardinals",
  "Tampa Bay Rays",
  "Texas Rangers",
  "Toronto Blue Jays",
  "Washington Nationals"
)

team_names_without_city <- c(
  "Dbacks",
  "Braves",
  "Orioles",
  "Red Sox",
  "White Sox",
  "Cubs",
  "Reds",
  "Guardians",
  "Rockies",
  "Tigers",
  "Astros",
  "Royals",
  "Angels",
  "Dodgers",
  "Marlins",
  "Brewers",
  "Twins",
  "Yankees",
  "Mets",
  "Athletics",
  "Phillies",
  "Pirates",
  "Padres",
  "Giants",
  "Mariners",
  "Cardinals",
  "Rays",
  "Rangers",
  "Blue Jays",
  "Nationals"
)

teams <- data.frame(team_with_city = team_names_with_city, team_wo_city = team_names_without_city)


bp_game_df_adjusted <- bp_game_df %>%
  left_join(., teams, by = c("home_team" = "team_wo_city")) %>%
  mutate(home_team = team_with_city) %>%
  left_join(., teams, by = c("away_team" = "team_wo_city")) %>%
  mutate(away_team = team_with_city.y) %>%
  select(c(1, 2, 3, 4, 5, 6, 7))

fade_data <- load_fade_result()

sides_df <- prepare_game_sides(bp_game_df_adjusted, prices_h2h)
plus_ev_picks <- score_plus_ev_picks(sides_df, fade_data$fade_result)

if (nrow(plus_ev_picks) == 0) {
  message("No +EV picks today after Spoorkbook fade filters.")
}

display_picks <- plus_ev_picks %>%
  left_join(teams, by = c("away_team" = "team_with_city")) %>%
  rename(away_short = team_wo_city) %>%
  left_join(teams, by = c("home_team" = "team_with_city")) %>%
  rename(home_short = team_wo_city) %>%
  left_join(teams, by = c("team" = "team_with_city")) %>%
  rename(pick_short = team_wo_city) %>%
  transmute(
    game = paste0(away_short, " @ ", home_short),
    start_time,
    bet = pick_short,
    bet_prob = model_prob,
    book = best_book,
    price = best_price,
    edge = best_edge,
    ev = best_ev,
    confidence
  )

if (nrow(display_picks) == 0) {
  display_picks <- data.frame(
    game = "No +EV plays today",
    start_time = "—",
    bet = "—",
    bet_prob = NA_real_,
    book = "—",
    price = NA_real_,
    edge = NA_real_,
    ev = NA_real_,
    confidence = "—",
    stringsAsFactors = FALSE
  )
}

write.csv(display_picks, "MLB_Plus_EV_Picks.csv", row.names = FALSE)

final_df <- display_picks %>%
  select(game, start_time, bet, bet_prob, book, price, edge, ev, confidence) %>%
  gt() %>%
  tab_spanner(label = "Pick", columns = c(bet, bet_prob, confidence)) %>%
  tab_spanner(label = "Best Book", columns = c(book, price)) %>%
  tab_spanner(label = "+EV", columns = c(edge, ev)) %>%
  cols_label(
    game = "Game",
    bet = "Side",
    bet_prob = "Model %",
    book = "Book",
    price = "Price",
    edge = "Edge",
    ev = "EV",
    confidence = "Conf.",
    start_time = "Time"
  ) %>%
  fmt_number(columns = price, force_sign = TRUE, decimals = 0) %>%
  fmt_percent(columns = c(bet_prob, edge), decimals = 1) %>%
  fmt_number(columns = ev, decimals = 3) %>%
  data_color(
    columns = ev,
    fn = scales::col_numeric(palette = c("white", "green"), domain = c(0, 0.15)),
    apply_to = "fill"
  ) %>%
  cols_align(columns = c(game, start_time), align = "left") %>%
  cols_align(columns = c(bet, book, price, edge, ev, confidence), align = "center") %>%
  tab_style(style = cell_borders(sides = "right"), locations = cells_body(columns = 2)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_spanners()) %>%
  tab_header(
    title = md("**MLB +EV Plays**"),
    subtitle = md("Only positive expected value picks surviving Spoorkbook fade filters")
  ) %>%
  tab_source_note(
    source_note = md(
      paste0(
        "Min edge: ", SCORING_CONFIG$min_edge * 100, "% | ",
        "Faded teams blocked | Dual-fade games skipped | Model: ballparkpal.com"
      )
    )
  )

gtsave(final_df, expand = 100,
       filename = "MLB_ML_Plays.png",
       vheight = max(100, 40 * max(nrow(display_picks), 1) + 80),
       vwidth = 1100)
