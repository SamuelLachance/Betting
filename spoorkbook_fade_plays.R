library(dplyr)
library(readr)

source("spoorkbook/fade_algorithm.R")
source("spoorkbook/report_reader.R")

report <- read_spoorkbook_report()
fade_result <- score_fade_plays(report)

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

dual_fade_df <- fade_result$game_recommendations %>%
  filter(dual_fade) %>%
  transmute(
    sport,
    game = paste0(team_a, " vs ", team_b),
    reason = "Both sides faded — excluded from +EV pool",
    blocked = TRUE
  )

write.csv(fade_index_df, "Spoorkbook_Fade_Index.csv", row.names = FALSE)
write.csv(dual_fade_df, "Spoorkbook_Dual_Fade_Games.csv", row.names = FALSE)

message("Spoorkbook filters loaded:")
message("  Blocked teams: ", nrow(fade_index_df))
message("  Dual-fade games (no ML): ", nrow(dual_fade_df))
message("")
message("Run mlb_ml_values.R to generate +EV picks (only positive EV after these filters).")

for (team in fade_index_df$team) {
  message("  BLOCK: ", team, " (", fade_index_df$signals[fade_index_df$team == team][1], ")")
}
