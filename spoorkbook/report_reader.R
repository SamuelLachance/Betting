# Shared Spoorkbook report ingestion for all betting scripts.

SPOORKBOOK_SHEET_URL <- Sys.getenv("SPOORKBOOK_SHEET_URL", unset = "")
SPOORKBOOK_REPORT_PATH <- Sys.getenv(
  "SPOORKBOOK_REPORT_PATH",
  unset = "spoorkbook/sample_report.csv"
)

read_spoorkbook_report <- function() {
  if (SPOORKBOOK_SHEET_URL != "") {
    if (!requireNamespace("gsheet", quietly = TRUE)) {
      stop("Install the gsheet package to read SPOORKBOOK_SHEET_URL")
    }
    message("Reading Spoorkbook report from Google Sheet...")
    return(read.csv(
      text = gsheet2text(SPOORKBOOK_SHEET_URL, format = "csv"),
      stringsAsFactors = FALSE
    ))
  }

  if (!file.exists(SPOORKBOOK_REPORT_PATH)) {
    stop("Spoorkbook report not found at ", SPOORKBOOK_REPORT_PATH)
  }

  message("Reading Spoorkbook report from ", SPOORKBOOK_REPORT_PATH)
  read.csv(SPOORKBOOK_REPORT_PATH, stringsAsFactors = FALSE)
}

load_fade_result <- function(report = NULL) {
  if (is.null(report)) {
    report <- tryCatch(
      read_spoorkbook_report(),
      error = function(e) {
        message("Could not load Spoorkbook report: ", conditionMessage(e))
        return(NULL)
      }
    )
  }

  if (is.null(report) || nrow(report) == 0) {
    return(list(
      fade_teams = character(),
      fade_teams_short = character(),
      fade_result = NULL,
      report = NULL
    ))
  }

  fade_result <- score_fade_plays(report)
  fade_teams <- get_faded_teams(fade_result)

  list(
    fade_teams = fade_teams$full,
    fade_teams_short = fade_teams$short,
    fade_result = fade_result,
    report = report
  )
}
