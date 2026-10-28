# IPL 2026 Dashboard — simplified R Shiny project
# Run from this folder in RStudio: shiny::runApp()

packages <- c("shiny", "dplyr", "ggplot2", "plotly", "DT")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))

teams <- c(CSK = "Chennai Super Kings", DC = "Delhi Capitals",
           GT = "Gujarat Titans", KKR = "Kolkata Knight Riders",
           LSG = "Lucknow Super Giants", MI = "Mumbai Indians",
           PBKS = "Punjab Kings", RR = "Rajasthan Royals",
           RCB = "Royal Challengers Bengaluru", SRH = "Sunrisers Hyderabad")
team_choices <- names(teams)
names(team_choices) <- unname(teams)

# Only three scorecard files are needed; the supplied starter CSVs contain headers.
urls <- c(
  matches = "https://raw.githubusercontent.com/PrashobhPaul/IPLBuzz/main/Schedule.csv",
  batting = "https://raw.githubusercontent.com/PrashobhPaul/IPLBuzz/main/Batting.csv",
  bowling = "https://raw.githubusercontent.com/PrashobhPaul/IPLBuzz/main/Bowling.csv"
)
columns <- list(
  matches = c("no", "t1", "t2", "completed", "nr", "result", "t1score", "t2score", "stage"),
  batting = c("match_no", "innings", "batting_team", "player", "runs", "balls", "dismissal"),
  bowling = c("match_no", "innings", "bowling_team", "player", "overs", "runs", "wickets")
)

valid_csv <- function(file, type) {
  if (!file.exists(file)) return(FALSE)
  tryCatch({
    x <- read.csv(file, nrows = 2, check.names = FALSE)
    nrow(x) > 0 && all(columns[[type]] %in% tolower(names(x)))
  }, error = function(e) FALSE)
}

load_csv <- function(type) {
  file <- file.path("data", paste0(type, "_2026.csv"))
  if (!valid_csv(file, type)) {
    dir.create("data", showWarnings = FALSE)
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    ok <- tryCatch({
      suppressWarnings(download.file(urls[[type]], tmp, mode = "wb", quiet = TRUE))
      valid_csv(tmp, type)
    }, error = function(e) FALSE)
    if (!ok || !file.copy(tmp, file, overwrite = TRUE)) {
      stop("Unable to load ", type, " data. Connect to the internet and rerun the app.")
    }
  }
  x <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- tolower(names(x))
  x
}

# Read and prepare the season data only once when the app starts.
raw_matches <- load_csv("matches")
raw_batting <- load_csv("batting")
raw_bowling <- load_csv("bowling")

score_number <- function(x) suppressWarnings(as.numeric(sub("/.*$", "", as.character(x))))
to_number <- function(x) suppressWarnings(as.numeric(x))
# Cricket 3.4 overs means 3 overs + 4 balls, not 3.4 decimal overs.
overs_balls <- function(x) {
  s <- strsplit(as.character(x), ".", fixed = TRUE)
  vapply(s, function(z) {
    if (length(z) == 0 || is.na(z[1])) return(NA_real_)
    whole <- to_number(z[1]); part <- if (length(z) > 1) to_number(z[2]) else 0
    if (is.na(whole) || is.na(part) || part > 5) return(NA_real_)
    whole * 6 + part
  }, numeric(1))
}

matches <- raw_matches %>% transmute(
  match_no = as.integer(no), team1 = trimws(t1), team2 = trimws(t2),
  score1 = score_number(t1score), score2 = score_number(t2score),
  result = as.character(result),
  completed = toupper(as.character(completed)) == "TRUE",
  no_result = toupper(as.character(nr)) == "TRUE",
  stage = ifelse(is.na(stage) | trimws(stage) == "", "League", stage)
) %>% filter(!is.na(match_no), completed)

batting <- raw_batting %>% transmute(
  match_no = as.integer(match_no), innings = as.integer(innings),
  team = trimws(batting_team), player = trimws(player),
  runs = to_number(runs), balls = to_number(balls),
  fours = to_number(fours), sixes = to_number(sixes),
  dismissal = as.character(dismissal)
) %>% distinct(match_no, innings, team, player, .keep_all = TRUE)

bowling <- raw_bowling %>% transmute(
  match_no = as.integer(match_no), innings = as.integer(innings),
  team = trimws(bowling_team), player = trimws(player),
  balls = overs_balls(overs), runs = to_number(runs), wickets = to_number(wickets)
) %>% distinct(match_no, innings, team, player, .keep_all = TRUE)

league <- matches %>% filter(stage == "League")
# Each match contributes one row per team. Abandoned games get one point.
team_games <- bind_rows(
  league %>% transmute(match_no, team = team1, score = score1,
                       conceded = score2, result, no_result),
  league %>% transmute(match_no, team = team2, score = score2,
                       conceded = score1, result, no_result)
) %>% mutate(
  win = !no_result & team == sub(" won.*$", "", result),
  loss = !no_result & !win,
  points = ifelse(win, 2L, ifelse(no_result, 1L, 0L)),
  valid_score = ifelse(no_result, NA_real_, score)
)

standings <- team_games %>% group_by(team) %>% summarise(
  played = n(), wins = sum(win), losses = sum(loss),
  points = sum(points), win_pct = 100 * mean(win),
  avg_score = mean(valid_score, na.rm = TRUE),
  highest = if (all(is.na(valid_score))) NA_real_ else max(valid_score, na.rm = TRUE),
  .groups = "drop"
)

batting_stats <- batting %>% group_by(team, player) %>% summarise(
  matches = n_distinct(match_no), innings = n(), runs = sum(runs, na.rm = TRUE),
  balls = sum(balls, na.rm = TRUE), fours = sum(fours, na.rm = TRUE),
  sixes = sum(sixes, na.rm = TRUE),
  outs = sum(!is.na(dismissal) & dismissal != "" &
               !grepl("not out", dismissal, ignore.case = TRUE)),
  highest = max(runs, na.rm = TRUE), .groups = "drop"
) %>% mutate(average = ifelse(outs > 0, runs / outs, NA_real_),
             strike_rate = ifelse(balls > 0, 100 * runs / balls, NA_real_)) %>%
  arrange(desc(runs))

bowling_stats <- bowling %>% group_by(team, player) %>% summarise(
  matches = n_distinct(match_no), balls = sum(balls, na.rm = TRUE),
  runs = sum(runs, na.rm = TRUE), wickets = sum(wickets, na.rm = TRUE),
  .groups = "drop"
) %>% mutate(
  overs = paste0(balls %/% 6, ".", balls %% 6),
  economy = ifelse(balls > 0, 6 * runs / balls, NA_real_),
  average = ifelse(wickets > 0, runs / wickets, NA_real_)
) %>% arrange(desc(wickets), economy)

# Small reusable helpers for the tables and graphs.
number <- function(x, digits = 0) ifelse(is.finite(x),
  formatC(x, digits = digits, format = "f", big.mark = ","), "—")
summary_row <- function(label, x) {
  x <- x[is.finite(x)]
  data.frame(Metric = label, Mean = if (length(x)) mean(x) else NA_real_,
             Median = if (length(x)) median(x) else NA_real_,
             Minimum = if (length(x)) min(x) else NA_real_,
             Maximum = if (length(x)) max(x) else NA_real_,
             SD = if (length(x) > 1) sd(x) else NA_real_,
             Observations = length(x))
}
safe_cor <- function(x, y) {
  good <- is.finite(x) & is.finite(y)
  if (sum(good) < 3 || length(unique(x[good])) < 2 ||
      length(unique(y[good])) < 2) return(NA_real_)
  cor(x[good], y[good])
}
bar_chart <- function(data, value, y_label) {
  data <- data[is.finite(data[[value]]), ] %>%
    slice_max(order_by = .data[[value]], n = 8, with_ties = FALSE)
  validate(need(nrow(data) > 0, "No data available."))
  ggplotly(ggplot(data, aes(x = reorder(player, .data[[value]]),
                          y = .data[[value]])) +
             geom_col(fill = "#2d79b9") + coord_flip() +
             labs(x = NULL, y = y_label) + theme_minimal())
}

# --------------------------- Shiny UI ----------------------------
team_cards <- lapply(names(teams), function(code) {
  div(class = "team-card-shell",
      actionButton(paste0("team_", code),
                   div(span(class = "team-code", code),
                       span(class = "team-name", teams[[code]])),
                   class = "team-card", width = "100%"))
})

ui <- navbarPage(
  title = div(class = "brand-title", "IPL 2026 Analysis"), id = "pages",
  header = tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
                     tags$link(rel = "stylesheet", href = "custom.css")),
  tabPanel("Home", value = "home",
           tags$main(class = "page-wrap",
                tags$section(class = "hero-wrap", h1("IPL 2026 Statistical Analysis"),
                        p("Team performance, batting, bowling and tournament statistics.")),
                tags$section(class = "panel-card", h2("Select a team"),
                        div(class = "team-grid", team_cards)))),
  tabPanel("Team Dashboard", value = "team",
           div(class = "page-wrap",
               selectInput("team", "Select team", choices = team_choices, selected = "RCB"),
               uiOutput("team_title"), uiOutput("team_kpis"),
               tabsetPanel(
                 tabPanel("Scores",
                          div(class = "panel-card", h3("Match-by-match scores"),
                              plotlyOutput("scores_chart", height = 330)),
                          fluidRow(
                            column(6, div(class = "panel-card", h3("Score distribution"),
                                          plotlyOutput("scores_hist", height = 320))),
                            column(6, div(class = "panel-card", h3("Score summary"),
                                          DTOutput("score_table"))))),
                 tabPanel("Batting",
                          div(class = "panel-card", h3("Batting statistics"), DTOutput("batting_table")),
                          div(class = "panel-card", uiOutput("top_batter")),
                          fluidRow(
                            column(4, div(class = "panel-card", h3("Runs"), plotlyOutput("runs_chart"))),
                            column(4, div(class = "panel-card", h3("Strike rate"), plotlyOutput("strike_chart"))),
                            column(4, div(class = "panel-card", h3("Average"), plotlyOutput("bat_avg_chart"))))),
                 tabPanel("Bowling",
                          div(class = "panel-card", h3("Bowling statistics"), DTOutput("bowling_table")),
                          div(class = "panel-card", uiOutput("top_bowler")),
                          fluidRow(
                            column(4, div(class = "panel-card", h3("Wickets"), plotlyOutput("wickets_chart"))),
                            column(4, div(class = "panel-card", h3("Economy"), plotlyOutput("economy_chart"))),
                            column(4, div(class = "panel-card", h3("Bowling average"), plotlyOutput("bowl_avg_chart"))))),
                 tabPanel("Statistics",
                          fluidRow(
                            column(6, div(class = "panel-card", h3("Team score statistics"),
                                          DTOutput("team_stats_table"))),
                            column(6, div(class = "panel-card", h3("Player statistics"),
                                          DTOutput("player_stats_table")))),
                          div(class = "panel-card", h3("Correlations"), DTOutput("correlation_table")))
               ))),
  tabPanel("IPL 2026 Overview", value = "overview",
           div(class = "page-wrap", h1("IPL 2026 Overview"),
               uiOutput("season_kpis"),
               fluidRow(
                 column(6, div(class = "panel-card", h3("Top run scorers"), DTOutput("top_batting_table"))),
                 column(6, div(class = "panel-card", h3("Top wicket-takers"), DTOutput("top_bowling_table")))),
               div(class = "panel-card", h3("Tournament score distribution"),
                   plotlyOutput("season_hist", height = 330)))),
  tabPanel("Data Sources", value = "sources",
           div(class = "page-wrap", h1("Data Sources"),
               div(class = "panel-card",
                   p("All statistics are calculated from the CSV data used by this project."),
                   tags$ul(lapply(names(urls), function(nm) {
                     tags$li(tags$a(href = urls[[nm]], target = "_blank", nm))
                   })),
                   p("The app saves downloaded files in the data folder for later runs."))))
)

# ------------------------- Shiny server ---------------------------
server <- function(input, output, session) {
  for (code in names(teams)) local({
    selected <- code
    observeEvent(input[[paste0("team_", selected)]], {
      updateSelectInput(session, "team", selected = selected)
      updateNavbarPage(session, "pages", selected = "team")
    })
  })

  games <- reactive(team_games %>% filter(team == input$team, !no_result,
                                          is.finite(score)) %>% arrange(match_no))
  bat <- reactive(batting_stats %>% filter(team == input$team))
  bowl <- reactive(bowling_stats %>% filter(team == input$team))

  kpi <- function(label, value) div(class = "kpi-card", span(class = "kpi-label", label),
                                     div(class = "kpi-value", value))
  table_show <- function(data, rounding = NULL) {
    out <- DT::datatable(data, rownames = FALSE,
                         options = list(pageLength = 10, scrollX = TRUE))
    if (!is.null(rounding)) out <- DT::formatRound(out, rounding, 2)
    out
  }
  output$team_title <- renderUI(h2(teams[[input$team]]))
  output$team_kpis <- renderUI({
    x <- standings %>% filter(team == input$team)
    if (!nrow(x)) return(p("No matches available."))
    div(class = "kpi-grid", kpi("Matches", x$played), kpi("Wins", x$wins),
        kpi("Losses", x$losses), kpi("Points", x$points),
        kpi("Win %", paste0(number(x$win_pct, 1), "%")),
        kpi("Average score", number(x$avg_score, 1)))
  })

  output$scores_chart <- renderPlotly({
    x <- games(); validate(need(nrow(x) > 0, "No scores available."))
    ggplotly(ggplot(x, aes(x = factor(match_no), y = score)) +
               geom_col(fill = "#2d79b9") + labs(x = "Match number", y = "Runs") + theme_minimal())
  })
  output$scores_hist <- renderPlotly({
    x <- games(); validate(need(nrow(x) > 1, "No scores available."))
    ggplotly(ggplot(x, aes(x = score)) + geom_histogram(bins = 7, fill = "#2d79b9") +
               labs(x = "Team score", y = "Matches") + theme_minimal())
  })
  output$score_table <- renderDT(table_show(summary_row("Team scores", games()$score),
                                            c("Mean", "Median", "Minimum", "Maximum", "SD")))

  output$batting_table <- renderDT({
    x <- bat() %>% select(Player = player, Matches = matches, Innings = innings,
                          Runs = runs, Average = average, `Strike Rate` = strike_rate,
                          `Highest Score` = highest, `4s` = fours, `6s` = sixes)
    table_show(x, c("Average", "Strike Rate"))
  })
  output$bowling_table <- renderDT({
    x <- bowl() %>% select(Player = player, Matches = matches, Overs = overs,
                           Wickets = wickets, `Runs Conceded` = runs,
                           Economy = economy, Average = average)
    table_show(x, c("Economy", "Average"))
  })
  output$top_batter <- renderUI({
    x <- bat(); if (!nrow(x)) return(p("No batting records available."))
    h3("Top run scorer: ", x$player[1], " — ", number(x$runs[1]), " runs")
  })
  output$top_bowler <- renderUI({
    x <- bowl(); if (!nrow(x)) return(p("No bowling records available."))
    h3("Leading wicket-taker: ", x$player[1], " — ", number(x$wickets[1]), " wickets")
  })
  output$runs_chart <- renderPlotly(bar_chart(bat(), "runs", "Runs"))
  output$strike_chart <- renderPlotly(bar_chart(bat() %>% filter(innings >= 5),
                                              "strike_rate", "Strike rate"))
  output$bat_avg_chart <- renderPlotly(bar_chart(bat() %>% filter(innings >= 5),
                                               "average", "Batting average"))
  output$wickets_chart <- renderPlotly(bar_chart(bowl(), "wickets", "Wickets"))
  output$economy_chart <- renderPlotly(bar_chart(bowl() %>% filter(balls >= 60),
                                               "economy", "Economy"))
  output$bowl_avg_chart <- renderPlotly(bar_chart(bowl() %>% filter(balls >= 60),
                                                "average", "Bowling average"))

  output$team_stats_table <- renderDT(table_show(summary_row("Team scores", games()$score),
                                                 c("Mean", "Median", "Minimum", "Maximum", "SD")))
  output$player_stats_table <- renderDT({
    x <- bind_rows(summary_row("Batting runs", bat()$runs),
                   summary_row("Bowling wickets", bowl()$wickets))
    table_show(x, c("Mean", "Median", "Minimum", "Maximum", "SD"))
  })
  output$correlation_table <- renderDT({
    hitters <- bat() %>% filter(innings >= 5)
    bowlers <- bowl() %>% filter(balls >= 60)
    x <- data.frame(Relationship = c("Runs vs strike rate", "Wickets vs economy"),
                    Correlation = c(safe_cor(hitters$runs, hitters$strike_rate),
                                    safe_cor(bowlers$wickets, bowlers$economy)))
    table_show(x, "Correlation")
  })

  output$season_kpis <- renderUI({
    x <- matches %>% filter(!no_result)
    all_scores <- c(x$score1, x$score2)
    div(class = "kpi-grid", kpi("Matches", nrow(matches)),
        kpi("Completed with result", nrow(x)),
        kpi("Total runs", number(sum(all_scores, na.rm = TRUE))),
        kpi("Wickets", number(sum(bowling$wickets, na.rm = TRUE))),
        kpi("Highest score", number(if (any(is.finite(all_scores)))
                                     max(all_scores, na.rm = TRUE) else NA_real_)))
  })
  output$top_batting_table <- renderDT({
    x <- batting_stats %>% head(12) %>% select(Player = player, Team = team,
                                                Runs = runs, Average = average)
    table_show(x, "Average")
  })
  output$top_bowling_table <- renderDT({
    x <- bowling_stats %>% head(12) %>% select(Player = player, Team = team,
                                                Wickets = wickets, Economy = economy)
    table_show(x, "Economy")
  })
  output$season_hist <- renderPlotly({
    x <- matches %>% filter(!no_result)
    scores <- c(x$score1, x$score2)
    scores <- scores[is.finite(scores)]
    validate(need(length(scores) > 1, "No scores available."))
    ggplotly(ggplot(data.frame(score = scores), aes(x = score)) +
               geom_histogram(bins = 12, fill = "#2d79b9") +
               labs(x = "Innings score", y = "Innings") + theme_minimal())
  })
}

shinyApp(ui, server)
