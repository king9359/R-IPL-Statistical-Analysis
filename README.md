# IPL 2026 — Simple R Shiny Statistical Dashboard

This is a streamlined version of the original project. It retains the team dashboards, batting/bowling tables and charts, IPL-wide overview, and basic descriptive statistics/correlations.

## Run the project

1. Extract the ZIP folder.
2. Open the `.Rproj` file in RStudio.
3. Open `app.R`, then click **Run App** or run:

```r
shiny::runApp()
```

R automatically installs any missing packages: `shiny`, `dplyr`, `ggplot2`, `plotly`, and `DT`.

**Internet is required on the first run.** The starter files inside `data/` for matches, batting and bowling have headers but no match records. The app downloads actual match/scorecard CSVs from [IPLBuzz](https://github.com/PrashobhPaul/IPLBuzz) and caches them in `data/` for later offline use. The season coverage depends on what the source provides at the time of download.

## What's inside?

- `app.R`: Entire R Shiny application (data loading, calculations, charts and UI).
- `www/custom.css`: Website styling.
- `data/`: Automatically populated CSV cache (starter headers on first run).
- `.Rproj`: Open in RStudio.

## What was removed?

- Score-consistency box plot.
- Team Comparison page and its charts.
- Player-count slider, qualification-rule note and dataset explanation box.
- Project architecture section, plain-English/viva-style explanations and frontend demo notes.
- Unused points-table and ball-by-ball pipelines (including dot-ball/maiden columns).

Batting strike rate/average charts use players with at least five innings; bowling economy/average charts use players with at least ten overs. These fixed filters avoid misleading one-match comparisons without adding controls to the UI.

Statistics are calculated from season source records, not hard-coded. Incomplete/no-result scorecards are excluded from score distributions, and team league points are computed from completed league-stage games. Player aggregates may include playoffs, while league KPIs exclude them.

**Note:** The project was reviewed statically, but execution could not be verified here because R is not installed in this environment. If the external CSV source changes its schema or becomes unavailable, the first download will need an update.
