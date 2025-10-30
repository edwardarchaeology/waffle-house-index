# Waffle House Index — Shiny App

A Shiny app that visualizes Waffle House locations and provides a simple Waffle House Index view for disaster assessment. The app shows current scraped locations (from locations.wafflehouse.com) and can overlay historical COVID-era closures (Wayback Machine snapshot) for comparison.

## Features

- Interactive MapLibre map of Waffle House locations
- Status-based styling (Green / Yellow / Red) according to the Waffle House Index
- Draw tools (lasso / rectangle) to select locations and export selections as CSV
- Toggle historical COVID closures (hide current points, show only historical closures)
- Collapsible informational blurb explaining the Waffle House Index and COVID context

## Repo layout

- `app.R` — main Shiny app
- `historical_data/` — contains COVID-era closure CSV (`covid_closures.csv`)
- Other scripts/resources may be in the project root

## Status code interpretation

The app maps raw `_status` values to a human-friendly `status_simple`:

- `_status == "A"` → `Open (Green)` — open, full menu
- `_status == "CT"` → `Closed Temporarily (Red)` — closed
- `formattedBusinessHours` containing "24" → `Open 24h` — 24-hour locations
- Has business hours but no explicit status → `Open (Yellow)` — limited menu / restricted hours
- Otherwise → `Status Unknown`

## Requirements

- R >= 4.0
- Required R packages (install if missing):

```r
install.packages(c(
  "shiny","mapgl","sf","dplyr","tidyr","purrr","rvest",
  "jsonlite","stringr","glue","DT","bslib","rsconnect"
))
```

- A modern browser with WebGL support (RStudio Viewer may have WebGL issues; use Chrome/Edge/Firefox)

## Local development / run

1. Open the project directory in RStudio or set working directory:

```r
setwd("c:/Users/C00607628/OneDrive - University of Louisiana at Lafayette/Documents/github_repos/WHI")
```

2. (Optional) Set MapTiler API key in your environment if you have one:

```r
Sys.setenv(MAPTILER_API_KEY = "your_maptiler_api_key")
```

If no key is provided, the app falls back to the public MapLibre demo style. 3. Run the app:

```r
shiny::runApp("app.R", launch.browser = TRUE)
```

## Deploy to shinyapps.io

1. Create an account at https://www.shinyapps.io/
2. Install and configure `rsconnect`:

```r
install.packages("rsconnect")
# Get the setAccountInfo call from shinyapps.io (Account → Tokens) and run it:
rsconnect::setAccountInfo(name='YOUR_NAME', token='YOUR_TOKEN', secret='YOUR_SECRET')
```

3. Deploy:

```r
rsconnect::deployApp(appDir = ".", appName = "waffle-house-index")
```

4. Add environment variable `MAPTILER_API_KEY` in the shinyapps.io app settings if you use MapTiler.

## Data notes

- Current location data is scraped from `https://locations.wafflehouse.com` at runtime.
- Historical COVID closures are read from `historical_data/covid_closures.csv`. Expected fields include `X` (lon), `Y` (lat), `status` and `ts` (timestamp). Update or replace this file to change the historical overlay.

## Troubleshooting

- Blank map: check browser console for errors. Common causes:
  - Map tiles/style failed to load (404 / CORS) — verify `MAPTILER_API_KEY` or use the default style.
  - WebGL unsupported or blocked — try an external browser.
  - JS errors: open DevTools Console (F12) and inspect messages like `maplibregl is not defined` or resource fetch failures.
- Selection shows no records when historical layer is active:
  - The app filters based on the visible layer; ensure you toggle the COVID layer correctly (button text indicates state).
  - Confirm `historical_data/covid_closures.csv` exists and contains `X`, `Y`, and `status`.

## Development notes & TODOs

- Consider caching scraped location data to avoid frequent live scrapes.
- Add unit tests for data parsing functions.
- Improve error handling for missing or malformed historical data.

## License

MIT — add a LICENSE file if needed.

## Contact

Repository maintainer: update with your contact info
