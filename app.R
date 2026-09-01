## Standalone launcher.
##
## Run this app in two ways:
##
## 1) With your own data.frame already in your R session:
##      source("R/climate_trend_app.R")
##      climate_trend_app(your_data)   # your_data needs: yyyymmdd, tg, tx, tn, rh, rain, sq
##
## 2) As a standalone app (e.g. via the "Run App" button in RStudio, or
##    shiny::runApp("climate-trends-shiny")) — it starts empty and lets you
##    upload a CSV, or click "load demo data" to explore the interface with
##    a synthetic 1900-present record first.

source("R/climate_trend_app.R")

climate_trend_app()
