# Climate trends explorer

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![r](https://img.shields.io/badge/-r-blue)](https://github.com/topics/r)
[![shiny](https://img.shields.io/badge/-shiny-blue)](https://github.com/topics/shiny)
[![r-shiny](https://img.shields.io/badge/-r--shiny-blue)](https://github.com/topics/r-shiny)
[![dashboard](https://img.shields.io/badge/-dashboard-blue)](https://github.com/topics/dashboard)
[![climate-data](https://img.shields.io/badge/-climate--data-blue)](https://github.com/topics/climate-data)
[![climate-change](https://img.shields.io/badge/-climate--change-blue)](https://github.com/topics/climate-change)
[![weather-data](https://img.shields.io/badge/-weather--data-blue)](https://github.com/topics/weather-data)
[![knmi](https://img.shields.io/badge/-knmi-blue)](https://github.com/topics/knmi)
[![data-visualization](https://img.shields.io/badge/-data--visualization-blue)](https://github.com/topics/data-visualization)
[![meteorology](https://img.shields.io/badge/-meteorology-blue)](https://github.com/topics/meteorology)
[![ggplot2](https://img.shields.io/badge/-ggplot2-blue)](https://github.com/topics/ggplot2)
[![time-series-analysis](https://img.shields.io/badge/-time--series--analysis-blue)](https://github.com/topics/time-series-analysis)

A Shiny app for exploring long-term trends in a daily climate record.

**Live demo:** [tabuladata.shinyapps.io/climate-trends-shiny](https://tabuladata.shinyapps.io/climate-trends-shiny/)
(click "load demo data" to explore it right away, or upload your own CSV)

| Tab | What it does |
|-----|---------------|
| [Dashboard](#tab-dashboard) | Overview: stat cards, all-indicators trend grid, full summary table |
| [Trend](#tab-trend) | One variable's long-term trend, with linear/loess fit and significance tests |
| [Climatology](#tab-climatology) | Monthly boxplots; optional early-vs-late period comparison |
| [Year x month heatmap](#tab-year-x-month-heatmap) | Year x month anomaly grid vs. a baseline period |
| [Heat & cold waves](#tab-heat--cold-waves) | Detects heatwaves/coldwaves (KNMI De Bilt definition by default) |
| [Dry & wet spells](#tab-dry--wet-spells) | Detects dry/wet spells from rainfall |
| [Frost & ice days](#tab-frost--ice-days) | Annual counts of frost days and ice days |
| [Missing data](#tab-missing-data) | % missing per year, per variable |
| [Data](#tab-data) | Filtered raw daily rows, with download |

Expected columns (case-insensitive, NAs allowed anywhere):

| column   | meaning                                   |
|----------|--------------------------------------------|
| yyyymmdd | date, e.g. `19000101`                      |
| tg       | mean temperature                           |
| tx       | max temperature                            |
| tn       | min temperature                            |
| rh       | relative humidity (%)                      |
| rain     | precipitation (mm)                         |
| sq       | sunshine duration (hours)                  |

If your source data stores values as tenths (a common raw-export convention,
e.g. KNMI files), tick "Values are in tenths" in the app — it divides
tg/tx/tn/rh/rain/sq by 10 after loading.

Not all six columns are required — the app only offers variables that are
actually present in your data.frame.

## Using real KNMI daily data

The easiest way: the app itself has a "Load a KNMI station" picker on the
start screen (De Bilt 260, Rotterdam Airport 344) — pick one and click
"Load from KNMI" to download and load it directly, no R code needed.

For scripted use, or other stations, watch out for two
naming traps: KNMI's own column called `RH` is **daily precipitation in mm**,
not relative humidity — true mean relative humidity is a separate `UG`
column that isn't in every station's daily export. And `SQ` (sunshine
duration in hours) is not the same as `SP` (% of the longest possible
sunshine duration) — this app uses `SQ`. `R/climate_trend_app.R` includes a
loader that handles all of this (plus the `-1` "trace" sentinel used for
both precipitation, < 0.05 mm, and sunshine, < 0.05 hour) and returns data
already shaped the way this app expects:

```r
source("R/climate_trend_app.R")
weather_tbl <- fetch_knmi_daily(260)   # downloads + parses De Bilt (station 260)
climate_trend_app(weather_tbl)
```

Or, to parse a file you already downloaded: `read_knmi_daily("etmgeg_260.zip")`.

## Install dependencies

```r
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "lubridate", "scales", "DT"))
# only needed if you use fetch_knmi_daily() / read_knmi_daily():
install.packages(c("readr", "janitor"))
```

## Run it

**With a data.frame already in your R session:**

```r
setwd("climate-trends-shiny")
source("R/climate_trend_app.R")
climate_trend_app(your_data)   # your_data has the columns listed above
```

**As a standalone app** (RStudio "Run App" button, or):

```r
shiny::runApp("climate-trends-shiny")
```

This starts empty — upload a CSV, or click "load demo data" to explore the
interface first with a synthetic 1900-present record (includes a realistic
warming trend and a couple of simulated station gaps).

## What's in each tab

### <a id="tab-dashboard"></a>Dashboard

The overview: stat cards for data coverage and the headline trends (mean
temperature, rainfall, frost days, heatwave days), a small-multiples grid
plotting every available indicator's long-term trend at once, and a full
summary table (trend/decade, p-values, Kendall's tau) covering all of them
— temperature, humidity, rainfall, sunshine, heatwave/coldwave days,
dry/wet spell days, and frost/ice days. It reuses the exact same reactives
(and current threshold settings) as the dedicated tabs below, so it always
matches them; only indicators your data actually supports are shown.

### <a id="tab-trend"></a>Trend

The selected variable aggregated (Annual / Seasonal / Monthly /
Daily-with-rolling-mean) over your chosen year range, with a linear or
loess fit and both a linear-regression trend (per decade) and a
Mann-Kendall-style Kendall's tau test underneath.

### <a id="tab-climatology"></a>Climatology

Monthly boxplots of the raw daily values; optionally split into an early
vs. late sub-period to see how the seasonal cycle itself has shifted.

### <a id="tab-year-x-month-heatmap"></a>Year x month heatmap

Each cell is that month's anomaly vs. its own average over a baseline
period you choose.

### <a id="tab-heat--cold-waves"></a>Heat & cold waves

Detects heatwaves (from `tx`) and coldwaves (from `tn`) as runs of
consecutive days past a "mild" threshold that include enough days past a
stricter one. Defaults to KNMI's official De Bilt definition: a heatwave
is ≥5 consecutive days with tx ≥ 25°C, including ≥3 days with tx ≥ 30°C; a
coldwave is ≥5 consecutive days with tn ≤ 0°C, including ≥3 days with
tn ≤ -10°C — all four thresholds are adjustable. Shows a timeline of
detected events, days-per-year by type with a linear trend, and a
downloadable event table. Requires `tx` and/or `tn` in your data; a run
only counts if every day in it has real data — a missing day or a gap in
the record breaks the streak.

### <a id="tab-dry--wet-spells"></a>Dry & wet spells

Same run-detection idea applied to `rain`: a dry spell is N+ consecutive
days with rain at or below a threshold (default ≤1 mm for ≥10 days), a wet
spell is N+ consecutive days at or above one (default ≥1 mm for ≥5 days)
— both thresholds and run lengths are adjustable. Shows the same
timeline/annual-trend/event-table layout as Heat & cold waves; the event
table also reports total rainfall accumulated during each spell. Requires
`rain` in your data.

### <a id="tab-frost--ice-days"></a>Frost & ice days

Unlike the two tabs above, this counts individual days rather than
multi-day runs: a frost day is any day with tn below a threshold (default
0°C), an ice day is any day with tx below a threshold (default 0°C, i.e.
it never thawed). Shows days-per-year by type with a linear/Kendall trend
and a year-by-year count table. Requires `tn` and/or `tx` in your data.

### <a id="tab-missing-data"></a>Missing data

% missing per year for the current variable, plus a table of missingness
across all variables for the current selection.

### <a id="tab-data"></a>Data

The filtered daily rows, and a download button for them.

Aggregated and raw data can both be downloaded as CSV from their respective
tabs; the trend plot can be downloaded as PNG.

## Topics

| | | | |
|---|---|---|---|
| `r` | `shiny` | `r-shiny` | `dashboard` |
| `climate-data` | `climate-change` | `weather-data` | `knmi` |
| `data-visualization` | `meteorology` | `ggplot2` | `time-series-analysis` |

## License

[MIT](LICENSE)
