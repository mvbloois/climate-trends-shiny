## climate_trend_app.R
##
## A Shiny app for exploring long-term trends in a daily climate record with
## columns: yyyymmdd, tg (mean temp), tx (max temp), tn (min temp), rh
## (relative humidity), rain (precipitation, mm), sq (sunshine duration,
## hours). See VAR_DEFS below for the canonical mapping used in this app.
## NAs are handled throughout.

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)

has_DT <- requireNamespace("DT", quietly = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# ---- Variable metadata -------------------------------------------------

VAR_DEFS <- list(
  tg   = list(label = "Mean temperature (tg)", unit = "°C", default_stat = "mean"),
  tx   = list(label = "Max temperature (tx)",  unit = "°C", default_stat = "mean"),
  tn   = list(label = "Min temperature (tn)",  unit = "°C", default_stat = "mean"),
  rh   = list(label = "Relative humidity (rh)", unit = "%",      default_stat = "mean"),
  rain = list(label = "Rain (rain)",            unit = "mm",     default_stat = "sum"),
  sq   = list(label = "Sunshine duration (sq)", unit = "hours",  default_stat = "sum")
)

SEASON_COLORS <- c(Winter = "#3182bd", Spring = "#31a354", Summer = "#e6550d", Autumn = "#8c510a")
SEASON_TINTS  <- c(Winter = "#eaf3fb", Spring = "#eaf5ec", Summer = "#fdece0", Autumn = "#f3e9df")
MONTH_SEASON <- c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer",
                   "Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter")

STAT_FUNS <- list(
  mean   = function(x) mean(x, na.rm = TRUE),
  sum    = function(x) sum(x, na.rm = TRUE),
  min    = function(x) suppressWarnings(min(x, na.rm = TRUE)),
  max    = function(x) suppressWarnings(max(x, na.rm = TRUE)),
  median = function(x) median(x, na.rm = TRUE)
)

# ---- Data preparation ---------------------------------------------------

# Map incoming column names (any case / a few common synonyms) onto the
# canonical names this app expects.
standardize_names <- function(df) {
  synonyms <- list(
    yyyymmdd = c("yyyymmdd", "date", "datum"),
    tg       = c("tg", "temp_mean", "tmean"),
    tx       = c("tx", "temp_max", "tmax"),
    tn       = c("tn", "temp_min", "tmin"),
    rh       = c("rh", "humidity", "relhum"),
    rain     = c("rain", "rr", "prcp", "precip", "precipitation"),
    sq       = c("sq", "sunshine", "sunhours", "sun_hours")
  )
  nm <- tolower(trimws(names(df)))
  for (canon in names(synonyms)) {
    hit <- match(TRUE, nm %in% synonyms[[canon]])
    if (!is.na(hit)) {
      names(df)[nm %in% synonyms[[canon]]][1] <- canon
    }
  }
  df
}

# Replace configured sentinel values with real NA, across the measurement
# columns only (never touches the date column).
coerce_na <- function(df, na_strings) {
  na_strings <- trimws(na_strings)
  na_strings <- na_strings[nzchar(na_strings)]
  if (length(na_strings) == 0) return(df)
  meas_cols <- intersect(names(VAR_DEFS), names(df))
  for (col in meas_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    df[[col]][as.character(df[[col]]) %in% na_strings] <- NA
    num_sentinels <- suppressWarnings(as.numeric(na_strings))
    num_sentinels <- num_sentinels[!is.na(num_sentinels)]
    if (length(num_sentinels) > 0) {
      df[[col]][df[[col]] %in% num_sentinels] <- NA
    }
  }
  df
}

# Parse dates, add calendar helper columns, coerce measurement columns to
# numeric, and optionally rescale tenths-of-a-unit values (the common raw
# export convention) down to natural units.
prepare_data <- function(df, divide_by_10 = FALSE) {
  df <- standardize_names(df)
  if (!"yyyymmdd" %in% names(df)) {
    stop("Could not find a yyyymmdd (or 'date') column in the data.")
  }

  if (inherits(df$yyyymmdd, "Date")) {
    df$date <- df$yyyymmdd
  } else if (inherits(df$yyyymmdd, "POSIXt")) {
    df$date <- as.Date(df$yyyymmdd)
  } else {
    date_chr <- trimws(as.character(df$yyyymmdd))
    df$date <- as.Date(date_chr, format = "%Y%m%d")
  }

  meas_cols <- intersect(names(VAR_DEFS), names(df))
  for (col in meas_cols) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))

  if (divide_by_10) {
    scale_cols <- intersect(c("tg", "tx", "tn", "rain", "rh", "sq"), meas_cols)
    for (col in scale_cols) df[[col]] <- df[[col]] / 10
  }

  df <- df[!is.na(df$date), , drop = FALSE]
  df <- df[order(df$date), , drop = FALSE]

  df$year  <- year(df$date)
  df$month <- month(df$date)
  df$doy   <- yday(df$date)
  df$season <- factor(MONTH_SEASON[df$month], levels = c("Winter", "Spring", "Summer", "Autumn"))
  # Meteorological winter (Dec-Jan-Feb) is grouped under the following year.
  df$season_year <- ifelse(df$month == 12, df$year + 1, df$year)

  df
}

# ---- Rolling mean (base R, vectorised, NA-aware) -------------------------

roll_mean <- function(x, k) {
  n <- length(x)
  if (k <= 1) return(x)
  half <- k %/% 2
  x0 <- ifelse(is.na(x), 0, x)
  cnt <- as.integer(!is.na(x))
  cx <- c(0, cumsum(x0))
  cc <- c(0, cumsum(cnt))
  idx <- seq_len(n)
  lo <- pmax(1, idx - half)
  hi <- pmin(n, idx + half)
  s <- cx[hi + 1] - cx[lo]
  c_ <- cc[hi + 1] - cc[lo]
  ifelse(c_ == 0, NA_real_, s / c_)
}

# ---- Aggregation ----------------------------------------------------------

aggregate_series <- function(df, var, period, stat, months_filter, roll_days = 30) {
  statf <- STAT_FUNS[[stat]]
  d <- df[df$month %in% months_filter, , drop = FALSE]

  if (period == "Annual") {
    out <- d %>%
      group_by(year) %>%
      summarise(
        value  = statf(.data[[var]]),
        n      = sum(!is.na(.data[[var]])),
        n_tot  = n(),
        .groups = "drop"
      ) %>%
      mutate(x = year, x_label = as.character(year))
  } else if (period == "Seasonal") {
    out <- d %>%
      group_by(season_year, season) %>%
      summarise(
        value  = statf(.data[[var]]),
        n      = sum(!is.na(.data[[var]])),
        n_tot  = n(),
        .groups = "drop"
      ) %>%
      mutate(year = season_year, x = season_year + (as.integer(season) - 1) / 4,
             x_label = paste(season, season_year))
  } else if (period == "Monthly") {
    out <- d %>%
      group_by(year, month) %>%
      summarise(
        value  = statf(.data[[var]]),
        n      = sum(!is.na(.data[[var]])),
        n_tot  = n(),
        .groups = "drop"
      ) %>%
      mutate(x = year + (month - 0.5) / 12, x_label = sprintf("%d-%02d", year, month))
  } else { # Daily
    d <- d %>% arrange(date)
    d$value_smooth <- roll_mean(d[[var]], roll_days)
    out <- d %>%
      transmute(
        year, x = year + doy / 365.25, x_label = as.character(date),
        value = .data[[var]], value_smooth, n = as.integer(!is.na(.data[[var]])), n_tot = 1
      )
  }
  out
}

# ---- Trend statistics -------------------------------------------------

trend_stats <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  if (n < 8) {
    return(list(n = n, ok = FALSE))
  }
  fit <- lm(y ~ x)
  slope <- unname(coef(fit)[2])
  p_lm <- summary(fit)$coefficients[2, 4]
  kt <- tryCatch(cor.test(x, y, method = "kendall"), error = function(e) NULL)
  list(
    n = n, ok = TRUE,
    slope_per_decade = slope * 10,
    p_lm = p_lm,
    tau = if (!is.null(kt)) unname(kt$estimate) else NA_real_,
    p_kendall = if (!is.null(kt)) kt$p.value else NA_real_
  )
}

fmt_p <- function(p) if (is.na(p)) "NA" else if (p < 0.001) "<0.001" else sprintf("%.3f", p)

# Zero-filled annual day-count series for one event `type` out of an events
# table shaped like heat_cold_events()/dry_wet_events() (columns: type,
# start, duration, ...) — every year in `yrs` gets a row, 0 if no events.
annual_event_days <- function(events, type_name, yrs) {
  if (is.null(events) || nrow(events) == 0) return(data.frame(year = yrs, value = 0))
  sub <- events[events$type == type_name, , drop = FALSE]
  data.frame(year = yrs, value = sapply(yrs, function(y) sum(sub$duration[year(sub$start) == y])))
}

# ---- Spell detection (heatwaves, coldwaves, dry spells, wet spells) ------
#
# Finds runs of consecutive days meeting a "mild" threshold (e.g. tx >= 25)
# that are at least `min_run` days long, and keeps only the runs that also
# contain at least `min_extreme` days meeting a stricter threshold (e.g.
# tx >= 30) — the standard shape of a heatwave/coldwave definition (KNMI's
# official De Bilt definition uses exactly these defaults: a heatwave is
# >=5 consecutive days with tx >= 25 including >=3 days with tx >= 30; a
# coldwave is >=5 consecutive days with tn <= 0 including >=3 days with
# tn <= -10). A day with a missing value never counts towards a run, which
# breaks it just like an actual gap in the record would.
#
# Dry/wet spells only need a single threshold, not the two-threshold shape
# above — call with threshold2 = threshold1 and min_extreme = 0 to get plain
# "N+ consecutive days past this threshold" detection.
#
# `dates` need not be a complete daily sequence — missing calendar days are
# filled in (as NA) before run-detection, so a gap in the data correctly
# breaks a streak rather than silently bridging it.
detect_extreme_spells <- function(dates, values, threshold1, threshold2, comparator,
                                   min_run = 5, min_extreme = 3) {
  ok <- !is.na(dates)
  dates <- dates[ok]; values <- values[ok]
  empty <- data.frame(start = as.Date(character()), end = as.Date(character()),
                       duration = integer(), n_extreme = integer(),
                       peak = numeric(), mean_value = numeric(), total = numeric())
  if (length(dates) == 0) return(empty)

  full_dates <- seq(min(dates), max(dates), by = "day")
  full_vals <- values[match(full_dates, dates)]

  cond1 <- if (comparator == "ge") full_vals >= threshold1 else full_vals <= threshold1
  cond2 <- if (comparator == "ge") full_vals >= threshold2 else full_vals <= threshold2
  cond1[is.na(cond1)] <- FALSE
  cond2[is.na(cond2)] <- FALSE

  r <- rle(cond1)
  run_ends <- cumsum(r$lengths)
  run_starts <- run_ends - r$lengths + 1
  candidates <- which(r$values & r$lengths >= min_run)
  if (length(candidates) == 0) return(empty)

  rows <- lapply(candidates, function(i) {
    idx <- run_starts[i]:run_ends[i]
    n_extreme <- sum(cond2[idx])
    if (n_extreme < min_extreme) return(NULL)
    vals <- full_vals[idx]
    data.frame(
      start = full_dates[run_starts[i]], end = full_dates[run_ends[i]],
      duration = length(idx), n_extreme = n_extreme,
      peak = if (comparator == "ge") max(vals, na.rm = TRUE) else min(vals, na.rm = TRUE),
      mean_value = mean(vals, na.rm = TRUE), total = sum(vals, na.rm = TRUE)
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(empty)
  do.call(rbind, rows)
}

# ---- KNMI daily-data loader -------------------------------------------
#
# KNMI's own column called "RH" is daily precipitation in mm, NOT relative
# humidity (true mean relative humidity is in a separate "UG" column, which
# isn't always present in every station's daily export). "SQ" is sunshine
# duration in hours (not to be confused with "SP", the percentage of the
# longest possible sunshine duration, which this app does not use). These
# helpers parse KNMI's etmgeg_<station>.zip format directly into the column
# names this app expects (tg, tx, tn, rh, rain, sq), so "rh" here really is
# humidity and "rain" really is precipitation. They also correctly handle
# KNMI's trace-value sentinel (-1, meaning "measured but below the smallest
# reportable amount": < 0.05 mm for rain, < 0.05 hour for sunshine), applied
# *before* scaling to natural units.

#' Download a KNMI daily station file and parse it for this app.
#'
#' @param station KNMI station number, e.g. 260 (De Bilt).
#' @param dest Where to save the downloaded .zip. Defaults to a temp file.
fetch_knmi_daily <- function(station = 260, dest = NULL) {
  if (is.null(dest)) dest <- tempfile(fileext = ".zip")
  url <- sprintf(
    "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_%s.zip",
    station
  )
  download.file(url, dest, mode = "wb", quiet = TRUE)
  read_knmi_daily(dest)
}

#' Parse an already-downloaded KNMI daily file (.zip, or the .txt inside it).
read_knmi_daily <- function(path) {
  if (!requireNamespace("readr", quietly = TRUE) || !requireNamespace("janitor", quietly = TRUE)) {
    stop("read_knmi_daily() needs the 'readr' and 'janitor' packages installed.")
  }
  raw <- readr::read_delim(path, delim = ",", skip = 50, show_col_types = FALSE)
  raw <- janitor::clean_names(raw)

  precip_raw <- as.numeric(raw$rh)  # KNMI's "RH" = precipitation (0.1 mm); -1 = trace, < 0.05 mm
  sun_raw <- if ("sq" %in% names(raw)) as.numeric(raw$sq) else NA_real_  # "SQ" = sunshine (0.1 hour); -1 = trace, < 0.05 hour

  out <- data.frame(
    yyyymmdd = lubridate::ymd(raw$yyyymmdd),
    tg = as.numeric(raw$tg) / 10,
    tx = as.numeric(raw$tx) / 10,
    tn = as.numeric(raw$tn) / 10,
    rain = ifelse(precip_raw == -1, 0, precip_raw / 10),
    sq = ifelse(sun_raw == -1, 0, sun_raw / 10)
  )
  # True relative humidity ("UG") isn't in every station's daily export.
  out$rh <- if ("ug" %in% names(raw)) as.numeric(raw$ug) else NA_real_

  out[, c("yyyymmdd", "tg", "tx", "tn", "rh", "rain", "sq")]
}

# ---- Demo data (used when no real data.frame / upload is available) ----

make_demo_data <- function(end_date = Sys.Date()) {
  set.seed(42)
  dates <- seq(as.Date("1900-01-01"), end_date, by = "day")
  n <- length(dates)
  doy <- yday(dates)
  yr  <- year(dates)
  t <- (as.numeric(dates) - as.numeric(as.Date("1900-01-01"))) / 365.25

  seasonal <- -cos(2 * pi * doy / 365.25)
  warming <- 0.018 * t  # ~1.8 degC over 100 years, roughly realistic

  tg <- 10 + 8 * seasonal + warming + rnorm(n, 0, 1.8)
  tx <- tg + 4 + rnorm(n, 0, 1.2)
  tn <- tg - 4 + rnorm(n, 0, 1.2)
  rh <- pmin(100, pmax(0, 75 - 8 * seasonal + rnorm(n, 0, 6)))
  rain_day <- rbinom(n, 1, 0.35 + 0.05 * seasonal)
  rain <- rain_day * rexp(n, rate = 1 / 3)
  sq <- pmin(16, pmax(0, 4 + 3.5 * seasonal + rnorm(n, 0, 2)))

  df <- data.frame(
    yyyymmdd = as.integer(format(dates, "%Y%m%d")),
    tg = round(tg, 1), tx = round(tx, 1), tn = round(tn, 1),
    rh = round(rh, 1), rain = round(rain, 1), sq = round(sq, 1)
  )

  # Sprinkle in some missing data: scattered single days plus a couple of
  # multi-year station gaps, similar to real historical records.
  scattered <- sample(seq_len(n), size = round(0.01 * n))
  for (col in c("tg", "tx", "tn", "rh", "rain", "sq")) df[[col]][scattered] <- NA
  gap1 <- which(yr >= 1940 & yr <= 1944)
  if (length(gap1) > 0) df$sq[sample(gap1, size = round(0.6 * length(gap1)))] <- NA
  gap2 <- which(yr >= 1917 & yr <= 1918)
  if (length(gap2) > 0) df[gap2, c("tg","tx","tn","rh","rain","sq")] <- NA

  df
}

# ---- The app --------------------------------------------------------------

#' Launch the climate trend explorer
#'
#' @param data Optional data.frame already in your R session, with columns
#'   yyyymmdd, tg, tx, tn, rh, rain, sq (NAs allowed). If omitted, the app
#'   starts empty and lets you upload a CSV or load demo data instead.
#' @export
climate_trend_app <- function(data = NULL) {

  ui <- fluidPage(
    titlePanel("Long-term climate trends"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        conditionalPanel(
          condition = "!output.has_data",
          h4("Load data"),
          fileInput("file", "Upload CSV",
                    accept = c(".csv", ".txt", "text/csv")),
          radioButtons("sep", "Column separator", choices = c("," = ",", ";" = ";", "tab" = "\t"), selected = ","),
          textInput("na_strings", "Treat these as missing (comma-separated)", value = "NA,-9999,-999"),
          actionButton("demo", "Or load demo data", class = "btn-secondary"),
          tags$hr()
        ),
        conditionalPanel(
          condition = "output.has_data",
          checkboxInput("divide10", "Values are in tenths (raw export) — divide by 10", value = FALSE),
          selectInput("var", "Variable", choices = setNames(names(VAR_DEFS), sapply(VAR_DEFS, `[[`, "label"))),
          uiOutput("year_range_ui"),
          checkboxGroupInput("months", "Months included", choices = setNames(1:12, month.abb),
                              selected = 1:12, inline = TRUE),
          selectInput("period", "Aggregate by", choices = c("Annual", "Seasonal", "Monthly", "Daily")),
          selectInput("stat", "Statistic", choices = names(STAT_FUNS)),
          conditionalPanel(
            condition = "input.period == 'Daily'",
            sliderInput("roll_days", "Smoothing window (days)", min = 7, max = 365, value = 30, step = 1)
          ),
          selectInput("smooth", "Trend line", choices = c("Linear" = "lm", "Loess" = "loess", "None" = "none")),
          tags$hr(),
          h5("Baseline period (for anomalies)"),
          uiOutput("baseline_ui"),
          tags$hr(),
          actionButton("reset_data", "Load different data", class = "btn-link")
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Dashboard",
                   uiOutput("dashboard_header"),
                   plotOutput("dashboard_plot", height = 560),
                   h5("All indicators, current year range and thresholds"),
                   if (has_DT) DT::dataTableOutput("dashboard_table") else tableOutput("dashboard_table")
          ),
          tabPanel("Trend", plotOutput("trend_plot", height = 480), verbatimTextOutput("trend_text"),
                   downloadButton("dl_trend_plot", "Download plot"), downloadButton("dl_agg_data", "Download aggregated data")),
          tabPanel("Climatology", plotOutput("clim_plot", height = 480),
                   checkboxInput("split_periods", "Compare early vs. late period", value = TRUE)),
          tabPanel("Year x month heatmap", plotOutput("heatmap_plot", height = 520)),
          tabPanel("Heat & cold waves",
                   fluidRow(
                     column(6, wellPanel(
                       h5("Heatwave (based on tx)"),
                       numericInput("heat_t1", "Warm-day threshold", value = 25, step = 0.5),
                       numericInput("heat_t2", "Hot-day threshold", value = 30, step = 0.5),
                       numericInput("heat_minrun", "Minimum consecutive warm days", value = 5, min = 1, step = 1),
                       numericInput("heat_minextreme", "...incl. at least this many hot days", value = 3, min = 0, step = 1)
                     )),
                     column(6, wellPanel(
                       h5("Coldwave (based on tn)"),
                       numericInput("cold_t1", "Frost-day threshold", value = 0, step = 0.5),
                       numericInput("cold_t2", "Severe-frost threshold", value = -10, step = 0.5),
                       numericInput("cold_minrun", "Minimum consecutive frost days", value = 5, min = 1, step = 1),
                       numericInput("cold_minextreme", "...incl. at least this many severe-frost days", value = 3, min = 0, step = 1)
                     ))
                   ),
                   uiOutput("waves_missing_cols_msg"),
                   plotOutput("wave_timeline_plot", height = 320),
                   plotOutput("wave_annual_plot", height = 320),
                   verbatimTextOutput("wave_trend_text"),
                   if (has_DT) DT::dataTableOutput("wave_table") else tableOutput("wave_table"),
                   downloadButton("dl_waves", "Download events as CSV")
          ),
          tabPanel("Dry & wet spells",
                   fluidRow(
                     column(6, wellPanel(
                       h5("Dry spell (based on rain)"),
                       numericInput("dry_t1", "Dry-day threshold (rain at or below)", value = 1, min = 0, step = 0.1),
                       numericInput("dry_minrun", "Minimum consecutive dry days", value = 10, min = 1, step = 1)
                     )),
                     column(6, wellPanel(
                       h5("Wet spell (based on rain)"),
                       numericInput("wet_t1", "Wet-day threshold (rain at or above)", value = 1, min = 0, step = 0.1),
                       numericInput("wet_minrun", "Minimum consecutive wet days", value = 5, min = 1, step = 1)
                     ))
                   ),
                   uiOutput("spells_missing_col_msg"),
                   plotOutput("spell_timeline_plot", height = 320),
                   plotOutput("spell_annual_plot", height = 320),
                   verbatimTextOutput("spell_trend_text"),
                   if (has_DT) DT::dataTableOutput("spell_table") else tableOutput("spell_table"),
                   downloadButton("dl_spells", "Download events as CSV")
          ),
          tabPanel("Frost & ice days",
                   fluidRow(
                     column(6, wellPanel(
                       h5("Frost days (based on tn)"),
                       numericInput("frost_t1", "Frost-day threshold (tn below)", value = 0, step = 0.5)
                     )),
                     column(6, wellPanel(
                       h5("Ice days (based on tx)"),
                       numericInput("ice_t1", "Ice-day threshold (tx below)", value = 0, step = 0.5)
                     ))
                   ),
                   uiOutput("frost_missing_cols_msg"),
                   plotOutput("frost_annual_plot", height = 380),
                   verbatimTextOutput("frost_trend_text"),
                   if (has_DT) DT::dataTableOutput("frost_table") else tableOutput("frost_table"),
                   downloadButton("dl_frost", "Download counts as CSV")
          ),
          tabPanel("Missing data", plotOutput("missing_plot", height = 600), tableOutput("missing_table")),
          tabPanel("Data",
                   if (has_DT) DT::dataTableOutput("data_table") else tableOutput("data_table"),
                   downloadButton("dl_raw_data", "Download filtered raw data"))
        )
      )
    )
  )

  server <- function(input, output, session) {

    raw_df <- reactiveVal(data)

    output$has_data <- reactive(!is.null(raw_df()))
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    observeEvent(input$file, {
      req(input$file)
      d <- tryCatch(
        read.csv(input$file$datapath, sep = input$sep, stringsAsFactors = FALSE, check.names = TRUE),
        error = function(e) { showNotification(paste("Could not read file:", e$message), type = "error"); NULL }
      )
      if (!is.null(d)) {
        d <- coerce_na(standardize_names(d), strsplit(input$na_strings, ",")[[1]])
        raw_df(d)
      }
    })

    observeEvent(input$demo, raw_df(make_demo_data()))
    observeEvent(input$reset_data, raw_df(NULL))

    # Only offer variables that actually exist in the loaded data (many
    # real climate extracts won't have all six columns).
    observeEvent(raw_df(), {
      req(raw_df())
      avail <- intersect(names(VAR_DEFS), names(standardize_names(raw_df())))
      if (length(avail) == 0) {
        showNotification(
          "None of the expected columns (tg, tx, tn, rh, rain, sq) were found in this data.",
          type = "error"
        )
        return(invisible())
      }
      choices <- setNames(avail, sapply(VAR_DEFS[avail], `[[`, "label"))
      updateSelectInput(session, "var", choices = choices, selected = avail[1])
    })

    prepared <- reactive({
      req(raw_df())
      tryCatch(prepare_data(raw_df(), divide_by_10 = isTRUE(input$divide10)),
               error = function(e) { showNotification(paste("Data error:", e$message), type = "error"); NULL })
    })

    output$year_range_ui <- renderUI({
      req(prepared())
      yrs <- range(prepared()$year, na.rm = TRUE)
      sliderInput("year_range", "Year range", min = yrs[1], max = yrs[2],
                  value = yrs, sep = "", step = 1)
    })

    output$baseline_ui <- renderUI({
      req(prepared())
      yrs <- range(prepared()$year, na.rm = TRUE)
      lo <- max(yrs[1], yrs[1])
      hi <- min(yrs[2], yrs[1] + 30)
      sliderInput("baseline_range", NULL, min = yrs[1], max = yrs[2],
                  value = c(lo, hi), sep = "", step = 1)
    })

    filtered <- reactive({
      req(prepared(), input$year_range)
      d <- prepared()
      d[d$year >= input$year_range[1] & d$year <= input$year_range[2], , drop = FALSE]
    })

    # Keep the "Statistic" choice sensible by default when the variable changes.
    observeEvent(input$var, {
      def <- VAR_DEFS[[input$var]]$default_stat %||% "mean"
      updateSelectInput(session, "stat", selected = def)
    })

    agg <- reactive({
      req(filtered(), input$var, input$period, input$stat, input$months)
      aggregate_series(filtered(), input$var, input$period, input$stat,
                        as.integer(input$months), roll_days = input$roll_days %||% 30)
    })

    var_label <- reactive({
      vd <- VAR_DEFS[[input$var]]
      paste0(vd$label, " (", vd$unit, ")")
    })

    build_trend_plot <- function() {
      d <- agg()
      req(nrow(d) > 0)
      is_seasonal <- input$period == "Seasonal"

      p <- if (is_seasonal) {
        ggplot(d, aes(x = x, y = value, color = season)) +
          geom_point(alpha = 0.7, size = 1.8) +
          scale_color_manual(values = SEASON_COLORS, drop = FALSE) +
          labs(color = "Season")
      } else {
        ggplot(d, aes(x = x, y = value)) +
          geom_point(alpha = if (input$period == "Daily") 0.15 else 0.6,
                     size = if (input$period == "Daily") 0.4 else 1.8,
                     color = "#2c7fb8")
      }
      p <- p +
        labs(x = "Year", y = var_label(),
             title = paste(var_label(), "-", input$period, "(", input$stat, ")"),
             subtitle = paste(input$year_range[1], "-", input$year_range[2])) +
        theme_minimal(base_size = 13)

      if (input$period == "Daily") {
        p <- p + geom_line(aes(y = value_smooth), color = "#08519c", linewidth = 0.6, na.rm = TRUE)
      }
      if (input$smooth != "none") {
        method <- if (input$smooth == "lm") "lm" else "loess"
        if (is_seasonal) {
          p <- p + geom_smooth(method = method, formula = y ~ x, se = TRUE, na.rm = TRUE)
        } else {
          p <- p + geom_smooth(method = method, formula = y ~ x, se = TRUE, color = "#e34a33", na.rm = TRUE)
        }
      }
      p
    }

    output$trend_plot <- renderPlot(build_trend_plot())

    output$trend_text <- renderPrint({
      d <- agg()
      if (input$period == "Seasonal") {
        for (s in levels(d$season)) {
          ds <- d[d$season == s, , drop = FALSE]
          ts <- trend_stats(ds$x, ds$value)
          if (!isTRUE(ts$ok)) {
            cat(sprintf("%-7s not enough non-missing data points (n = %d).\n", s, ts$n))
          } else {
            cat(sprintf(
              "%-7s %.4f %s/decade (p = %s), Kendall's tau %.3f (p = %s), n = %d\n",
              s, ts$slope_per_decade, VAR_DEFS[[input$var]]$unit, fmt_p(ts$p_lm), ts$tau, fmt_p(ts$p_kendall), ts$n
            ))
          }
        }
      } else {
        y_for_stats <- if (input$period == "Daily") d$value_smooth else d$value
        ts <- trend_stats(d$x, y_for_stats)
        if (!isTRUE(ts$ok)) {
          cat("Not enough non-missing data points in this selection to fit a trend (n =", ts$n, ").\n")
        } else {
          cat(sprintf(
            "Linear trend: %.4f %s per decade (p = %s)\nKendall's tau: %.3f (p = %s)\nData points used: %d\n",
            ts$slope_per_decade, VAR_DEFS[[input$var]]$unit, fmt_p(ts$p_lm), ts$tau, fmt_p(ts$p_kendall), ts$n
          ))
        }
      }
    })

    output$dl_trend_plot <- downloadHandler(
      filename = function() paste0("trend_", input$var, "_", input$period, ".png"),
      content = function(file) ggsave(file, plot = build_trend_plot(), width = 10, height = 6, dpi = 150)
    )
    output$dl_agg_data <- downloadHandler(
      filename = function() paste0("aggregated_", input$var, "_", input$period, ".csv"),
      content = function(file) write.csv(agg(), file, row.names = FALSE)
    )

    output$clim_plot <- renderPlot({
      d <- filtered()
      req(nrow(d) > 0)
      d <- d[!is.na(d[[input$var]]), , drop = FALSE]
      d$month_f <- factor(d$month, levels = 1:12, labels = month.abb)

      if (isTRUE(input$split_periods)) {
        split_year <- floor(median(c(input$year_range[1], input$year_range[2])))
        d$period_grp <- ifelse(d$year <= split_year,
                                paste(input$year_range[1], "-", split_year),
                                paste(split_year + 1, "-", input$year_range[2]))
        ggplot(d, aes(x = month_f, y = .data[[input$var]], fill = period_grp, color = season)) +
          geom_boxplot(outlier.alpha = 0.15, linewidth = 0.9, position = position_dodge(width = 0.75)) +
          labs(x = NULL, y = var_label(), fill = "Period", color = "Season",
               title = paste("Seasonal pattern of", var_label(), "- early vs. late period")) +
          scale_fill_manual(values = c("#c9c9c9", "#f0b34a")) +
          scale_color_manual(values = SEASON_COLORS, drop = FALSE) +
          theme_minimal(base_size = 13)
      } else {
        ggplot(d, aes(x = month_f, y = .data[[input$var]], fill = season)) +
          geom_boxplot(outlier.alpha = 0.15) +
          labs(x = NULL, y = var_label(), fill = "Season", title = paste("Seasonal pattern of", var_label())) +
          scale_fill_manual(values = SEASON_COLORS, drop = FALSE) +
          theme_minimal(base_size = 13)
      }
    })

    output$heatmap_plot <- renderPlot({
      req(filtered(), input$baseline_range)
      d <- prepared()
      base_d <- d[d$year >= input$baseline_range[1] & d$year <= input$baseline_range[2], , drop = FALSE]
      monthly_norm <- base_d %>%
        group_by(month) %>%
        summarise(norm = mean(.data[[input$var]], na.rm = TRUE), .groups = "drop")

      dsel <- filtered() %>%
        group_by(year, month) %>%
        summarise(value = mean(.data[[input$var]], na.rm = TRUE), .groups = "drop") %>%
        left_join(monthly_norm, by = "month") %>%
        mutate(anomaly = value - norm, month_f = factor(month, levels = 1:12, labels = month.abb))

      month_axis_colors <- unname(SEASON_COLORS[MONTH_SEASON])
      # An invisible dummy layer, just to get a "Season" color legend that
      # matches the (unofficially) per-label-colored x axis text below.
      season_key <- data.frame(month_f = dsel$month_f[1], year = dsel$year[1],
                                season = factor(names(SEASON_COLORS), levels = names(SEASON_COLORS)))

      ggplot(dsel, aes(x = month_f, y = year)) +
        geom_tile(aes(fill = anomaly)) +
        geom_point(data = season_key, aes(color = season), alpha = 0) +
        scale_fill_gradient2(low = "#3182bd", mid = "white", high = "#e34a33", midpoint = 0, na.value = "grey85") +
        scale_color_manual(values = SEASON_COLORS, guide = guide_legend(override.aes = list(alpha = 1, size = 4))) +
        scale_y_reverse() +
        labs(x = NULL, y = "Year", fill = "Anomaly", color = "Season (month labels)",
             title = paste(var_label(), "- anomaly vs.", input$baseline_range[1], "-", input$baseline_range[2], "baseline")) +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(colour = month_axis_colors, face = "bold"))
    })

    waves_available <- reactive({
      req(prepared())
      intersect(c("tx", "tn"), names(prepared()))
    })

    output$waves_missing_cols_msg <- renderUI({
      missing_cols <- setdiff(c("tx", "tn"), waves_available())
      if (length(missing_cols) == 0) return(NULL)
      what <- if (length(missing_cols) == 2) "tx and tn are" else paste(missing_cols, "is")
      cant_detect <- if ("tx" %in% missing_cols && "tn" %in% missing_cols) "neither heatwaves nor coldwaves can"
                     else if ("tx" %in% missing_cols) "heatwaves can't"
                     else "coldwaves can't"
      div(class = "alert alert-warning",
          sprintf("%s not present in this data, so %s be detected.", what, cant_detect))
    })

    heat_cold_events <- reactive({
      req(prepared(), input$year_range)
      d <- prepared()
      d <- d[d$year >= input$year_range[1] & d$year <= input$year_range[2], , drop = FALSE]
      d <- d[order(d$date), , drop = FALSE]
      avail <- waves_available()

      heat <- if ("tx" %in% avail) {
        req(input$heat_t1, input$heat_t2, input$heat_minrun, input$heat_minextreme)
        ev <- detect_extreme_spells(d$date, d$tx, input$heat_t1, input$heat_t2, "ge",
                                     input$heat_minrun, input$heat_minextreme)
        if (nrow(ev) > 0) ev$type <- "Heatwave"
        ev
      } else NULL

      cold <- if ("tn" %in% avail) {
        req(input$cold_t1, input$cold_t2, input$cold_minrun, input$cold_minextreme)
        ev <- detect_extreme_spells(d$date, d$tn, input$cold_t1, input$cold_t2, "le",
                                     input$cold_minrun, input$cold_minextreme)
        if (nrow(ev) > 0) ev$type <- "Coldwave"
        ev
      } else NULL

      out <- bind_rows(heat, cold)
      if (nrow(out) > 0) out$type <- factor(out$type, levels = c("Heatwave", "Coldwave"))
      out
    })

    WAVE_COLORS <- c(Heatwave = "#e34a33", Coldwave = "#3182bd")

    output$wave_timeline_plot <- renderPlot({
      req(waves_available())
      ev <- heat_cold_events()
      if (nrow(ev) == 0) {
        return(ggplot() +
                 labs(title = "No heatwaves or coldwaves detected with the current thresholds and year range") +
                 theme_minimal(base_size = 13))
      }
      ggplot(ev, aes(x = start, xend = end, y = duration, yend = duration, color = type)) +
        geom_segment(linewidth = 3, lineend = "round") +
        scale_color_manual(values = WAVE_COLORS, drop = FALSE) +
        labs(x = "Date", y = "Duration (days)", color = NULL, title = "Detected heatwaves and coldwaves") +
        theme_minimal(base_size = 13)
    })

    output$wave_annual_plot <- renderPlot({
      req(waves_available())
      ev <- heat_cold_events()
      if (nrow(ev) == 0) {
        return(ggplot() + labs(title = "No events to summarise by year") + theme_minimal(base_size = 13))
      }
      m <- ev %>%
        mutate(year = year(start)) %>%
        group_by(year, type) %>%
        summarise(n_events = n(), n_days = sum(duration), .groups = "drop")
      ggplot(m, aes(x = year, y = n_days, fill = type)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.8) +
        scale_fill_manual(values = WAVE_COLORS, drop = FALSE) +
        labs(x = "Year", y = "Total days in a wave", fill = NULL,
             title = "Heatwave / coldwave days per year") +
        theme_minimal(base_size = 13)
    })

    output$wave_trend_text <- renderPrint({
      ev <- heat_cold_events()
      if (nrow(ev) == 0) {
        cat("No events detected — nothing to compute a trend on.\n")
        return(invisible())
      }
      yrs <- input$year_range[1]:input$year_range[2]
      for (ty in levels(ev$type)) {
        sub <- ev[ev$type == ty, , drop = FALSE]
        by_year <- sapply(yrs, function(y) sum(sub$duration[year(sub$start) == y]))
        ts <- trend_stats(yrs, by_year)
        if (!isTRUE(ts$ok)) {
          cat(sprintf("%-9s not enough years in range to fit a trend.\n", ty))
        } else {
          cat(sprintf(
            "%-9s %.3f days/decade (p = %s), Kendall's tau %.3f (p = %s), %d events total\n",
            ty, ts$slope_per_decade, fmt_p(ts$p_lm), ts$tau, fmt_p(ts$p_kendall), sum(ev$type == ty)
          ))
        }
      }
    })

    output$wave_table <- if (has_DT) {
      DT::renderDataTable({
        ev <- heat_cold_events()
        req(nrow(ev) > 0)
        DT::datatable(ev %>% arrange(start) %>% select(type, start, end, duration, n_extreme, peak, mean_value),
                      options = list(pageLength = 15)) %>%
          DT::formatStyle("type", fontWeight = "bold",
                           color = DT::styleEqual(names(WAVE_COLORS), unname(WAVE_COLORS))) %>%
          DT::formatRound(c("peak", "mean_value"), digits = 1)
      })
    } else {
      renderTable({
        ev <- heat_cold_events()
        req(nrow(ev) > 0)
        ev %>% arrange(start) %>% select(type, start, end, duration, n_extreme, peak, mean_value)
      })
    }

    output$dl_waves <- downloadHandler(
      filename = "heat_cold_waves.csv",
      content = function(file) write.csv(heat_cold_events() %>% arrange(start), file, row.names = FALSE)
    )

    spells_available <- reactive({
      req(prepared())
      "rain" %in% names(prepared())
    })

    output$spells_missing_col_msg <- renderUI({
      if (isTRUE(spells_available())) return(NULL)
      div(class = "alert alert-warning",
          "rain is not present in this data, so dry and wet spells can't be detected.")
    })

    dry_wet_events <- reactive({
      req(prepared(), input$year_range)
      req(spells_available())
      d <- prepared()
      d <- d[d$year >= input$year_range[1] & d$year <= input$year_range[2], , drop = FALSE]
      d <- d[order(d$date), , drop = FALSE]

      req(input$dry_t1, input$dry_minrun, input$wet_t1, input$wet_minrun)
      dry <- detect_extreme_spells(d$date, d$rain, input$dry_t1, input$dry_t1, "le",
                                    input$dry_minrun, min_extreme = 0)
      if (nrow(dry) > 0) dry$type <- "Dry spell"

      wet <- detect_extreme_spells(d$date, d$rain, input$wet_t1, input$wet_t1, "ge",
                                    input$wet_minrun, min_extreme = 0)
      if (nrow(wet) > 0) wet$type <- "Wet spell"

      out <- bind_rows(dry, wet)
      if (nrow(out) > 0) out$type <- factor(out$type, levels = c("Dry spell", "Wet spell"))
      out
    })

    SPELL_COLORS <- c(`Dry spell` = "#c9a227", `Wet spell` = "#1b7837")

    output$spell_timeline_plot <- renderPlot({
      req(spells_available())
      ev <- dry_wet_events()
      if (nrow(ev) == 0) {
        return(ggplot() +
                 labs(title = "No dry or wet spells detected with the current thresholds and year range") +
                 theme_minimal(base_size = 13))
      }
      ggplot(ev, aes(x = start, xend = end, y = duration, yend = duration, color = type)) +
        geom_segment(linewidth = 3, lineend = "round") +
        scale_color_manual(values = SPELL_COLORS, drop = FALSE) +
        labs(x = "Date", y = "Duration (days)", color = NULL, title = "Detected dry and wet spells") +
        theme_minimal(base_size = 13)
    })

    output$spell_annual_plot <- renderPlot({
      req(spells_available())
      ev <- dry_wet_events()
      if (nrow(ev) == 0) {
        return(ggplot() + labs(title = "No events to summarise by year") + theme_minimal(base_size = 13))
      }
      m <- ev %>%
        mutate(year = year(start)) %>%
        group_by(year, type) %>%
        summarise(n_days = sum(duration), .groups = "drop")
      ggplot(m, aes(x = year, y = n_days, fill = type)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.8) +
        scale_fill_manual(values = SPELL_COLORS, drop = FALSE) +
        labs(x = "Year", y = "Total days in a spell", fill = NULL,
             title = "Dry / wet spell days per year") +
        theme_minimal(base_size = 13)
    })

    output$spell_trend_text <- renderPrint({
      ev <- dry_wet_events()
      if (nrow(ev) == 0) {
        cat("No events detected — nothing to compute a trend on.\n")
        return(invisible())
      }
      yrs <- input$year_range[1]:input$year_range[2]
      for (ty in levels(ev$type)) {
        sub <- ev[ev$type == ty, , drop = FALSE]
        by_year <- sapply(yrs, function(y) sum(sub$duration[year(sub$start) == y]))
        ts <- trend_stats(yrs, by_year)
        if (!isTRUE(ts$ok)) {
          cat(sprintf("%-10s not enough years in range to fit a trend.\n", ty))
        } else {
          cat(sprintf(
            "%-10s %.3f days/decade (p = %s), Kendall's tau %.3f (p = %s), %d events total\n",
            ty, ts$slope_per_decade, fmt_p(ts$p_lm), ts$tau, fmt_p(ts$p_kendall), sum(ev$type == ty)
          ))
        }
      }
    })

    output$spell_table <- if (has_DT) {
      DT::renderDataTable({
        ev <- dry_wet_events()
        req(nrow(ev) > 0)
        DT::datatable(ev %>% arrange(start) %>% select(type, start, end, duration, total, peak, mean_value),
                      options = list(pageLength = 15)) %>%
          DT::formatStyle("type", fontWeight = "bold",
                           color = DT::styleEqual(names(SPELL_COLORS), unname(SPELL_COLORS))) %>%
          DT::formatRound(c("total", "peak", "mean_value"), digits = 1)
      })
    } else {
      renderTable({
        ev <- dry_wet_events()
        req(nrow(ev) > 0)
        ev %>% arrange(start) %>% select(type, start, end, duration, total, peak, mean_value)
      })
    }

    output$dl_spells <- downloadHandler(
      filename = "dry_wet_spells.csv",
      content = function(file) write.csv(dry_wet_events() %>% arrange(start), file, row.names = FALSE)
    )

    # Frost/ice days are single-day counts (tn or tx past a threshold on that
    # day alone), not multi-day runs — no run-length parameter needed, unlike
    # the heatwave/coldwave and dry/wet-spell tabs above.
    frost_ice_available <- reactive({
      req(prepared())
      intersect(c("tn", "tx"), names(prepared()))
    })

    output$frost_missing_cols_msg <- renderUI({
      missing_cols <- setdiff(c("tn", "tx"), frost_ice_available())
      if (length(missing_cols) == 0) return(NULL)
      what <- if (length(missing_cols) == 2) "tn and tx are" else paste(missing_cols, "is")
      cant_detect <- if ("tn" %in% missing_cols && "tx" %in% missing_cols) "neither frost days nor ice days can"
                     else if ("tn" %in% missing_cols) "frost days can't"
                     else "ice days can't"
      div(class = "alert alert-warning",
          sprintf("%s not present in this data, so %s be counted.", what, cant_detect))
    })

    frost_ice_counts <- reactive({
      req(prepared(), input$year_range)
      d <- prepared()
      d <- d[d$year >= input$year_range[1] & d$year <= input$year_range[2], , drop = FALSE]
      avail <- frost_ice_available()

      frost <- if ("tn" %in% avail) {
        req(input$frost_t1)
        d %>% group_by(year) %>% summarise(count = sum(tn < input$frost_t1, na.rm = TRUE), .groups = "drop") %>%
          mutate(type = "Frost days")
      } else NULL

      ice <- if ("tx" %in% avail) {
        req(input$ice_t1)
        d %>% group_by(year) %>% summarise(count = sum(tx < input$ice_t1, na.rm = TRUE), .groups = "drop") %>%
          mutate(type = "Ice days")
      } else NULL

      out <- bind_rows(frost, ice)
      if (nrow(out) > 0) out$type <- factor(out$type, levels = c("Frost days", "Ice days"))
      out
    })

    FROST_COLORS <- c(`Frost days` = "#9ecae1", `Ice days` = "#08519c")

    output$frost_annual_plot <- renderPlot({
      req(frost_ice_available())
      m <- frost_ice_counts()
      if (nrow(m) == 0) {
        return(ggplot() +
                 labs(title = "No frost or ice days in this year range with the current thresholds") +
                 theme_minimal(base_size = 13))
      }
      ggplot(m, aes(x = year, y = count, fill = type)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.8) +
        scale_fill_manual(values = FROST_COLORS, drop = FALSE) +
        labs(x = "Year", y = "Days per year", fill = NULL,
             title = "Frost days (tn below threshold) and ice days (tx below threshold) per year") +
        theme_minimal(base_size = 13)
    })

    output$frost_trend_text <- renderPrint({
      m <- frost_ice_counts()
      if (nrow(m) == 0) {
        cat("No data — nothing to compute a trend on.\n")
        return(invisible())
      }
      for (ty in levels(m$type)) {
        sub <- m[m$type == ty, , drop = FALSE]
        ts <- trend_stats(sub$year, sub$count)
        if (!isTRUE(ts$ok)) {
          cat(sprintf("%-11s not enough years in range to fit a trend.\n", ty))
        } else {
          cat(sprintf(
            "%-11s %.3f days/decade (p = %s), Kendall's tau %.3f (p = %s), %d days total across the period\n",
            ty, ts$slope_per_decade, fmt_p(ts$p_lm), ts$tau, fmt_p(ts$p_kendall), sum(sub$count)
          ))
        }
      }
    })

    output$frost_table <- if (has_DT) {
      DT::renderDataTable({
        m <- frost_ice_counts()
        req(nrow(m) > 0)
        wide <- m %>% select(year, type, count) %>%
          tidyr::pivot_wider(names_from = type, values_from = count, values_fill = 0) %>%
          arrange(year)
        DT::datatable(wide, options = list(pageLength = 15))
      })
    } else {
      renderTable({
        m <- frost_ice_counts()
        req(nrow(m) > 0)
        m %>% select(year, type, count) %>%
          tidyr::pivot_wider(names_from = type, values_from = count, values_fill = 0) %>%
          arrange(year)
      })
    }

    output$dl_frost <- downloadHandler(
      filename = "frost_ice_days.csv",
      content = function(file) write.csv(frost_ice_counts() %>% arrange(year), file, row.names = FALSE)
    )

    # ---- Dashboard: one annual series per available indicator, reusing the
    # same reactives (and current threshold settings) as the dedicated tabs.
    DASHBOARD_BASE_VARS <- list(
      tg   = list(label = "Mean temperature", unit = "°C", stat = "mean"),
      tx   = list(label = "Max temperature",  unit = "°C", stat = "mean"),
      tn   = list(label = "Min temperature",  unit = "°C", stat = "mean"),
      rh   = list(label = "Relative humidity", unit = "%", stat = "mean"),
      rain = list(label = "Annual rainfall",   unit = "mm", stat = "sum"),
      sq   = list(label = "Annual sunshine",   unit = "hours", stat = "sum")
    )

    dashboard_series <- reactive({
      req(prepared(), input$year_range)
      d <- prepared()
      d <- d[d$year >= input$year_range[1] & d$year <= input$year_range[2], , drop = FALSE]
      yrs <- input$year_range[1]:input$year_range[2]
      parts <- list()

      for (v in names(DASHBOARD_BASE_VARS)) {
        if (v %in% names(d)) {
          bv <- DASHBOARD_BASE_VARS[[v]]
          statf <- STAT_FUNS[[bv$stat]]
          s <- d %>% group_by(year) %>% summarise(value = statf(.data[[v]]), .groups = "drop")
          parts[[bv$label]] <- data.frame(indicator = bv$label, unit = bv$unit, year = s$year, value = s$value)
        }
      }

      wa <- waves_available()
      if (length(wa) > 0) {
        ev_hc <- heat_cold_events()
        if ("tx" %in% wa) {
          a <- annual_event_days(ev_hc, "Heatwave", yrs)
          parts[["Heatwave days"]] <- data.frame(indicator = "Heatwave days", unit = "days", year = a$year, value = a$value)
        }
        if ("tn" %in% wa) {
          a <- annual_event_days(ev_hc, "Coldwave", yrs)
          parts[["Coldwave days"]] <- data.frame(indicator = "Coldwave days", unit = "days", year = a$year, value = a$value)
        }
      }

      if (isTRUE(spells_available())) {
        ev_dw <- dry_wet_events()
        a <- annual_event_days(ev_dw, "Dry spell", yrs)
        parts[["Dry spell days"]] <- data.frame(indicator = "Dry spell days", unit = "days", year = a$year, value = a$value)
        a <- annual_event_days(ev_dw, "Wet spell", yrs)
        parts[["Wet spell days"]] <- data.frame(indicator = "Wet spell days", unit = "days", year = a$year, value = a$value)
      }

      fia <- frost_ice_available()
      if (length(fia) > 0) {
        fic <- frost_ice_counts()
        if ("tn" %in% fia) {
          sub <- fic[fic$type == "Frost days", , drop = FALSE]
          parts[["Frost days"]] <- data.frame(indicator = "Frost days", unit = "days", year = sub$year, value = sub$count)
        }
        if ("tx" %in% fia) {
          sub <- fic[fic$type == "Ice days", , drop = FALSE]
          parts[["Ice days"]] <- data.frame(indicator = "Ice days", unit = "days", year = sub$year, value = sub$count)
        }
      }

      bind_rows(parts)
    })

    dashboard_trends <- reactive({
      s <- dashboard_series()
      req(nrow(s) > 0)
      s %>%
        group_by(indicator, unit) %>%
        group_modify(~ {
          ts <- trend_stats(.x$year, .x$value)
          if (!isTRUE(ts$ok)) {
            data.frame(slope_per_decade = NA_real_, p_lm = NA_real_, tau = NA_real_, p_kendall = NA_real_, n = ts$n)
          } else {
            data.frame(slope_per_decade = ts$slope_per_decade, p_lm = ts$p_lm, tau = ts$tau, p_kendall = ts$p_kendall, n = ts$n)
          }
        }) %>%
        ungroup()
    })

    stat_card <- function(title, value_text, sub_text = NULL, color = "#333333") {
      div(style = "flex:1 1 160px; background:#f5f5f5; border-radius:8px; padding:12px 16px; margin:5px;",
          div(style = "font-size:11px; color:#888; text-transform:uppercase; letter-spacing:0.5px;", title),
          div(style = paste0("font-size:20px; font-weight:700; color:", color, "; white-space:nowrap;"), value_text),
          if (!is.null(sub_text)) div(style = "font-size:11px; color:#999;", sub_text)
      )
    }

    output$dashboard_header <- renderUI({
      req(prepared())
      d <- prepared()
      yrs <- range(d$year, na.rm = TRUE)
      cards <- list(stat_card("Data period", paste(yrs[1], "-", yrs[2]), paste(yrs[2] - yrs[1] + 1, "years")))

      tr <- tryCatch(dashboard_trends(), error = function(e) NULL)
      if (!is.null(tr) && nrow(tr) > 0) {
        headline <- c("Mean temperature", "Annual rainfall", "Frost days", "Heatwave days")
        for (h in headline) {
          row <- tr[tr$indicator == h, ]
          if (nrow(row) == 1 && !is.na(row$slope_per_decade)) {
            arrow <- if (row$slope_per_decade >= 0) "▲" else "▼"
            color <- if (row$slope_per_decade >= 0) "#e34a33" else "#3182bd"
            cards[[length(cards) + 1]] <- stat_card(
              h, sprintf("%s %.2f %s/decade", arrow, abs(row$slope_per_decade), row$unit),
              paste("p =", fmt_p(row$p_lm)), color = color
            )
          }
        }
      }
      div(style = "display:flex; flex-wrap:wrap; margin-bottom: 10px;", cards)
    })

    output$dashboard_plot <- renderPlot({
      s <- dashboard_series()
      req(nrow(s) > 0)
      ggplot(s, aes(x = year, y = value)) +
        geom_point(alpha = 0.5, size = 1, color = "#2c7fb8") +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#e34a33", linewidth = 0.6, na.rm = TRUE) +
        facet_wrap(~indicator, scales = "free_y", ncol = 3) +
        labs(x = "Year", y = NULL, title = "Every available indicator, at a glance") +
        theme_minimal(base_size = 12) +
        theme(strip.text = element_text(face = "bold"))
    })

    output$dashboard_table <- if (has_DT) {
      DT::renderDataTable({
        tr <- dashboard_trends()
        req(nrow(tr) > 0)
        tbl <- tr %>%
          transmute(
            Indicator = indicator,
            Trend = ifelse(is.na(slope_per_decade), "NA",
                            sprintf("%s%.3f %s/decade", ifelse(slope_per_decade >= 0, "+", "-"),
                                    abs(slope_per_decade), unit)),
            `p (linear)` = vapply(p_lm, fmt_p, character(1)),
            `Kendall tau` = ifelse(is.na(tau), "NA", sprintf("%.3f", tau)),
            `p (Kendall)` = vapply(p_kendall, fmt_p, character(1)),
            Years = n
          ) %>%
          arrange(Indicator)
        DT::datatable(tbl, options = list(pageLength = 15, dom = "t"))
      })
    } else {
      renderTable({
        tr <- dashboard_trends()
        req(nrow(tr) > 0)
        tr %>%
          transmute(
            Indicator = indicator,
            Trend = ifelse(is.na(slope_per_decade), "NA",
                            sprintf("%s%.3f %s/decade", ifelse(slope_per_decade >= 0, "+", "-"),
                                    abs(slope_per_decade), unit)),
            `p (linear)` = vapply(p_lm, fmt_p, character(1)),
            `Kendall tau` = ifelse(is.na(tau), "NA", sprintf("%.3f", tau)),
            `p (Kendall)` = vapply(p_kendall, fmt_p, character(1)),
            Years = n
          ) %>%
          arrange(Indicator)
      })
    }

    output$missing_plot <- renderPlot({
      d <- filtered()
      req(nrow(d) > 0)
      m <- d %>%
        group_by(year, season) %>%
        summarise(pct_missing = 100 * mean(is.na(.data[[input$var]])), .groups = "drop")
      ggplot(m, aes(x = year, y = pct_missing, fill = season)) +
        geom_col() +
        scale_fill_manual(values = SEASON_COLORS, drop = FALSE, guide = "none") +
        facet_wrap(~season, ncol = 1, strip.position = "right") +
        labs(x = "Year", y = "% missing", title = paste("Missing data by year and season -", var_label())) +
        theme_minimal(base_size = 13) +
        theme(strip.text = element_text(face = "bold"))
    })

    output$missing_table <- renderTable({
      d <- filtered()
      req(nrow(d) > 0)
      avail <- intersect(names(VAR_DEFS), names(d))
      data.frame(
        Variable = avail,
        `% missing (selection)` = sapply(avail, function(v) round(100 * mean(is.na(d[[v]])), 2)),
        check.names = FALSE
      )
    })

    output$data_table <- if (has_DT) {
      DT::renderDataTable({
        df <- filtered() %>% select(date, year, month, season, any_of(names(VAR_DEFS)))
        DT::datatable(df, options = list(pageLength = 15)) %>%
          DT::formatStyle(
            columns = names(df), valueColumns = "season",
            backgroundColor = DT::styleEqual(names(SEASON_TINTS), unname(SEASON_TINTS))
          ) %>%
          DT::formatStyle(
            "season", fontWeight = "bold",
            color = DT::styleEqual(names(SEASON_COLORS), unname(SEASON_COLORS))
          )
      })
    } else {
      renderTable({
        df <- head(filtered() %>% select(date, year, month, season, any_of(names(VAR_DEFS))), 200)
        df$season <- sprintf('<span style="color:%s;font-weight:bold">%s</span>',
                              SEASON_COLORS[as.character(df$season)], df$season)
        df
      }, sanitize.text.function = function(x) x)
    }

    output$dl_raw_data <- downloadHandler(
      filename = "climate_data_filtered.csv",
      content = function(file) write.csv(filtered(), file, row.names = FALSE)
    )
  }

  shinyApp(ui, server)
}
