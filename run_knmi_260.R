## Launch the app directly with De Bilt (station 260) daily data from KNMI.
## Replaces manual download + readr/janitor prep with fetch_knmi_daily(),
## which also fixes the RH/rain naming mix-up and the trace-precipitation
## sentinel (see README.md > "Using real KNMI data").

source("R/climate_trend_app.R")

weather_tbl <- fetch_knmi_daily(260)

climate_trend_app(weather_tbl)
