**Indonesia CPI Forecast with VAR and Custom LM-VAR Models**

**Overview**
This R script forecasts Indonesia’s monthly CPI using two approaches: a baseline VAR model and a custom linear-model VAR (LM-VAR). It allows one-step-ahead and multi-month forecasts, backtesting, and comparison against historical CPI.

**Features**
Forecast CPI MoM and convert to YoY
Baseline VAR with AIC-selected optimal lags
Custom LM-VAR using lagged predictors
Supports exogenous oil path inputs for scenario analysis
Backtesting with RMSE and MAE evaluation
Generates combined plots: Actual vs Baseline vs Custom forecasts
Produces YoY CPI tables for further analysis

**Requirements**
R packages: readxl, dplyr, tidyr, vars, purrr, lubridate, tibble, ggplot2, Metrics
Local Excel file Data Inputs.xlsx with columns: Date, cpi, oil_px, reer, sales, pmi

**Usage**
Load the required libraries.
Update file path to your Data Inputs.xlsx.
Run the script to generate forecasts, plots, and backtest metrics.
