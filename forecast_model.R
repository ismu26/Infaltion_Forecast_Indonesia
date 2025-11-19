library(readxl)
library(dplyr)
library(tidyr)
library(vars)
library(purrr)
library(lubridate)
library(tibble)
library(ggplot2)

df <- read_excel("Data Inputs.xlsx", sheet = "XXX")
df <- df %>%
  mutate(cpi_mom = (cpi/lag(cpi)) - 1
         , oil_mom = (oil_px/lag(oil_px)) - 1
         , reer_mom = (reer/lag(reer)) - 1
         , sales_mom = (sales/lag(sales)) - 1
         , pmi_mom = (pmi/lag(pmi)) - 1) %>%
  dplyr::select(Date, cpi_mom, oil_mom, reer_mom, sales_mom, pmi_mom) %>%
  drop_na()

##VAR Package Model

optimal_lags_aic <- ts(df %>%
                         dplyr::select(-Date), frequency = 12, start = c(2014, 9)) %>%
  VARselect(lag.max = 3, type = "const") %>%
  purrr::pluck("selection") %>%
  purrr::pluck("AIC(n)")

baseline_model <- VAR(ts(df %>%
                           dplyr::select(-Date), frequency = 12, start = c(2014, 9))
                      , p = optimal_lags_aic
                      , type = "const")
forecast_start <- as.Date("2025-07-01")
forecast_end   <- forecast_start %m+% months(11)  
baseline_fc <- predict(baseline_model, n.ahead = (time_length(interval(forecast_start, forecast_end), "months") + 1))

cpi_fc <- baseline_fc$fcst$cpi_mom[, "fcst"]
forecast_dates <- seq(as.Date("2025-07-01"), by = "month", length.out = length(cpi_fc))


forecast_df <- tibble(
  date = forecast_dates,
  cpi_forecast = cpi_fc
)

ggplot(forecast_df, aes(x = date, y = cpi_forecast)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "VAR Package Forecasted CPI YoY", x = "Date", y = "CPI Forecast (%)") +
  theme_minimal()


forecast_df <- tibble(
  date = forecast_dates,
  cpi = cpi_fc,
  type = "Forecast"
)

actual_df <- df %>%
  dplyr::select(Date, cpi_mom) %>%
  dplyr::rename(date = Date, cpi = cpi_mom) %>%
  dplyr::mutate(type = "Actual")

combined_df <- dplyr::bind_rows(actual_df, forecast_df)

ggplot(combined_df, aes(x = date, y = cpi, color = type)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "VAR Package Forecasted CPI YoY",
       x = "Date", y = "CPI MoM (%)",
       color = "Data Type") +
  theme_minimal()

tail(actual_df)

##LM VAR Mode

variables_to_lag <- c("cpi_mom", "oil_mom", "reer_mom", "sales_mom", "pmi_mom")

lagged_vars <- map_dfc(1:optimal_lags_aic, function(lag_i) {
  df %>%
    dplyr::select(all_of(variables_to_lag)) %>%
    rename_with(~ paste0(.x, "_lag_", lag_i)) %>%
    mutate(across(everything(), ~ lag(.x, n = lag_i)))
})

df_data_lagged <- bind_cols(df, lagged_vars) %>%
  dplyr::select(Date, everything()) %>%
  na.omit()

cpi.fit <- lm(cpi_mom ~ ., data = df_data_lagged %>%
                dplyr::select(-Date, -oil_mom, -reer_mom, -sales_mom, -pmi_mom))

oil.fit <- lm(oil_mom ~ ., data = df_data_lagged %>%
                dplyr::select(-Date, -cpi_mom, -reer_mom, -sales_mom, -pmi_mom))

reer.fit <- lm(reer_mom ~ ., data = df_data_lagged %>%
                 dplyr::select(-Date, -cpi_mom, -oil_mom, -sales_mom, -pmi_mom))

sales.fit <- lm(sales_mom ~ ., data = df_data_lagged %>%
                  dplyr::select(-Date, -cpi_mom, -oil_mom, -reer_mom, -pmi_mom))

pmi.fit <- lm(pmi_mom ~ ., data = df_data_lagged %>%
                dplyr::select(-Date, -cpi_mom, -oil_mom, -reer_mom, -sales_mom))

forecast_start <- as.Date("2025-07-01")
forecast_end <- forecast_start %m+% months(11) 

h <- round(time_length(interval(forecast_start, forecast_end), "months") + 1)

fc_dates <- seq(forecast_start, by = "month", length.out = h)

fc.df <- data.frame(
  Date = fc_dates,
  matrix(NA_real_, nrow = h, ncol = ncol(df_data_lagged) - 1)
)
names(fc.df)[-1] <- names(df_data_lagged)[-1]
names(fc.df)[-1] <- colnames(df_data_lagged[2:ncol(df_data_lagged)])

fc.df <- rbind(tail(df_data_lagged, optimal_lags_aic), fc.df)

raw_df <- read_excel("Data Inputs.xlsx", sheet = "IDN")

oil_level_start <- raw_df %>%
  filter(Date < forecast_start) %>%
  tail(1) %>%
  pull(oil_px)

if (!exists("oil_constant", envir = .GlobalEnv)) {
  oil_constant <- oil_level_start
  message("oil_constant not supplied — using last known price.")
}

oil_path <- data.frame(
  Date = seq(forecast_start %m-% months(1), forecast_end, by = "month"),
  oil_px = c(oil_level_start, rep(oil_constant, h))
) %>%
  dplyr::mutate(oil_mom = oil_px / dplyr::lag(oil_px) - 1) %>%
  tidyr::drop_na() %>%
  dplyr::select(-oil_px)

fc.df <- fc.df %>%
  dplyr::left_join(oil_path, by = "Date", suffix = c("", ".new")) %>%
  dplyr::mutate(
    oil_mom = dplyr::coalesce(oil_mom, oil_mom.new)
  ) %>%
  dplyr::select(-oil_mom.new)

variables_to_forecast <- c("cpi_mom", "oil_mom", "reer_mom", "sales_mom", "pmi_mom")


for (i in (optimal_lags_aic + 1):nrow(fc.df)) {
  
  for (lag_i in 1:optimal_lags_aic) {
    lag_index <- i - lag_i
    if (lag_index > 0) {
      
      for (var in variables_to_forecast) {
        lagged_var_name <- paste0(var, "_lag_", lag_i)
        fc.df[i, lagged_var_name] <- fc.df[lag_index, var]
      }
    }
  }
  
  newdata <- fc.df[i, setdiff(names(fc.df), c("Date", variables_to_forecast))]
  
  fc.df[i, "cpi_mom"]   <- predict(cpi.fit, newdata = newdata)
  fc.df[i, "sales_mom"] <- predict(sales.fit, newdata = newdata)
  fc.df[i, "reer_mom"]  <- predict(reer.fit, newdata = newdata)
  fc.df[i, "pmi_mom"]   <- predict(pmi.fit, newdata = newdata)
  # fc.df[i, "oil_mom"] <- predict(oil.fit, newdata = newdata)
}
fc.df <- fc.df %>%
  filter(Date >= forecast_start)

actual_df <- df %>%
  dplyr::select(Date, cpi_mom) %>%
  dplyr::rename(date = Date, cpi = cpi_mom) %>%
  dplyr::mutate(type = "Actual")

cpi_fc <- baseline_fc$fcst$cpi_mom[, "fcst"]

forecast_dates <- seq(forecast_start, by = "month", length.out = length(cpi_fc))

baseline_df <- tibble::tibble(
  date = forecast_dates,
  cpi = cpi_fc,
  type = "Baseline VAR"
)

custom_df <- fc.df %>%
  dplyr::select(Date, cpi_mom) %>%
  dplyr::rename(date = Date, cpi = cpi_mom) %>%
  dplyr::mutate(type = "Custom Forecast")

combined_df <- dplyr::bind_rows(actual_df, baseline_df, custom_df)

combined_df <- combined_df %>%
  mutate(date = as.Date(date))  

combined_df %>%
  filter(date >= as.Date("2022-01-01")) %>%
  ggplot(aes(x = date, y = cpi, color = type)) +
  geom_line(size = 1) +
  geom_point() +
  geom_vline(xintercept = as.Date("2025-07-01"), linetype = "dashed", color = "black") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               limits = c(as.Date("2022-01-01"), as.Date("2026-06-01"))) +
  labs(
    title = "CPI MoM: Actual vs Baseline VAR vs Custom Forecast",
    x = "Date", y = "CPI MoM",
    color = "Legend"
  ) +
  theme_minimal()

##YOY conversion 

last_cpi_level <- raw_df %>%
  filter(Date == as.Date("2025-06-01")) %>%
  pull(cpi)

cpi_custom_fc   <- last_cpi_level * cumprod(1 + fc.df$cpi_mom)
cpi_baseline_fc <- last_cpi_level * cumprod(1 + baseline_fc$fcst$cpi_mom[, "fcst"])

historical_cpi <- raw_df %>%
  dplyr::select(Date, cpi) %>%
  filter(!is.na(cpi)) %>%
  mutate(type = "Actual")

forecast_dates <- seq(from = as.Date("2025-07-01"), by = "month", length.out = length(cpi_custom_fc))

custom_forecast <- tibble(
  Date = forecast_dates,
  cpi = cpi_custom_fc,
  type = "Custom Forecast"
)

baseline_forecast <- tibble(
  Date = forecast_dates,
  cpi = cpi_baseline_fc,
  type = "Baseline Forecast"
)

cpi_combined_all <- bind_rows(historical_cpi, baseline_forecast, custom_forecast) %>%
  arrange(Date) %>%
  mutate(
    cpi_yoy = 100 * (cpi / lag(cpi, 12) - 1)
  )


cpi_combined_all <- cpi_combined_all %>%
  mutate(Date = as.Date(Date))

library(ggplot2)

ggplot(cpi_combined_all, aes(x = Date, y = cpi_yoy, color = type)) +
  geom_line(size = 1) +
  geom_point(size = 1.2, alpha = 0.7) +  # Adds clarity to monthly points
  geom_vline(xintercept = as.Date("2025-07-01"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2025-07-01"), y = max(cpi_combined_all$cpi_yoy, na.rm = TRUE),
           label = "Forecast Start", vjust = -1, hjust = -0.1, angle = 90, size = 3.5) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y",
    limits = c(as.Date("2022-01-01"), as.Date("2026-06-01"))
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_color_manual(values = c("Actual" = "firebrick", 
                                "Baseline Forecast" = "forestgreen", 
                                "Custom Forecast" = "dodgerblue")) +
  labs(
    title = "CPI YoY: Actual vs Forecasts",
    subtitle = "Indonesia, forecast starts July 2025",
    x = "Date", y = "CPI YoY (%)",
    color = "Series"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

##backtest 

library(Metrics)

backtest_start <- as.Date("2022-01-01")
backtest_end   <- as.Date("2025-05-01")
forecast_dates <- seq(backtest_start, backtest_end, by = "month")

lags <- optimal_lags_aic

for (lag_i in 1:lags) {
  for (var in c("cpi_mom", "oil_mom", "reer_mom", "sales_mom", "pmi_mom")) {
    df[[paste0(var, "_lag_", lag_i)]] <- lag(df[[var]], lag_i)
  }
}
df_lagged <- df %>% drop_na()

oil_level_start <- raw_df %>%
  filter(Date < backtest_start %m+% months(1)) %>%
  tail(1) %>%
  pull(oil_px)

if (!exists("oil_constant", envir = .GlobalEnv)) {
  oil_constant <- oil_level_start
  message("oil_constant not supplied — using last known oil price.")
}

lm_backtest <- tibble(
  date = forecast_dates + months(1),
  forecast = NA_real_,
  actual = NA_real_,
  error = NA_real_
)

for (i in seq_along(forecast_dates)) {
  
  train_date <- forecast_dates[i]
  test_date <- train_date %m+% months(1)
  
  #training data
  train <- df_lagged %>% filter(Date <= train_date)
  
  #synthetic oil path (flat)
  oil_px_vec <- c(oil_level_start, rep(oil_constant, lags))  # include one before for lagging
  oil_mom_vec <- diff(oil_px_vec) / head(oil_px_vec, -1)  # percent change
  
  #test row from original data
  test <- df_lagged %>% filter(Date == test_date)
  if (nrow(test) == 0) next  # skip if actual not available yet
  
  # replace with synthetic values
  for (lag_j in 1:lags) {
    col_name <- paste0("oil_mom_lag_", lag_j)
    if (col_name %in% names(test)) {
      test[[col_name]] <- oil_mom_vec[lag_j]
    }
  }
  
  cpi_model <- lm(cpi_mom ~ ., data = train %>%
                    dplyr::select(cpi_mom,
                                  starts_with("cpi_mom_lag_"),
                                  starts_with("oil_mom_lag_"),
                                  starts_with("reer_mom_lag_"),
                                  starts_with("sales_mom_lag_"),
                                  starts_with("pmi_mom_lag_")))
  
  test_x <- test %>%
    dplyr::select(starts_with("cpi_mom_lag_"),
                  starts_with("oil_mom_lag_"),
                  starts_with("reer_mom_lag_"),
                  starts_with("sales_mom_lag_"),
                  starts_with("pmi_mom_lag_"))
  
  fc <- predict(cpi_model, newdata = test_x)
  actual <- test$cpi_mom
  
  lm_backtest$forecast[i] <- fc
  lm_backtest$actual[i] <- actual
  lm_backtest$error[i] <- actual - fc
}

rmse_val <- rmse(lm_backtest$actual, lm_backtest$forecast)
mae_val  <- mae(lm_backtest$actual, lm_backtest$forecast)

cat("Custom LM Backtest (Real-time with synthetic oil path):\n")
cat("RMSE:", round(rmse_val, 4), "\n")
cat("MAE :", round(mae_val, 4), "\n")

ggplot(lm_backtest, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = forecast, color = "Forecast")) +
  labs(title = "Custom LM VAR (Real-time): One-Step Ahead Forecast vs Actual",
       x = "Date", y = "CPI MoM (%)", color = "Legend") +
  theme_minimal()

cpi_history <- raw_df %>%
  filter(
    Date >= (backtest_start %m-% months(12)) &
      Date < backtest_start
  ) %>%
  dplyr::select(date = Date, cpi)

forecast_cpi_index <- cumprod(1 + lm_backtest$forecast) * cpi_history$cpi[12]  # April 2024 level

cpi_forecast <- tibble(
  date = lm_backtest$date,
  cpi = forecast_cpi_index
)

cpi_all <- bind_rows(cpi_history, cpi_forecast) %>%
  arrange(date)

cpi_all <- cpi_all %>%
  mutate(cpi_yoy = 100 * (cpi / lag(cpi, 12) - 1))

#keep only forecast period dates
cpi_yoy_forecast <- cpi_all %>%
  filter(date >= min(lm_backtest$date)) %>%
  dplyr::select(Date = date, CPI_YoY = cpi_yoy)

print(cpi_yoy_forecast)

#actual CPI YOY
actual_yoy <- raw_df %>%
  arrange(Date) %>%
  mutate(
    Actual_YoY = 100 * (cpi / lag(cpi, 12) - 1)
  ) %>%
  filter(!is.na(Actual_YoY)) %>%
  dplyr::select(Date, Actual_YoY)

actual_yoy_window <- actual_yoy %>%
  filter(Date %in% cpi_yoy_forecast$Date)

#combine forecast and actual, calculate error
yoy_comparison_tbl <- cpi_yoy_forecast %>%
  rename(Custom_YoY = CPI_YoY) %>%
  inner_join(actual_yoy_window, by = "Date") %>%
  mutate(
    Error = Custom_YoY - Actual_YoY,
    Index = row_number()
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 6))) %>%
  dplyr::select(Date, Custom_YoY, Actual_YoY, Error, Index)

print(yoy_comparison_tbl)

rmse_val <- rmse(yoy_comparison_tbl$Actual_YoY, yoy_comparison_tbl$Custom_YoY)
cat("RMSE:", round(rmse_val, 6), "\n")

mae_val <- mae(yoy_comparison_tbl$Actual_YoY, yoy_comparison_tbl$Custom_YoY)
cat("MAE:", round(mae_val, 6), "\n")


ggplot(yoy_comparison_tbl, aes(x = Date)) +
  geom_line(aes(y = Actual_YoY, color = "Actual")) +
  geom_line(aes(y = Custom_YoY, color = "Custom Forecast")) +
  labs(title = "YoY CPI: Actual vs Custom Forecast",
       y = "CPI YoY (%)", x = "Date", color = "Legend") +
  theme_minimal()
