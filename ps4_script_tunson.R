# Problem Set 4


library(tidyverse)
library(fpp3)
library(BVAR)

#2.1a
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

#2.1b
liquor |>
  autoplot(sales) +
  labs(title = "U.S. Retail Sales at Beer, Wine & Liquor Stores",
       x = "Month", y = "$ (Millions)")

liquor |>
  gg_season(log(sales)) +
  labs(title = "Seasonal Plot of Log Liquor Store Sales",
       x = "Month", y = "log(sales)")
#Write up

# 2.2
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

# Write up

# 2.2b
liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)

#lb_stat = 120, lb_pvalue = 0

#2.3

liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

#liquor_cv_fit <- liquor_cv |>
#  model(snaive = SNAIVE(sales),
#        ets    = ETS(log(sales)),
#        arima  = ARIMA(log(sales)))
#
#saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")

#2.3b
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))
# |> knitr::kable(digits = 2) qmd

# ARIMA forecasts most accurately, having both a lower RMSE and MAE. 
# The two metrics do agree with a consistent ranking of errors showing 
# 

# 2.4
liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)

forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)

# For the ARIMA and ETS test, DM = -2.9602 with a p-value of 0.004042.
# For the ARIMA and Seasonal Random Walk test, DM = -10.755 with p < 2.2e-16.
# The liquor chain should use ARIMA, it had the lowest RMSE and MAE. The DM tests show
# that against ETS the DM statistic is negative so ARIMA has smaller squared errors. 
# the p-value allows us to reject the null of equal accuracy at the 1% level.
# With DM = -10.755 and a p-value close to 0 against SRW, ARIMA is also far more accurate.

# Part 3

liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))
# |> knitr::kable(digits = 2) qmd

# combo beats ets in both RMSE and MAE. However, ARIMA is still slightly more
# accurate in both errors.

#3b

cor(liquor_errs$arima, liquor_errs$ets)

# Correlation = 0.8782934



























