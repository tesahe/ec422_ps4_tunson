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

# For the ARIMA and ETS test, DM = -2.9062 with a p-value of 0.004042.
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
#  With that high of a correlation, there isn't much value to get out of combining the models.
# ETS was also signifcantly worse than ARIMA so giving them equal weight should make it worse 
# than ARIMA, making sense with the errors.

# Part 4
qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

# 4.1c
N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]

# We will produce 120 forecasts per variable. The date of the first forecast is 1990 Q1,
# and the date of the last forecast is 2019 Q4. 

# 4.2
var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

# VAR(1) forecasts better for 7 of the 8 variables. In this case I think the smaller
# forecast model forecasts better because it has many less coefficients.  
# The VAR(4) adds a lot of estimation noise and causes overfitting.

# 4.3
bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

#set.seed(422)
#fc_bvar_03 <- bvar_cv(0.3)
#saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")
fc_bvar_03 <- readRDS("results/fc_bvar_03.rds")



#set.seed(422)
#fc_bvar_005 <- bvar_cv(0.05)

#saveRDS(fc_bvar_005, "results/fc_bvar_005.rds")
fc_bvar_005 <- readRDS("results/fc_bvar_005.rds")


var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)

# The BVAR(4) wins for all 8 variables against OLS VAR(4)
# The difference is explained by shrinkage since OLS a large amount of coefficients, 64,
# that fit a lot of noise.

# Against VAR(1), BVAR(4) wins for 3 out of 8 variables, so it's mixed. 
# With lambda = 0.05, the forecasts get worse for 7 out of 8 variables. 
# Lambda is too low causing too much shrinkage, the prior shrinks every coefficient too low.
# This model is underfitting. 


# 4.3c
e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

# Consumption growth
e_var4_cons <- actual[, "cons"] - fc_var4[, 2]
e_bvar_cons <- actual[, "cons"] - fc_bvar_03[, 2]
forecast::dm.test(e_var4_cons, e_bvar_cons, h = 1, power = 2)

# For GDP, DM = 2.1366, p = 0.03468
# For Consumption, DM = 3.0264, p = 0.003034.
# DM for GDP is positive, so VAR(4) has larger squared errors. Therefore, we reject
# the null of equal accuracy at 5%. BVAR is better for GDP.
# For consumption DM is also positive, VAR(4) has larger squared errors as well. We reject
# the null at 1%. 
# Although BVAR beat VAR(4), it is only statistically significant for about half of them.
# This can easily be noise and there are many close values that definitely aren't significant.


# Part 5
md_raw <- read_csv("data/fredmd_2026-09.csv", show_col_types = FALSE)
tcode <- unlist(md_raw[1, -1])
md <- md_raw[-1, ] |> mutate(date = as.Date(sasdate, "%m/%d/%Y"))

transform_series <- function(x, code) {
  d <- function(v) c(NA, diff(v))
  switch(as.character(code),
         "1" = x,                      # level
         "2" = d(x),                   # first difference
         "3" = d(d(x)),                # second difference
         "4" = log(x),                 # log
         "5" = 100 * d(log(x)),        # growth rate
         "6" = 100 * d(d(log(x))),     # change in the growth rate
         "7" = d(x / dplyr::lag(x) - 1))
}

fm <- map2_dfc(md[names(tcode)], tcode, transform_series) |>
  mutate(date = md$date, .before = 1) |>
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

fm <- fm[, colSums(is.na(fm)) == 0]

dim(fm)

# 718 122

tcode[c("INDPRO", "PAYEMS", "CPIAUCSL", "FEDFUNDS")]

# INDPRO and PAYEMS are in their monthly growth rates. CPIAUCSL is in change in monthly inflation,
# and FEDFUNDS is is the change in percentage points, its first difference.
# FEDFUNDS is different because it's already a percentage.

#5.2
Z <- fm |> select(-date) |> as.matrix()

pc <- prcomp(scale(Z))

summary(pc)$importance[, 1:5]

# The first three factors explain about 29.5 percent of the variation.

#5.2b
F1 <- pc$x[, 1] * sign(cor(pc$x[, 1], fm$INDPRO))

tibble(date = fm$date, F1 = F1) |>
  ggplot(aes(date, F1)) +
  geom_line() +
  labs(title = "First Factor from FRED-MD", x = NULL, y = NULL) +
  theme_minimal()

#5.2c
sort(abs(cor(pc$x[, 1], Z))[1, ], decreasing = TRUE)[1:8]

sort(abs(cor(pc$x[, 2], Z))[1, ], decreasing = TRUE)[1:8]

# Factor 1 is business cycles. It's top series are employment, industrial production,
# and capacity utilization.
# Factor 2 is price / inflation. Its top series are CPI price measures. 

# 5.3
ip <- fm$INDPRO
n <- nrow(Z)
init <- which(fm$date == as.Date("1989-12-01"))

e_ar <- NULL
e_fa <- NULL
for (T in init:(n - 1)) {
  F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:3]         # factors using data through T
  d <- data.frame(y_next = ip[2:T], y = ip[1:(T - 1)], F_T[1:(T - 1), ])
  new <- data.frame(y = ip[T], t(F_T[T, ]))
  e_ar <- c(e_ar, ip[T + 1] - predict(lm(y_next ~ y, d), new))
  e_fa <- c(e_fa, ip[T + 1] - predict(lm(y_next ~ ., d), new))
}

c(RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)))

# The numbers match class with 0.616 RMSE_AR and .589 RMSE_FA.
# It would be cheating to use those because you get bias by looking ahead, 
# using information that could not have been had at the time.

#5.4
direct_race <- function(series, h = 12, r = 3) {
  lv <- md |>
    mutate(log_x = log(.data[[series]]),
           g = 100 * (log_x - lag(log_x))) |>
    filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
  yh <- 100 * (lead(lv$log_x, h) - lv$log_x)      # growth over the next h months
  g  <- lv$g                                      # growth over the latest month
  
  e_ar <- NULL
  e_fa <- NULL
  for (T in init:(n - h)) {
    F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:r, drop = FALSE]
    i <- 1:(T - h)
    d <- data.frame(yh = yh[i], g = g[i], F_T[i, , drop = FALSE])
    new <- data.frame(g = g[T], F_T[T, , drop = FALSE])
    e_ar <- c(e_ar, yh[T] - predict(lm(yh ~ g, d), new))
    e_fa <- c(e_fa, yh[T] - predict(lm(yh ~ ., d), new))
  }
  tibble(series, forecasts = length(e_ar),
         RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)),
         MAE_AR = mean(abs(e_ar)), MAE_FA = mean(abs(e_fa)),
         DM_pvalue = forecast::dm.test(e_ar, e_fa, h = h, power = 2,
                                       varestimator = "bartlett")$p.value)
}

direct_race("CPIAUCSL")

# The factors do improve the forecast, dropping RMSE from 2.1 to 1.89.
# MAE also falls from 1.58 to 1.35.
# They reduce the RMSE by 10.2%
# The difference is statistically significant with a DM p-value of 0.000443,
# we reject the null at the 1% level.






















