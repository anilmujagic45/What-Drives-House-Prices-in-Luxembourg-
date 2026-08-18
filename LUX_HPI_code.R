#==============================================================================#
# Empirical Analysis: Dynamic Impact of Interest Rate Innovations on Housing Prices
#==============================================================================#

# Author: Anil Mujagic
# Student-ID: 65067

#==============================================================================#
# Packages                                                                     #
#==============================================================================#

# Install required packages 
install.packages(c(
  "tidyverse",     # dplyr, ggplot2, tidyr, etc.
  "lubridate",
  "readxl",
  "zoo",
  "rio",
  "tseries",
  "lmtest",
  "sandwich",
  "forecast",
  "car",
  "urca",
  "ARDL",
  "dynlm",
  "gridExtra",
  "data.table",
  "fixest",
  "eurostat",
  "rsdmx",
  "TSA",
  "vars",
  "collapse",
  "curl"
))

# Load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)
library(rio)
library(tseries)
library(lmtest)
library(sandwich)
library(forecast)
library(car)
library(urca)
library(ARDL)
library(dynlm)
library(gridExtra)
library(grid)
library(data.table)
library(fixest)
library(eurostat)
library(rsdmx)
library(TSA)
library(vars)
library(collapse)
library(curl)


#===========================================#
# Set Directory
#===========================================#


setwd("C:\\Users\\anilm\\Documents\\LUX Rental INDEX\\Data")

#===========================================#
# 1) Data Import:
#===========================================#

# Interest Rates:

interest_rate_raw <- read_excel(
  "Loan_rates_unemp.xlsx",
  sheet = "Tabelle1",
  col_names = TRUE
)
interest_rate_raw


# HPI:

LU_HPI_raw <- read_excel(
  "d4016.xlsx",
  sheet = "Indices b15",
  col_names = FALSE
)

# Unemployment in %:

unemp_rate_raw <- read_excel(
  "Loan_rates_unemp.xlsx",
  sheet = "Tabelle2",
  col_names = FALSE
)


# Consumer Confidence

consumer_conf_raw <- read_excel(
  "ConsumerConfidence.xls",
  sheet = "Table 05.05_seas.adj.",
  col_names = FALSE
)

# Transactions:

transactions_raw <- read_excel(
  "d4016.xlsx",
  sheet = "actes de vente & vol. fin.",
  col_names = FALSE
)




#===========================================#
# 2) Data cleansing:
#===========================================#

# Interest Rate:

interest_rate <- interest_rate_raw %>%
  dplyr::slice(-1) %>%
  dplyr::rename(
    ym = Date,
    interest_rate = `Interest Rate`
  ) %>%
  dplyr::filter(!is.na(ym)) %>%                
  dplyr::filter(grepl("^[0-9]{6}$", ym)) %>%    
  dplyr::mutate(
    date = lubridate::ymd(paste0(ym, "01")),
    interest_rate = as.numeric(interest_rate)
  ) %>%
  dplyr::select(date, interest_rate)


interest_rate_q <- interest_rate %>%
  dplyr::mutate(
    year = lubridate::year(date),
    quarter = lubridate::quarter(date)
  ) %>%
  dplyr::group_by(year, quarter) %>%
  dplyr::summarise(
    interest_rate = mean(interest_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    qdate = zoo::as.yearqtr(paste(year, quarter), format = "%Y %q")
  ) %>%
  dplyr::select(qdate, interest_rate)


interest_rate_q

# HPI:

HPI_values <- as.numeric(LU_HPI_raw[2, -1])
HPI_ts <- ts(HPI_values, start = c(2007, 1), frequency = 4)

qdate <- as.yearqtr(time(HPI_ts))

HPI_df <- tibble(
  qdate = qdate,
  Hpi   = HPI_values
)

# Unemployment rate:

unemp_rate <- unemp_rate_raw %>%
  dplyr::slice(-1) %>%
  dplyr::rename(
    ym = ...1,
    unemp_rate = ...2
  ) %>%
  dplyr::mutate(
    ym = as.character(ym),
    date = lubridate::ymd(paste0(ym, "01")),
    unemp_rate = as.numeric(unemp_rate)
  ) %>%
  dplyr::select(date, unemp_rate)


unemp_rate_q <- unemp_rate %>%
  dplyr::mutate(
    year = lubridate::year(date),
    quarter = lubridate::quarter(date)
  ) %>%
  dplyr::group_by(year, quarter) %>%
  dplyr::summarise(
    unemp_rate = mean(unemp_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    qdate = zoo::as.yearqtr(paste(year, quarter), format = "%Y %q")
  ) %>%
  dplyr::select(qdate, unemp_rate)


# Consumer Confidence:


consumer_conf <- consumer_conf_raw %>%
  dplyr::slice(6:dplyr::n()) %>%
  dplyr::select(1:2) %>%
  dplyr::rename(month = 1, consumer_confidence = 2) %>%
  dplyr::mutate(
    month = as.Date(as.numeric(month), origin = "1899-12-30"),
    consumer_confidence = as.numeric(consumer_confidence)
  )


consumer_conf_q <- consumer_conf %>%
  dplyr::mutate(
    year = lubridate::year(month),
    quarter = lubridate::quarter(month)
  ) %>%
  dplyr::group_by(year, quarter) %>%
  dplyr::summarise(
    consumer_confidence = mean(consumer_confidence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    qdate = zoo::as.yearqtr(paste(year, quarter), format = "%Y %q")
  ) %>%
  dplyr::select(qdate, consumer_confidence)


# Transactions 

transactions_values <- as.numeric(transactions_raw[3, -1])
transactions_ts <- ts(transactions_values, start = c(2007, 1), frequency = 4)


transactions_df <- tibble(
  qdate = as.yearqtr(time(transactions_ts)),
  actes = as.numeric(transactions_ts)
)

df_master <- HPI_df %>%
  left_join(interest_rate_q,  by = "qdate") %>%
  left_join(unemp_rate_q,     by = "qdate") %>%
  left_join(consumer_conf_q,  by = "qdate") %>%
  left_join(transactions_df,  by = "qdate") %>%
  arrange(qdate)

df_master





df_master <- df_master %>%
  mutate(
    log_Hpi = log(Hpi),
    dummy_2015 = ifelse(qdate >= as.yearqtr("2015 Q1"), 1, 0),
    log_actes = log(actes)
  ) %>%
  arrange(qdate) %>%
  mutate(
    log_Hpi_lag1 = lag(log_Hpi, 1),
    rate_lag1    = lag(interest_rate, 1),
    unemp_lag1   = lag(unemp_rate, 1),
    cc_lag1      = lag(consumer_confidence, 1),
    log_actes_lag1 = lag(log_actes, 1)
  )

head(df_master)
summary(df_master)
max(df_master$qdate)


#===========================================#
# 3) Data Analysis and Stationarity Tests:
#===========================================#

# Plots

df_master$qdate <- as.yearqtr(df_master$qdate)


plot_theme <- theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13),
    panel.grid.minor = element_blank()
  )


p1 <- ggplot(df_master, aes(x = qdate, y = log_Hpi)) +
  geom_line(color = "#1f4e79", linewidth = 1.2) +
  labs(
    title = "Log House Price Index",
    subtitle = "Luxembourg (Quarterly Data)",
    x = "Time",
    y = "log(HPI)"
  ) +
  scale_x_yearqtr(format = "%Y", n = 6) +
  plot_theme



# p2: Interest Rate
p2 <- ggplot(df_master, aes(x = qdate, y = interest_rate)) +
  geom_line(color = "#1f4e79", linewidth = 1) +
  labs(
    title = "Interest Rate",
    x = NULL,
    y = "%"
  ) +
  scale_x_yearqtr(format = "%Y", n = 6) +
  plot_theme

# p3: Unemployment
p3 <- ggplot(df_master, aes(x = qdate, y = unemp_rate)) +
  geom_line(color = "#1f4e79", linewidth = 1) +
  labs(
    title = "Unemployment Rate",
    x = NULL,
    y = "%"
  ) +
  scale_x_yearqtr(format = "%Y", n = 6) +
  plot_theme

# p4: Consumer Confidence
p4 <- ggplot(df_master, aes(x = qdate, y = consumer_confidence)) +
  geom_line(color = "#1f4e79", linewidth = 1) +
  labs(
    title = "Consumer Confidence",
    x = NULL,
    y = "Index"
  ) +
  scale_x_yearqtr(format = "%Y", n = 6) +
  plot_theme

# p5: Transactions (Actes)
p5 <- ggplot(df_master, aes(x = qdate, y = actes)) +
  geom_line(color = "#1f4e79", linewidth = 1) +
  labs(
    title = "Housing Transactions",
    x = NULL,
    y = "Count"
  ) +
  scale_x_yearqtr(format = "%Y", n = 6) +
  plot_theme


# Export als PDF (beste Qualität)
pdf("hpi_plot.pdf", width = 10, height = 6)
print(p1)
dev.off()

# Export als PDF (scharf für Beamer)
pdf("ts_overview_p2_p5.pdf", width = 11, height = 8)

grid.arrange(
  p2, p3,
  p4, p5,
  ncol = 2
)

dev.off()
#





#===========================================#
# Stationary TEST:
#===========================================#


# log(HPI): trend in level
ur.df(df_master$log_Hpi, type = "trend", selectlags = "AIC")
kpss.test(df_master$log_Hpi, null = "Trend")

# interest rate: trend in level
ur.df(df_master$interest_rate, type = "trend", selectlags = "AIC")
kpss.test(df_master$interest_rate, null = "Trend")

# unemployment: drift in level
ur.df(df_master$unemp_rate, type = "drift", selectlags = "AIC")
kpss.test(df_master$unemp_rate, null = "Level")

# consumer confidence: drift in level
ur.df(df_master$consumer_confidence, type = "drift", selectlags = "AIC")
kpss.test(df_master$consumer_confidence, null = "Level")

# log transactions: drift, plus robustness with trend
ur.df(df_master$log_actes, type = "drift", selectlags = "AIC")
kpss.test(df_master$log_actes, null = "Level")



ur.df(df_master$log_actes, type = "trend", selectlags = "AIC")
kpss.test(df_master$log_actes, null = "Trend")

ur.df(diff(df_master$log_actes), type = "drift", selectlags = "AIC")
kpss.test(diff(df_master$log_actes), null = "Level")



d_log_Hpi <- na.omit(diff(df_master$log_Hpi))
d_interest_rate <- na.omit(diff(df_master$interest_rate))
d_log_actes <- na.omit(diff(df_master$log_actes))

ur.df(d_log_Hpi, type = "drift", selectlags = "AIC")
kpss.test(d_log_Hpi, null = "Level")

ur.df(d_interest_rate, type = "drift", selectlags = "AIC")
kpss.test(d_interest_rate, null = "Level")

ur.df(d_log_actes, type = "drift", selectlags = "AIC")
kpss.test(d_log_actes, null = "Level")

#===========================================#

#ACF/PACF Plots

acf(df_master$log_Hpi,
    lag.max = 40,
    main    = "ACF:LOG HPI",
    col     = "#003f5c",
    lwd     = 2)

pacf(df_master$log_Hpi,
     lag.max = 40,
     main    = "PACF: LOG HPI",
     col     = "#003f5c",
     lwd     = 2)

acf(df_master$log_actes,
    lag.max = 40,
    main    = "ACF:LOG Actes",
    col     = "#003f5c",
    lwd     = 2)

pacf(df_master$log_actes,
     lag.max = 40,
     main    = "PACF: LOG actes",
     col     = "#003f5c",
     lwd     = 2)

acf(df_master$unemp_rate,
    lag.max = 40,
    main    = "ACF: Unemp Rates",
    col     = "#003f5c",
    lwd     = 2)

pacf(df_master$unemp_rate,
     lag.max = 40,
     main    = "PACF: Unemp Rates",
     col     = "#003f5c",
     lwd     = 2)





#===========================================#
# 4) ARDL ECM Framework
#===========================================#

# Model 1 -3 are the Models that were presented in Class

# Model 1: without consumer confidence

model_1 <- auto_ardl(
  formula = log_Hpi ~ interest_rate + unemp_rate + log_actes + dummy_2015,
  data = df_master,
  max_order = c(
    log_Hpi = 3,
    interest_rate = 2,
    unemp_rate = 2,
    log_actes = 2,
    dummy_2015 = 1
  ),
  fixed_order = c(
    log_Hpi = -1,
    interest_rate = -1,
    unemp_rate = -1,
    log_actes = -1,
    dummy_2015 = 0
  ),
  selection = "BIC"
)

cat("\n===== BEST MAIN ARDL MODEL =====\n")
print(model_1$best_order)
summary(model_1$best_model)



# Modell 2: including consumer confidence
model_2 <- auto_ardl(
  formula = log_Hpi ~ interest_rate + unemp_rate + consumer_confidence + log_actes + dummy_2015,
  data = df_master,
  max_order = c(
    log_Hpi = 3,
    interest_rate = 2,
    unemp_rate = 2,
    log_actes = 2,
    consumer_conf = 2,
    dummy_2015 = 1
  ),
  fixed_order = c(
    log_Hpi = -1,
    interest_rate = -1,
    unemp_rate = -1,
    log_actes = -1,
    consumer_conf = -1,
    dummy_2015 = 0
  ),
  selection = "BIC"
)

cat("\n===== BEST EXTENDED ARDL MODEL =====\n")
print(model_2$best_order)
summary(model_2$best_model)




# Modell 3: without log_actes
model_3 <- auto_ardl(
  formula = log_Hpi ~ interest_rate + unemp_rate + consumer_confidence  ,
  data = df_master,
  max_order = c(
    log_Hpi = 3,
    interest_rate = 2,
    unemp_rate = 2,
    consumer_conf = 2
  ),
  selection = "BIC"
)

cat("\n===== BEST EXTENDED ARDL MODEL =====\n")
print(model_3$best_order)
summary(model_3$best_model)


# ============================================================
# Model specification choice
# ============================================================
# Following the feedback from the presentation, the preferred ARDL specification
# is adjusted to include additional lags of log(HPI) in order to better capture
# the strong persistence in house price dynamics.
#
# The interest rate is included only in lagged form, reflecting the delayed
# transmission of monetary policy to the housing market.
#
# Housing transactions (log_actes) are excluded from the baseline specification,
# as they are likely to be jointly determined with house prices. Including them
# may therefore introduce endogeneity and simultaneity bias.
#
# Specifications including log_actes are reported separately as robustness checks.

# Main Model:

df_master <- df_master %>%
  arrange(qdate) %>%
  mutate(
    rate_lag1 = dplyr::lag(interest_rate, 1)
  )

model_main <- auto_ardl(
  formula = log_Hpi ~ rate_lag1 + unemp_rate + consumer_confidence + dummy_2015,
  data = df_master,
  max_order = c(
    log_Hpi = 3,
    rate_lag1 = 2,
    unemp_rate = 2,
    consumer_confidence = 2,
    dummy_2015 = 0
  ),
  fixed_order = c(
    log_Hpi = 2,
    rate_lag1 = -1,
    unemp_rate = -1,
    consumer_confidence = -1,
    dummy_2015 = 0
  ),
  selection = "BIC"
)


cat("\n===== BEST MAIN ARDL MODEL =====\n")
print(model_main$best_order)
summary(model_main$best_model)

# Model for robustness including transactions
df_master <- df_master %>%
  arrange(qdate) %>%
  mutate(
    actes_lag1 = dplyr::lag(log_actes, 1)
  )


model_robust <- auto_ardl(
  formula = log_Hpi ~ rate_lag1 + unemp_rate + consumer_confidence + actes_lag1 + dummy_2015,
  data = df_master,
  max_order = c(
    log_Hpi = 3,
    rate_lag1 = 2,
    unemp_rate = 2,
    consumer_confidence = 2,
    actes_lag1 = 2,
    dummy_2015 = 0
  ),
  fixed_order = c(
    log_Hpi = 2,
    rate_lag1 = -1,
    unemp_rate = -1,
    consumer_confidence = -1,
    actes_lag1 = -1,
    dummy_2015 = 0
  ),
  selection = "BIC"
)


cat("\n===== BEST MAIN ARDL MODEL =====\n")
print(model_robust$best_order)
summary(model_robust$best_model)



############################################
# COMPARE MODELS BY BIC / AIC
############################################

cat("\n\n===== MODEL COMPARISON =====\n")

cat("Model 1 AIC:", AIC(model_1$best_model), "\n")
cat("Model 1 BIC:", BIC(model_1$best_model), "\n")

cat("Model 2 AIC:", AIC(model_2$best_model), "\n")
cat("Model 2 BIC:", BIC(model_2$best_model), "\n")

cat("Model 3 AIC:", AIC(model_3$best_model), "\n")
cat("Model 3 BIC:", BIC(model_3$best_model), "\n")

cat("Main Model AIC:", AIC(model_main$best_model), "\n")
cat("Main Model BIC:", BIC(model_main$best_model), "\n")

cat("Robust Model AIC:", AIC(model_robust$best_model), "\n")
cat("Robust Model BIC:", BIC(model_robust$best_model), "\n")


############################################
# BOUNDS TEST FOR COINTEGRATION
############################################

# case = 3:
# unrestricted intercept, no deterministic trend

cat("\n\n===== BOUNDS TEST MODEL 1 (CLASS SPECIFICATION) =====\n")
bt1 <- bounds_f_test(model_1$best_model, case = 3)
print(bt1)

cat("\n\n===== BOUNDS TEST MODEL 2 (CLASS SPECIFICATION) =====\n")
bt2 <- bounds_f_test(model_2$best_model, case = 3)
print(bt2)

cat("\n\n===== BOUNDS TEST MODEL 3 (CLASS SPECIFICATION) =====\n")
bt3 <- bounds_f_test(model_3$best_model, case = 3)
print(bt3)

cat("\n\n===== BOUNDS TEST MAIN MODEL (PREFERRED) =====\n")
bt_main <- bounds_f_test(model_main$best_model, case = 3)
print(bt_main)

cat("\n\n===== BOUNDS TEST ROBUSTNESS MODEL (WITH LOG_ACTES) =====\n")
bt_robust <- bounds_f_test(model_robust$best_model, case = 3)
print(bt_robust)





############################################
# LONG-RUN RELATIONSHIP
############################################

cat("\n\n===== LONG-RUN COEFFICIENTS MODEL 1 =====\n")
lr1 <- multipliers(model_1$best_model, type = "lr")
print(lr1)

cat("\n\n===== LONG-RUN COEFFICIENTS MODEL 2 =====\n")
lr2 <- multipliers(model_2$best_model, type = "lr")
print(lr2)

cat("\n\n===== LONG-RUN COEFFICIENTS MODEL 3 =====\n")
lr3 <- multipliers(model_3$best_model, type = "lr")
print(lr3)

cat("\n\n===== LONG-RUN COEFFICIENTS MAIN MODEL =====\n")
lr_main <- multipliers(model_main$best_model, type = "lr")
print(lr_main)

cat("\n\n===== LONG-RUN COEFFICIENTS ROBUST MODEL =====\n")
lr_robust <- multipliers(model_robust$best_model, type = "lr")
print(lr_robust)




############################################
# ERROR CORRECTION MODEL (ECM)
############################################

ecm1 <- recm(model_1$best_model, case = 3)
summary(ecm1)

ecm2 <- recm(model_2$best_model, case = 3)
summary(ecm2)

ecm3 <- recm(model_3$best_model, case = 3)
summary(ecm3)


cat("\n\n===== ECM MAIN MODEL (PREFERRED) =====\n")
ecm_main <- recm(model_main$best_model, case = 3)
summary(ecm_main)

cat("\n\n===== ECM ROBUST MODEL (WITH ACTES) =====\n")
ecm_robust <- recm(model_robust$best_model, case = 3)
summary(ecm_robust)


# ============================================================
# ARDL Model Diagnostics (Preferred Model: Main Model)
# ============================================================


# ------------------------------------------------------------
# Extract residuals from the ARDL model
# ------------------------------------------------------------
res_ardl <- residuals(model_main$best_model)

# ------------------------------------------------------------
# 1. Test for serial correlation (Breusch-Godfrey)
# ------------------------------------------------------------
# Null hypothesis: no serial correlation
# If p-value > 0.05 → no evidence of autocorrelation

bg_test <- bgtest(model_main$best_model, order = 4)
print(bg_test)

# ------------------------------------------------------------
# 2. Test for heteroskedasticity (Breusch-Pagan)
# ------------------------------------------------------------
# Null hypothesis: homoskedasticity (constant variance)
# If p-value > 0.05 → no heteroskedasticity problem

bp_test <- bptest(model_main$best_model)
print(bp_test)

# ------------------------------------------------------------
# 3. Test for normality (Jarque-Bera)
# ------------------------------------------------------------
# Null hypothesis: residuals are normally distributed
# If p-value > 0.05 → no strong deviation from normality

jb_test <- jarque.bera.test(res_ardl)
print(jb_test)

# ------------------------------------------------------------
# 4. Optional: Ljung-Box test (additional check)
# ------------------------------------------------------------
# Used to confirm absence of autocorrelation
# (More common in time series than regression context)

lb_test <- Box.test(res_ardl, lag = 8, type = "Ljung-Box")
print(lb_test)

# ------------------------------------------------------------
# Summary output for easy interpretation
# ------------------------------------------------------------
cat("\n=== ARDL Model Diagnostics Summary ===\n")

cat("\nBreusch-Godfrey Test (Serial Correlation):\n")
print(bg_test)

cat("\nBreusch-Pagan Test (Heteroskedasticity):\n")
print(bp_test)

cat("\nJarque-Bera Test (Normality):\n")
print(jb_test)

cat("\nLjung-Box Test (Residual Autocorrelation):\n")
print(lb_test)


#===========================================#
# 5) ARIMA
#===========================================#



ARIMA <- auto.arima(
  df_master$log_Hpi,
  d = 1,
  seasonal = FALSE,
  ic = "bic",
  stepwise = FALSE,
  approximation = FALSE
)
summary(ARIMA)




d_log_Hpi <- diff(df_master$log_Hpi)

p1 <- ggAcf(d_log_Hpi) + ggtitle("ACF of Δlog(HPI)")
p2 <- ggPacf(d_log_Hpi) + ggtitle("PACF of Δlog(HPI)")

pdf("acf_pacf_hpi.pdf", width = 8, height = 4)
grid.arrange(p1, p2, ncol = 2)
dev.off()

# Diagnostic Checks:


res_arima <- residuals(ARIMA)

# Ljung-Bos autocorrelation test
Box.test(res_arima, lag = 8, type = "Ljung-Box")


# Jarque-Bera Test 
jarque.bera.test(res_arima)



#===========================================#
# 6) Forecaste Evaluation
#===========================================#

# =========================================================
# I) Data Prep
# =========================================================

df <- df_master %>%
  arrange(qdate) %>%
  filter(
    !is.na(log_Hpi),
    !is.na(interest_rate),
    !is.na(unemp_rate),
    !is.na(consumer_confidence),
    !is.na(log_actes),
    !is.na(dummy_2015)
  ) %>%
  as.data.frame()

# =========================================================
# II) Helper function:
#    Generates one-step-ahead forecasts from the ARDL model in levels
# =========================================================


manual_predict_ardl_one_step <- function(best_model, data_full, i_forecast) {
  coefs <- coef(best_model)
  coef_names <- names(coefs)
  
  yhat <- 0
  
  for (j in seq_along(coefs)) {
    cname <- coef_names[j]
    beta  <- coefs[j]
    
    # Intercept
    if (cname == "(Intercept)") {
      yhat <- yhat + beta
      next
    }
    
    # Lag-Terme erkennen, z.B. "L(log_rpi, 1)"
    if (grepl("^L\\(", cname)) {
      inside <- sub("^L\\((.*)\\)$", "\\1", cname)
      parts  <- strsplit(inside, ",")[[1]]
      var    <- trimws(parts[1])
      lag_k  <- as.numeric(trimws(parts[2]))
      
      row_index <- i_forecast - lag_k
      
      if (row_index < 1 || !(var %in% names(data_full)) || is.na(data_full[row_index, var])) {
        return(NA_real_)
      }
      
      xval <- data_full[row_index, var]
      yhat <- yhat + beta * xval
      
    } else {
      # zeitgleiche Variable, z.B. "interest_rate"
      # Das ist nur dann sauber, wenn sie zum Prognosezeitpunkt bekannt ist.
      if (!(cname %in% names(data_full)) || is.na(data_full[i_forecast, cname])) {
        return(NA_real_)
      }
      
      xval <- data_full[i_forecast, cname]
      yhat <- yhat + beta * xval
    }
  }
  
  return(as.numeric(yhat))
}

# =========================================================
# III) Settings
# =========================================================

initial_train_size <- 60


results <- data.frame(
  qdate        = df$qdate[(initial_train_size + 1):nrow(df)],
  actual       = NA_real_,
  fc_arima     = NA_real_,
  fc_ardl      = NA_real_,
  stringsAsFactors = FALSE
)

# Models / Lags / RECM
ardl_orders <- list()
recm_models <- list()
arima_models <- list()

# =========================================================
# IV) Forecast-loop
# =========================================================

for (i in (initial_train_size + 1):nrow(df)) {
  
  train_df <- df[1:(i - 1), ]
  
  # -------------------------------------------------------
  # A) ARIMA Benchmark
  # -------------------------------------------------------
  fit_arima <- auto.arima(
    train_df$log_Hpi,
    d = 1,
    seasonal = FALSE,
    ic = "bic",
    stepwise = FALSE,
    approximation = FALSE
  )
  
  pred_arima <- forecast(fit_arima, h = 1)$mean[1]
  
  # -------------------------------------------------------
  # B) ARDL Main Model using BIC
  #    New preferred specification:
  #    - lagged interest rate only: rate_lag1
  #    - no log_actes in main model
  #    - richer HPI dynamics
  # -------------------------------------------------------
  fit_ardl <- auto_ardl(
    formula = log_Hpi ~ rate_lag1 + unemp_rate + consumer_confidence + dummy_2015,
    data = train_df,
    max_order = c(
      log_Hpi = 3,
      rate_lag1 = 2,
      unemp_rate = 2,
      consumer_confidence = 2,
      dummy_2015 = 0
    ),
    fixed_order = c(
      log_Hpi = 2,
      rate_lag1 = -1,
      unemp_rate = -1,
      consumer_confidence = -1,
      dummy_2015 = 0
    ),
    selection = "BIC"
  )
  
  best_ardl <- fit_ardl$best_model
  
  # -------------------------------------------------------
  # C) Optional RECM deviation
  #    Only auxiliary, since cointegration evidence is weak
  # -------------------------------------------------------
  recm_fit <- tryCatch(
    recm(best_ardl, case = 3),
    error = function(e) NA
  )
  
  # -------------------------------------------------------
  # D) ARDL Forecast
  # -------------------------------------------------------
  pred_ardl <- manual_predict_ardl_one_step(
    best_model = best_ardl,
    data_full = df,
    i_forecast = i
  )
  
  # -------------------------------------------------------
  # E) Save Results
  # -------------------------------------------------------
  idx <- i - initial_train_size
  
  results$actual[idx]   <- df$log_Hpi[i]
  results$fc_arima[idx] <- as.numeric(pred_arima)
  results$fc_ardl[idx]  <- as.numeric(pred_ardl)
  
  arima_models[[length(arima_models) + 1]] <- fit_arima
  ardl_orders[[length(ardl_orders) + 1]]   <- fit_ardl$best_order
  recm_models[[length(recm_models) + 1]]   <- recm_fit
}

arima_models
ardl_orders
recm_models

# =========================================================
# V) Metrics
# =========================================================

results_clean <- results %>%
  filter(
    !is.na(actual),
    !is.na(fc_arima),
    !is.na(fc_ardl)
  ) %>%
  mutate(
    err_arima = actual - fc_arima,
    err_ardl  = actual - fc_ardl
  )

rmse_arima <- sqrt(mean(results_clean$err_arima^2))
rmse_ardl  <- sqrt(mean(results_clean$err_ardl^2))

mae_arima  <- mean(abs(results_clean$err_arima))
mae_ardl   <- mean(abs(results_clean$err_ardl))

metrics <- data.frame(
  Model = c("ARIMA", "ARDL_RECM"),
  RMSE  = c(rmse_arima, rmse_ardl),
  MAE   = c(mae_arima, mae_ardl)
)

print(metrics)

# =========================================================
# VI) Diebold-Mariano-Test
# =========================================================

dm_res <- dm.test(
  e1 = results_clean$err_arima,
  e2 = results_clean$err_ardl,
  h = 1,
  power = 2,
  alternative = "two.sided"
)

print(dm_res)

# =========================================================
# VII) Plot
# =========================================================

pdf("forecast_comparison.pdf", width = 10, height = 6)

plot(
  results_clean$qdate, results_clean$actual,
  type = "l", lwd = 2,
  xlab = "Date", ylab = "log_rpi",
  main = "Pseudo-out-of-sample forecast comparison"
)

lines(results_clean$qdate, results_clean$fc_arima, lwd = 2, lty = 2)
lines(results_clean$qdate, results_clean$fc_ardl,  lwd = 2, lty = 3)

legend(
  "topleft",
  legend = c("Actual", "ARIMA", "ARDL/RECM"),
  lty = c(1, 2, 3),
  lwd = 2,
  bty = "n"
)

dev.off()
# =========================================================
# VIII)  # Models / Lags / RECM
# =========================================================

print(ardl_orders)
print(recm_models)
print(arima_models)





#==============================================================================#
# 7) Local Projection Analysis                                                 #
#==============================================================================#


#==============================================================================#
# 7.1 External Data Import                                                     #
#==============================================================================#

# -------- Euribor --------
euribor_raw <- fread("IR3TIB01EZM156N.csv")
euribor_raw[, qdate := as.yearqtr(observation_date)]

euribor_q <- euribor_raw[
  , .(euribor = mean(IR3TIB01EZM156N, na.rm = TRUE)),
  by = qdate
]


# -------- HICP                                        #


hicp_raw <- read.csv(
  "HICP.csv",
  stringsAsFactors = FALSE
)



setDT(hicp_raw)


hicp <- hicp_raw[
  , .(
    hicp = mean(HICP...Overall.index...ICP.M.LU.N.000000.4.INX., na.rm = TRUE)
  ),
  by = .(qdate = zoo::as.yearqtr(as.Date(DATE)))
]

hicp <- hicp[, .(hicp = mean(hicp, na.rm = TRUE)), by = qdate]
hicp[, log_hicp := log(hicp)]
#==============================================================================#
# 7.2 Data Preparation                                                         #
#==============================================================================#

dt <- df_master %>%
  dplyr::select(
    qdate, 
    HPI = log_Hpi, 
    i   = interest_rate,
    u   = unemp_rate
  ) %>%
  tidyr::drop_na()

setDT(dt)
dt[, qdate := as.yearqtr(qdate)]
setorder(dt, qdate)

#==============================================================================#
# 7.3 Merge External Data                                                      #
#==============================================================================#

dt <- merge(dt, euribor_q, by = "qdate", all.x = TRUE)
dt <- merge(dt, hicp,      by = "qdate", all.x = TRUE)

#==============================================================================#
# 7.4 Construction of Lagged Variables                                         #
#==============================================================================#

dt[, HPI_l1 := shift(HPI, 1)]
dt[, HPI_l2 := shift(HPI, 2)]
dt[, HPI_l3 := shift(HPI, 3)]

dt[, i_lag1       := shift(i, 1)]
dt[, euribor_lag1 := shift(euribor, 1)]
dt[, hicp_lag1    := shift(log_hicp, 1)]
dt[, u_lag1       := shift(u, 1)]

dt <- drop_na(dt)
dt
#==============================================================================#
# 7.5 Control Selection Diagnostics                                            #
#==============================================================================#

model <- fixest::feols(HPI ~ HPI_l1 + HPI_l2, data = dt)

LB.test(model, lag = 10, type = c("Ljung-Box"))
acf(residuals(model))

#==============================================================================#
# 7.6 Shock Construction (Residualization)                                     #
#==============================================================================#

model_i <- feols(
  i ~ i_lag1 + euribor_lag1 + hicp_lag1 + u_lag1,
  data = dt,
  vcov = "HC1"
)

dt[, shock := as.numeric(residuals(model_i))]

#==============================================================================#
# 7.7 Local Projection Setup                                                   #
#==============================================================================#

p <- 2 
H <- 6

for (j in 1:(p + H)) {
  dt[, paste0("HPI_l", j) := shift(HPI, j)]
}

#==============================================================================#
# 7.8 Estimation of Impulse Response Functions                                 #
#==============================================================================#

irf  <- numeric(H + 1)
se   <- numeric(H + 1)
tval <- numeric(H + 1)
pval <- numeric(H + 1)

for (h in 0:H) {
  dt[, y_h := shift(HPI, type = "lead", n = h)]
  
  rhs_lags <- paste0("HPI_l", 1:(p + h), collapse = " + ")
  controls <- "i_lag1 + euribor_lag1 + hicp_lag1 + u_lag1"
  
  fml <- as.formula(
    paste0("y_h ~ shock + ", rhs_lags, " + ", controls)
  )
  
  mod <- fixest::feols(fml, data = dt, vcov = "HC1")
  
  b <- coef(mod)["shock"]
  V <- vcov(mod)
  s <- sqrt(V["shock","shock"])
  
  irf[h + 1]  <- b
  se[h + 1]   <- s
  tval[h + 1] <- b / s
  pval[h + 1] <- 2 * pnorm(-abs(tval[h + 1]))
}

#==============================================================================#
# 7.9 IRF Storage and Pointwise Confidence Intervals                           #
#==============================================================================#

irf_dt <- data.table(
  horizon = 0:H,
  beta    = irf,
  se      = se,
  t       = tval,
  p       = pval
)

irf_dt[, `:=`(
  lo = beta - 1.96 * se,
  up = beta + 1.96 * se
)]
irf_dt
#==============================================================================#
# 7.10 Visualization of IRFs                                                   #
#==============================================================================#

p <- ggplot(irf_dt, aes(x = horizon, y = beta)) +
  geom_ribbon(aes(ymin = lo, ymax = up), fill = "royalblue", alpha = 0.2) +
  geom_line(color = "navy", size = 1) +
  geom_point(color = "navy", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()

ggsave(
  filename = "irf_plot.pdf",
  plot = p,
  width = 8,
  height = 5
)



