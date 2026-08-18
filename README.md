# What Drives House Prices in Luxembourg? Forecasting and Monetary Policy Evidence


**Date:** May 2026

## Overview

This project studies the Luxembourg housing market using quarterly data from 2007Q1 to 2025Q3. It addresses two questions:

1. Do macroeconomic fundamentals improve house price forecasts relative to a purely univariate benchmark?
2. How do house prices respond dynamically to unexpected mortgage interest rate shocks?

The analysis combines an ARDL/ECM forecasting framework benchmarked against ARIMA, with a local projections model for dynamic causal-style inference.

## Methodology

- **Data:** Quarterly Luxembourg data (2007Q1–2025Q3) — log House Price Index, mortgage interest rate, unemployment rate, consumer confidence, log housing transactions, plus Euribor and HICP for shock construction; includes a structural dummy for the 2015 tax reform.
- **Stationarity analysis:** ADF and KPSS tests to classify variables as I(0)/I(1), ruling out I(2) processes.
- **ARDL model:** Autoregressive Distributed Lag specification (lag selection via BIC) linking log(HPI) to macro fundamentals.
- **Cointegration testing:** Pesaran et al. (2001) bounds test (case 3: unrestricted intercept, no trend).
- **Error Correction Model (ECM):** Reparameterized ARDL to separate short-run dynamics from long-run adjustment.
- **ARIMA benchmark:** Univariate ARIMA(p,d,q), model order chosen via BIC, ACF/PACF diagnostics.
- **Forecast evaluation:** Pseudo out-of-sample expanding-window forecasts, RMSE/MAE, and Diebold–Mariano test for equal predictive accuracy.
- **Local projections (Jordà, 2005):** Impulse responses of log(HPI) to a residual-based mortgage rate shock, with lag augmentation and HC1-robust standard errors.
- **Diagnostics:** Breusch–Godfrey, Breusch–Pagan, Jarque–Bera, and Ljung–Box tests on model residuals.

## Key Findings

- Luxembourg house prices are **highly persistent**; own past values dominate short-run dynamics in both ARDL and ARIMA models.
- Evidence for a stable **long-run cointegrating relationship** is weak-to-borderline (bounds test p-values of 0.10 and 0.06) and not robust across specifications.
- The ARDL model slightly **outperforms ARIMA** out-of-sample (lower RMSE/MAE), but the improvement is **not statistically significant** (Diebold–Mariano test, p = 0.4).
- **Local projections show a delayed, significant negative response** of house prices to unexpected mortgage rate increases, peaking around 3–4 quarters after the shock (β₃ = −0.439, p < 0.01; β₄ = −0.390, p = 0.031), then fading at longer horizons.
- Overall: macroeconomic fundamentals add limited short-run forecasting power but are economically meaningful for understanding medium-term transmission of financing conditions to house prices.

## References

- Pesaran, M. H., Shin, Y., and Smith, R. J. (2001). Bounds testing approaches to the analysis of level relationships. *Journal of Applied Econometrics*, 16(3):289–326.
- Jordà, O. (2005). Estimation and inference of impulse responses by local projections. *American Economic Review*, 95(1):161–182.
- Engle, R. F. and Granger, C. W. J. (1987). Co-integration and error correction: Representation, estimation, and testing. *Econometrica*, 55(2):251–276.
- Diebold, F. X. and Mariano, R. S. (1995). Comparing predictive accuracy. *Journal of Business and Economic Statistics*, 13(3):253–263.
- Dickey, D. A. and Fuller, W. A. (1979). Distribution of the estimators for autoregressive time series with a unit root. *JASA*, 74(366):427–431.
- Kwiatkowski, D., Phillips, P. C. B., Schmidt, P., and Shin, Y. (1992). Testing the null hypothesis of stationarity against the alternative of a unit root. *Journal of Econometrics*, 54(1–3):159–178.
- Montiel Olea, J. L. and Plagborg-Møller, M. (2021). Local projection inference is simpler and more robust than you think. *Econometrica*, 89(4):1789–1823.
- Box, G. E. P. and Jenkins, G. M. (1970). *Time Series Analysis: Forecasting and Control*. Holden-Day.
