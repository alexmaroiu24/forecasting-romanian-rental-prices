# Forecasting-Rental-Prices-in-Romania-A-Time-Series-and-Econometric-Approach
Time series forecasting and econometric analysis of the Romanian rental market using ARIMA, GARCH, Holt-Winters and VECM models.

# Overview

Housing affordability has become one of the most important economic challenges across Europe. Rising rental prices, inflationary pressures, and limited housing supply have increased the financial burden on households and transformed the rental market into a key driver of economic inequality.

This project investigates the evolution of rental prices in Romania between May 2007 and February 2026 using advanced time series econometric techniques. The analysis combines both univariate and multivariate approaches to understand historical dynamics, identify long-run relationships, and generate reliable forecasts for the Romanian rental market.

# Research Objectives

# The project aims to answer the following questions:

Which forecasting model provides the highest predictive accuracy for rental prices in Romania?
Is there a long-run equilibrium relationship between rents, real wages, mortgage interest rates and the EUR/RON exchange rate?
Which macroeconomic variable contains the strongest predictive information for rental prices?
How quickly does the rental market adjust after a macroeconomic shock?
What is the expected trajectory of rental prices during 2026–2028?

# Data
Period
May 2007 – February 2026

# Frequency
Monthly observations

# Sources
Eurostat
National Bank of Romania (BNR)
European Central Bank (ECB)

# Variables
# Univariate Analysis
HICP Rental Index (Actual Rentals for Housing)

# Multivariate Analysis
Rental Price Index (HICP Rent)
Real Wage Index
Mortgage Interest Rate (APRC)
EUR/RON Exchange Rate

# Methodology
# Univariate Time Series Analysis
The Box-Jenkins methodology was applied to identify the best forecasting model.

Main steps:
Exploratory Data Analysis
Stationarity Testing
Training/Test Split
SARIMA Model Selection
Exponential Smoothing Models
Forecast Evaluation

Models evaluated:
SARIMA with Drift
Holt Linear Trend
Holt-Winters Additive
Holt-Winters Multiplicative

Forecast accuracy metrics:
RMSE
MAE
MAPE
MASE

3Volatility Analysis
To investigate changing uncertainty over time, ARCH effects were tested and GARCH models were estimated
The volatility analysis captures:
Volatility clustering
Persistence of shocks
Heteroscedasticity in residuals

# Multivariate Analysis
The long-run dynamics between rents and macroeconomic variables were analyzed through a Vector Error Correction Model (VECM).

Main procedures:
Phillips-Perron Unit Root Tests
Zivot-Andrews Structural Break Tests
Johansen Cointegration Tests
VECM Estimation
Granger Causality Analysis
Impulse Response Functions (IRF)
Forecast Error Variance Decomposition (FEVD)
Key Findings

# Univariate Results
The Holt-Winters Multiplicative model achieved the best forecasting performance and outperformed simpler exponential smoothing approaches.

Main conclusions:
Rental prices exhibit a strong long-term upward trend.
Seasonality exists but remains relatively weak.
Multiplicative seasonality captures inflationary periods more effectively.

# Multivariate Results

Evidence supports the existence of a long-run equilibrium relationship between:
Rental prices
Real wages
Mortgage interest rates
EUR/RON exchange rate

Granger Causality
The exchange rate provides significant predictive information for rental price dynamics, supporting the hypothesis that depreciation pressures are transmitted into the rental market.

Impulse Response Analysis
Macroeconomic shocks generate persistent effects on rental prices, confirming the slow adjustment process typical of housing markets.

# Technologies
R

# Conclusions 

The findings suggest that rental prices in Romania are strongly linked to broader macroeconomic conditions and that real wages and exchange rate movements play an important role in explaining long-run rental market dynamics.

The proposed framework combines forecasting performance with economic interpretability, making it useful for researchers, policymakers, investors and housing market analysts.

# Author

Alex Măroiu

Bachelor's Degree in Economic Cybernetics

Time Series Analysis Project – Bucharest University of Economic Studies (ASE)
