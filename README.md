# Forecasting Rental Prices in Romania: A Time Series and Econometric Approach
Time series forecasting and econometric analysis of the Romanian rental market using ARIMA, GARCH, Holt-Winters and VECM models.

# Project Overview

Housing affordability has become one of the most significant economic challenges across Europe. Rising rental prices, inflationary pressures, and limited housing supply have increased the financial burden on households and transformed rental markets into an important source of economic vulnerability.

This project investigates the dynamics of rental prices in Romania between May 2007 and February 2026 using both univariate and multivariate time series techniques.

The analysis combines forecasting methods and econometric modeling to understand the determinants of rental prices, evaluate predictive performance, and estimate the long-run effects of macroeconomic shocks on the Romanian housing market.

# Research Questions

This study addresses five key questions:

1. Which forecasting model provides the highest predictive accuracy for rental prices in Romania?
2. Is there a long-run equilibrium relationship between rental prices, real wages, mortgage interest rates, and the EUR/RON exchange rate?
3. Which macroeconomic variable contains the strongest predictive information for rental price dynamics?
4. How quickly does the rental market return to equilibrium after macroeconomic shocks?
5. What is the expected evolution of rental prices during 2026–2028?

# Dataset

Period
May 2007 – February 2026

Frequency
Monthly observations

Data Sources
Eurostat
National Bank of Romania (BNR)
European Central Bank (ECB)

# Variables
Univariate Analysis
HICP Rental Index (Actual Rentals for Housing)

Multivariate Analysis
- HICP Rental Index (Actual Rentals for Housing)
- Real Wage Index
- Mortgage Interest Rate (APRC)
- EUR/RON Exchange Rate

# Methodology
Univariate Time Series Analysis
The Box-Jenkins framework was applied to identify the most accurate forecasting model.

Main steps:
Exploratory Data Analysis
Stationarity Testing
Training/Test Split
SARIMA Model Selection
Exponential Smoothing Models
Forecast Evaluation

Models evaluated
SARIMA with Drift
Holt Linear Trend
Holt-Winters Additive
Holt-Winters Multiplicative


# Volatility Analysis

To investigate changing uncertainty over time, ARCH effects were tested and GARCH models were estimated
The volatility analysis captures:
Volatility clustering
Persistence of shocks
Heteroscedasticity in residuals

# Multivariate Analysis

The long-run dynamics between rents and macroeconomic variables were analyzed through a Vector Error Correction Model (VECM).

Methods applied:

Phillips-Perron Unit Root Tests
Zivot-Andrews Structural Break Tests
Johansen Cointegration Tests
VECM Estimation
Granger Causality Analysis
Impulse Response Functions (IRF)
Forecast Error Variance Decomposition (FEVD)

# Main Findings
Forecasting Performance

Four forecasting approaches were evaluated on an independent test set covering the inflationary period between January 2023 and February 2026.

The Holt-Winters Multiplicative model delivered the highest forecasting accuracy, achieving:

RMSE = 3.83
MAE = 3.37
MAPE = 2.26%

The model successfully captured the nonlinear growth pattern observed during the recent inflationary cycle and outperformed alternative exponential smoothing specifications. Statistical comparison using the Diebold-Mariano test confirmed a significant improvement over simpler trend-based models, while demonstrating predictive performance comparable to the selected SARIMA benchmark.

Rental Market Volatility

Volatility diagnostics revealed significant ARCH effects and volatility clustering in rental price dynamics.

The estimated GARCH model showed that shocks affecting the rental market tend to persist over time rather than dissipate immediately. Periods of elevated uncertainty were associated with major macroeconomic events, including:

- the 2008–2009 Global Financial Crisis;
- the COVID-19 pandemic;
- the inflationary shock of 2022–2024.

These findings suggest that rental markets exhibit not only long-term price trends but also changing levels of uncertainty that must be considered when constructing forecasts.

Long-Run Equilibrium Relationships

Johansen cointegration testing identified a statistically significant long-run equilibrium relationship between:Rental Prices (HICP Rent), Real Wages, Mortgage Interest Rates (APRC), EUR/RON Exchange Rate

The results indicate that rental prices do not evolve independently but are closely linked to broader macroeconomic conditions.

Granger Causality and Economic Drivers

The analysis provides evidence that the EUR/RON exchange rate contains valuable predictive information for future rental price movements.

This result supports the hypothesis that depreciation of the national currency is partially transmitted to the rental market through the widespread practice of indexing rents to euro-denominated values.

Real wages were identified as a key structural determinant of rental affordability, while mortgage financing costs influence the allocation of housing demand between ownership and renting.

Adjustment to Economic Shocks

Impulse Response Functions (IRF) and VECM estimation revealed that the Romanian rental market adjusts gradually following macroeconomic shocks.

The estimated error-correction mechanism confirms that deviations from the long-run equilibrium are corrected slowly, reflecting the rigid nature of housing markets, long contractual arrangements, and delayed price adjustments.

Forecast Outlook (2026–2028)

The final forecasting model projects a continued upward trajectory of rental prices over the next two years.

Although inflation is expected to normalize compared to the 2022–2024 period, rental prices remain on a positive long-run trend driven by:

- increasing household incomes;
- persistent housing supply constraints;
- exchange rate dynamics;
- structural demand pressures in urban areas.

The forecasts suggest that housing affordability will remain a major economic challenge in Romania over the medium term.

# Conclusions

This project demonstrates how advanced time series and econometric techniques can be combined to analyze both the short-run dynamics and long-run determinants of rental prices.

The results show that:

Holt-Winters Multiplicative provides the most accurate forecasting performance;
rental prices exhibit significant volatility persistence;
a stable long-run equilibrium relationship exists between rents and key macroeconomic variables;
exchange rate movements contain important predictive information for future rental price changes;
the Romanian rental market responds slowly to macroeconomic disturbances.

From a policy perspective, the findings highlight the importance of wage growth, monetary conditions, and exchange rate stability in shaping housing affordability. The proposed framework can support evidence-based decision making for policymakers, investors, financial institutions, and housing market analysts.

# Skills Demonstrated
# Data Analytics & Statistical Modeling
Time Series Analysis
Forecasting and Predictive Analytics
Econometric Modeling
Statistical Testing and Model Validation
Volatility Modeling (ARCH/GARCH)
Cointegration Analysis
Vector Error Correction Models (VECM)
Granger Causality Analysis
Forecast Accuracy Evaluation

# Data Science & Programming
R Programming
Data Cleaning and Transformation
Data Visualization
Exploratory Data Analysis (EDA)

# Business & Economic Analysis
Macroeconomic Analysis
Housing Market Analytics
Financial Data Analysis
Economic Forecasting
Policy-Oriented Research
Market Trend Analysis

# Communication & Business Storytelling
Data Storytelling
Insight Generation
Translating Complex Statistical Results into Business Insights
Executive Reporting

# Problem Solving
Hypothesis Testing
Analytical Thinking
Quantitative Research
Model Selection and Comparison
Evidence-Based Decision Making

