# Forecasting Romanian Rental Prices

## Executive Summary

Romania’s rental market has undergone a visible structural change. After more than a decade of moderate growth, rental prices accelerated sharply beginning in **2022**. This project combines **time-series analysis, macroeconomic modeling, and statistical forecasting** to explain the recent market dynamics and estimate how rental prices are likely to evolve over the next **24 months**.

The analysis was developed in **R** using both **univariate forecasting models** and a **multivariate VECM framework** that incorporates inflation, interest rates, wages, and the exchange rate.

---

## Data

### Source

* **Eurostat / INSSE** monthly data.

### Period analyzed

* **January 2007 – February 2026**

### Variables included

* Rental Price Index (HICP – actual rents, 2015 = 100)
* Consumer Price Index (CPI)
* National Bank policy interest rate
* Average net wage
* EUR/RON exchange rate

---

## What changed in the rental market?

The first step was to understand whether the recent increase represents a temporary fluctuation or a structural break.

![Rental price dynamics](images/price_trend.png)

### What the chart shows

* stabilization after the **2008 financial crisis**,
* a temporary slowdown during the **COVID-19 period**,
* a sharp acceleration after the **February 2022 inflation shock**,
* a new growth phase that differs materially from the previous decade.

This evidence motivated the forecasting analysis.

---

## Statistical investigation

The project included a full time-series econometric workflow:

* stationarity testing using **ADF tests**,
* lag selection for VAR models,
* Johansen cointegration testing,
* estimation of a **Vector Error Correction Model (VECM)**,
* forecast generation and model comparison.

The results indicated the existence of a **long-run relationship** between rental prices and the selected macroeconomic variables.

---

## Main forecasting model

The **Holt–Winters multiplicative model** was selected as the final operational forecasting model.

![Holt-Winters forecast](images/forecast_univariate.png)

### Interpretation

* rental prices continue to increase throughout the forecast horizon,
* no short-term stabilization is visible after 2026,
* uncertainty widens gradually as the horizon extends, while the central scenario remains upward.

---

## Macroeconomic robustness check

To verify whether the conclusion depends on the forecasting technique, a multivariate **VECM** specification was estimated.

![VECM forecast](images/forecast_multivariate.png)

The multivariate forecast confirms the same overall message: rental prices are likely to remain on an upward trajectory in the near term.

---

## What drives rental prices?

The analysis suggests that rental prices are influenced by a combination of macroeconomic factors:

* **inflation** increases housing costs,
* **wages** support rental demand,
* **interest rates** affect financing conditions,
* **EUR/RON movements** influence construction and housing-market costs.

The persistence of the recent trend indicates that the post-2022 increase is not merely a temporary spike.

---

## Key Findings

* The strongest rental price acceleration occurs **after 2022**.
* Recent market dynamics differ materially from the pre-2022 period.
* Both the univariate and multivariate models indicate **continued upward pressure on rents**.
* Rental prices appear to have entered a **higher-growth regime** rather than reverting to the previous trend.

---

## Why the results matter

The findings are relevant for:

* **real estate investors** evaluating rental income expectations,
* **financial institutions** assessing housing-market exposure,
* **public authorities** monitoring housing affordability,
* **households and businesses** planning future housing costs.

The project demonstrates how statistical forecasting can transform historical market data into forward-looking insights that support economic and financial decision-making.

---

## Analytical Workflow

1. Data collection and validation
2. Exploratory time-series analysis
3. Structural interpretation of major economic events
4. ADF stationarity testing
5. VAR lag selection
6. Johansen cointegration analysis
7. VECM estimation
8. Holt–Winters forecasting
9. Forecast comparison and interpretation
10. Executive reporting and visualization

---

## Tools & Methods

**R · Time-Series Analysis · ADF Test · VAR · VECM · Johansen Cointegration · Holt–Winters · Forecasting · Econometrics · Statistical Modeling · Data Visualization**

---

## Repository Structure

<CodeBlock language=
