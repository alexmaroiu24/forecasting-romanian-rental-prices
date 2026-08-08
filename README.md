# Forecasting Romanian Rental Prices

Forecasting rental prices in major Romanian cities using statistical time-series models and exploratory market analysis.

![Project Preview](images/hero_forecast.png)

---

## Business Question

**How can rental market data be used to estimate short-term price movements and support investment, budgeting, and housing market decisions?**

This project analyzes rental price dynamics across major Romanian cities and compares forecasting approaches to identify the most reliable model for short-term predictions.

---

## Key Results

* **12 Romanian cities analyzed**
* **Best forecasting model selected using MAE, RMSE, and MAPE**
* **Consistent upward rental trend identified in major urban markets**
* **Bucharest recorded the highest absolute rental levels**
* **City-specific dynamics had a stronger effect than a national average**

---

## Project Workflow

1. Data collection and validation
2. Data cleaning and preprocessing
3. Exploratory market analysis
4. Time-series modeling
5. Forecast generation
6. Model evaluation
7. Business interpretation

---

## Exploratory Analysis

### Rental Price Trend

![Rental Price Trend](images/price_trend.png)

The exploratory analysis revealed persistent price growth in large urban centers and substantial differences between local rental markets.

---

## Forecasting Results

### Forecasted Rental Prices

![Forecast Results](images/forecast_results.png)

Forecasts suggest continued short-term growth rather than market contraction during the analyzed period.

---

## Model Evaluation

### Forecast Accuracy Comparison

![Model Comparison](images/model_comparison.png)

| Model                 | MAE | RMSE | MAPE |
| --------------------- | --: | ---: | ---: |
| Moving Average        |     |      |      |
| Exponential Smoothing |     |      |      |
| Holt–Winters          |     |      |      |

**Final model:** Holt–Winters (lowest forecasting error).

---

## Main Insights

* Rental prices were highest in Bucharest and other major university centers.
* Smaller cities showed lower volatility and more stable forecasts.
* Forecasted prices indicated continued market growth in the short term.
* Local market conditions explained price differences better than a single national trend.

---

## Interactive Preview

![Forecast Demo](images/forecast_demo.gif)

---

## My Role

I independently:

* collected and validated the dataset;
* cleaned and transformed the data;
* performed exploratory and statistical analysis;
* built and compared forecasting models;
* evaluated forecast accuracy;
* prepared visualizations and business interpretations.

---

## Repository Structure

data/            Raw and processed datasets
scripts/         R scripts for cleaning, analysis, and forecasting
images/          Charts, screenshots, and GIF preview
report/          Project report
README.md        Project overview and results

---

## Reproducibility

1. Open the R project.
2. Run the scripts in the `scripts/` folder in numerical order.
3. The cleaned data, forecasts, and visualizations will be generated automatically.

---

## Tools & Technologies

**R · Excel · Time-Series Forecasting · Holt–Winters · Exponential Smoothing · Moving Average · Statistical Analysis · Data Visualization · Econometrics**

---

## What I Learned

The project strengthened my ability to work with real market data, evaluate competing forecasting models, investigate unexpected results, and translate statistical findings into practical business insights.

