# Stock Price Prediction with Linear Regression

This project explores the use of **Linear Regression** to predict stock prices across companies from three industries: **Technology**, **Healthcare**, and **Energy**. The study evaluates model performance using metrics like **R-squared**, **Mean Absolute Error (MAE)**, and **Mean Squared Error (MSE)**, highlighting varying predictability across industries. By analyzing feature significance, it provides insights into stock price behavior and market dynamics.

---

## Research Questions
1. **How accurately can linear regression predict daily stock prices using engineered features?**
2. **How does the performance of the linear regression model vary across the technology, healthcare, and energy industries?**
3. **Which features have the strongest predictive power for stock prices within each industry?**

---

## Key Findings
1. **Technology stocks** had the highest model performance, with **R-squared values reaching up to 0.83**, attributed to smoother growth trends. (*Details in the Results section.*)
2. **Energy sector stocks** demonstrated the lowest model performance due to the volatile nature of this sector. (*Details in the Results section.*)
3. **Rolling Standard Deviation** emerged as the feature with the strongest predictive power across all industries. (*Details in the Results section.*)

---

## Project Structure
- **Data Preparation**: Feature engineering for stock data, including moving averages, volatility measures, and returns.
- **Modeling**: Linear regression applied to predict daily stock prices.
- **Evaluation Metrics**: R-squared, MAE, and MSE.
- **Industry Analysis**: Comparison of model performance and feature importance across industries.

---

## Technologies Used
- **R Programming** for data manipulation, feature engineering, and model building.
- **GitHub** for version control and collaboration.

---

## How to Run the Code

1. **Install the latest version of R and RStudio**:
   - Download R from the [CRAN website](https://cran.r-project.org/).
   - Download and install RStudio from the [RStudio website](https://posit.co/download/rstudio-desktop/).


## How to Use
1. Clone this repository:
   ```bash
   git clone https://github.com/your-username/stock-price-prediction.git

