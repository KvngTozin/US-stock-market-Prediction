# US-stock-market-Prediction

## 📊 Project Overview  
This project is an interactive R Shiny application designed to help users analyze stock performance, explore financial trends, and forecast future stock prices using machine learning.  
It combines real-time market data, sentiment analysis, technical indicators, and predictive modeling to support smarter investment decisions.
The dataset was obtained through web scraping in R to ensure up-to-date data.

Users can:
- View live and historical price charts  
- Explore technical indicators  
- Check news sentiment for market context  
- Access company overviews  
- Generate machine learning–based future price predictions  

This app integrates data engineering, financial analytics, and machine learning into a seamless web interface.

---

## 🛠 Tools and Technologies  

### **Languages & Frameworks**
- **R**  
- **R Shiny** (UI + Server + Reactivity)

### **APIs**
- **Alpha Vantage API** – real-time & historical stock market data  
- **NewsAPI** – financial news + sentiment extraction  

### **Machine Learning**
- **XGBoost** – trained on engineered time-series features  
- **Tidyquant** – technical indicators (SMA, EMA, MACD, RSI, etc.)  
- **Feature engineering** for lagged variables & rolling statistics  

### **Data Processing & Visualization**
- **dplyr**, **tidyr** – data wrangling  
- **ggplot2**, **plotly** – dynamic charting  
- **ShinyWidgets**, **Shinydashboard** – UI components  

---

## 🎯 Objectives  
- Predict future stock prices using supervised ML (XGBoost)  
- Visualize trends through technical indicators  
- Integrate real-time stock market data  
- Analyze financial news sentiment  
- Provide a clean, intuitive dashboard for investors  
- Combine quantitative (ML + indicators) and qualitative (sentiment) insights  

---

## 🔍 Features  

### **1. Real-Time Price Analysis**
- Line charts for selected tickers  
- Historical vs. current price comparison  
- Interactive time-period filters  

### **2. Technical Indicators**
Includes:  
- Simple Moving Average (SMA)  
- Exponential Moving Average (EMA)  
- MACD  
- RSI  
- Bollinger Bands  
- Volume trends  

Displayed as layered, interactive charts.

### **3. ML-Based Stock Price Prediction**
- XGBoost regression model  
- Features include:  
  - Lag values  
  - Rolling means  
  - Volatility measures  
  - Technical indicators  
- Predicts next-day or multi-step future prices  
- Shows predicted vs. actual on dynamic plot  

### **4. Sentiment Analysis**
- Fetches recent financial news using NewsAPI  
- Performs:
  - Polarity scoring  
  - Positive/neutral/negative tagging  
- Displays headline-level sentiment  
- Gives overall market sentiment indicator  

### **5. Company Overview & Investment Notes**
For each ticker:
- Summary description  
- Market cap  
- Sector/industry  
- Fundamental insights  
- Auto-generated investment commentary based on trends + sentiment  

### **6. Clean & Interactive R Shiny Dashboard**
- Sidebar ticker selection  
- Live reactivity  
- Dynamic plots  
- ML prediction panels  
- Sentiment and overview tabs  

---

## 📈 Machine Learning Workflow  

1. **Data Extraction**  
   Fetch OHLC + volume + technical indicators.  

2. **Feature Engineering**  
   - Lag features: `lag(price, 1:10)`  
   - Rolling windows: SMA, EMA, volatility  
   - MACD & RSI  
   - Normalization  

3. **Model Training (XGBoost)**  
   - Train/test split  
   - Hyperparameter tuning  
   - Model evaluation (RMSE, MAE)  

4. **Forecasting**  
   - Predict future closing prices  
   - Plot predicted vs. actual in Shiny  

---

## 📉 Key Insights the App Can Reveal  
- Whether a stock is trending bullish or bearish  
- If technical indicators show reversal/breakout patterns  
- Sentiment alignment with market movement  
- ML model confidence & price trajectory  
- Whether fundamentals support short-term movement  

---

## 🧭 Recommendations & Use Cases  

### **1. Short-Term Traders**
Use predictions + indicators to time entries/exits.

### **2. Long-Term Investors**
Use sentiment + company overview + ML trend direction.

### **3. Students & Analysts**
Great for learning:
- Time-series forecasting  
- ML modelling  
- Technical analysis  
- Shiny dashboard building  

---

## 🖥 Dashboard  
<img width="831" height="465" alt="stock 1" src="https://github.com/user-attachments/assets/978d2c31-7fee-4504-84f4-18dbd3f16d34" />
---
<img width="780" height="348" alt="stock 2" src="https://github.com/user-attachments/assets/1f0c1196-b8e4-4b19-85e3-4006115f28ca" />

---

<img width="780" height="342" alt="stock 3" src="https://github.com/user-attachments/assets/11205230-ddc9-4e43-9aa8-de2f2e0889ec" />

---

<img width="780" height="384" alt="stock 4" src="https://github.com/user-attachments/assets/d4e7fbba-e30f-4a9e-83ae-488480c90a9e" />

---

<img width="780" height="383" alt="stock 5" src="https://github.com/user-attachments/assets/dee74e05-805c-44b9-a23e-de781d066cbb" />

---

<img width="780" height="381" alt="stock 6" src="https://github.com/user-attachments/assets/39e30c62-c90d-4c7b-97b7-15d9d2dfd2c7" />









---
