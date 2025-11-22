# Main code
# Load libraries
library(shiny)
library(shinythemes)
library(quantmod)
library(DT)
library(httr)
library(jsonlite)
library(plotly)
library(xgboost)
library(sentimentr)
library(dplyr)
library(lubridate)
library(memoise)

# Auth libs
library(DBI)
library(RSQLite)
library(digest)

# ---------- CONFIG ----------
DB_PATH <- "users.db"          # SQLite file
APP_SALT_LEN <- 16             # length of per-user salt
THEME_NAME <- "flatly"

# ---------- DB HELPERS ----------
ensure_users_table <- function() {
  con <- dbConnect(SQLite(), DB_PATH)
  on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      username TEXT PRIMARY KEY,
      password_hash TEXT NOT NULL,
      salt TEXT NOT NULL,
      created_at TEXT NOT NULL
    );
  ")
}

user_exists <- function(username) {
  con <- dbConnect(SQLite(), DB_PATH)
  on.exit(dbDisconnect(con), add = TRUE)
  res <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM users WHERE username = ?", params = list(username))
  res$n[1] > 0
}

random_salt <- function(n = APP_SALT_LEN) paste(sample(c(0:9, letters, LETTERS), n, TRUE), collapse = "")

hash_password <- function(password, salt) digest(paste0(password, salt), algo = "sha256")

create_user <- function(username, password) {
  if (is.na(username) || username == "" || is.na(password) || password == "") {
    return(list(ok = FALSE, msg = "Username and password are required."))
  }
  if (user_exists(username)) return(list(ok = FALSE, msg = "Username already taken."))
  salt <- random_salt()
  phash <- hash_password(password, salt)
  con <- dbConnect(SQLite(), DB_PATH)
  on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con,
            "INSERT INTO users(username, password_hash, salt, created_at) VALUES(?,?,?,datetime('now'))",
            params = list(username, phash, salt))
  list(ok = TRUE, msg = "Account created successfully. Please log in.")
}

check_login <- function(username, password) {
  con <- dbConnect(SQLite(), DB_PATH)
  on.exit(dbDisconnect(con), add = TRUE)
  u <- dbGetQuery(con, "SELECT username, password_hash, salt FROM users WHERE username = ?", params = list(username))
  if (nrow(u) == 0) return(FALSE)
  expected <- u$password_hash[1]
  salt <- u$salt[1]
  hash_password(password, salt) == expected
}

# Ensure DB ready
ensure_users_table()

# ---------- APP CONFIG / FUNCTIONS ----------
# API Keys (required for live company overview + news sentiment)
api_key <- "insert your api key"
news_api_key <- "insert your api key"

# Predefined stock symbols
stock_symbols <- c("AAPL", "GOOGL", "AMZN", "MSFT", "TSLA", "NFLX")

# ---------- UTIL: DATA SANITIZERS (NEW) ----------
to_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.factor(x))  return(suppressWarnings(as.numeric(as.character(x))))
  if (is.character(x)) return(suppressWarnings(as.numeric(gsub(",", "", x))))
  if (is.integer(x)) return(as.numeric(x))
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(x))
}

clean_train_df <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  names(df) <- tolower(gsub("\\s+", "", names(df)))
  for (nm in intersect(c("close","lag1","lag2","lag3","sentiment"), names(df))) {
    df[[nm]] <- to_num(df[[nm]])
  }
  if ("date" %in% names(df)) {
    if (inherits(df$date, "Date")) {
      # ok
    } else if (inherits(df$date, "POSIXct") || inherits(df$date, "POSIXt")) {
      df$date <- as.Date(df$date)
    } else if (is.numeric(df$date)) {
      if (all(df$date > 10^7, na.rm = TRUE)) {
        df$date <- as.Date(as.POSIXct(df$date, origin = "1970-01-01", tz = "UTC"))
      } else {
        df$date <- as.Date(df$date, origin = "1970-01-01")
      }
    } else {
      df$date <- suppressWarnings(as.Date(df$date))
      if (anyNA(df$date)) df$date <- suppressWarnings(lubridate::ymd(df$date))
    }
  }
  df
}

clean_newdata <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  df[] <- lapply(df, to_num)
  keep <- vapply(df, function(col) any(is.finite(col)), logical(1))
  as.data.frame(df[, keep, drop = FALSE])
}

# -------- NEW: CACHE LOADING (load-only, no training) --------
CACHE_DIR <- "cache"

path_try_both <- function(fname) {
  cand <- c(file.path(CACHE_DIR, paste0(fname, ".rds")),
            file.path(CACHE_DIR, fname))
  existing <- cand[file.exists(cand)]
  if (length(existing)) existing[1] else NA_character_
}

cache_model_path <- function(sym) path_try_both(paste0(sym, "_xgb_model"))
cache_train_path <- function(sym) path_try_both(paste0(sym, "_training_data"))

CACHED_MODELS <- new.env(parent = emptyenv())
CACHED_TRAIN  <- new.env(parent = emptyenv())

for (sym in stock_symbols) {
  mp <- cache_model_path(sym)
  tp <- cache_train_path(sym)
  if (!is.na(mp) && file.exists(mp)) {
    CACHED_MODELS[[sym]] <- tryCatch(readRDS(mp), error = function(e) NULL)
  } else {
    CACHED_MODELS[[sym]] <- NULL
  }
  if (!is.na(tp) && file.exists(tp)) {
    CACHED_TRAIN[[sym]]  <- tryCatch(clean_train_df(readRDS(tp)), error = function(e) NULL)
  } else {
    CACHED_TRAIN[[sym]] <- NULL
  }
}

load_cached_bundle <- function(symbol) {
  list(
    model    = CACHED_MODELS[[symbol]],
    train_df = CACHED_TRAIN[[symbol]]
  )
}

# ------------- API/memoised helpers -------------
get_financial_data <- function(symbol) {
  url <- paste0("https://www.alphavantage.co/query?function=OVERVIEW&symbol=", symbol, "&apikey=", api_key)
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content))
    return(content)
  } else {
    return(NULL)
  }
}
mem_get_financial_data <- memoise(get_financial_data)

get_news_data <- function(symbol) {
  from_date <- as.character(Sys.Date() - 30)
  url <- paste0(
    "https://newsapi.org/v2/everything?q=", symbol,
    "&language=en&sortBy=publishedAt&pageSize=50&from=", from_date,
    "&apiKey=", news_api_key
  )
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content))
    return(content$articles)
  } else {
    return(NULL)
  }
}
mem_get_news_data <- memoise(get_news_data)

perform_sentiment_analysis <- function(news_data) {
  if (is.null(news_data) || length(news_data) == 0) return(NULL)
  df <- data.frame(
    title = unlist(news_data$title),
    description = unlist(news_data$description),
    publishedAt = ymd_hms(unlist(news_data$publishedAt)),
    stringsAsFactors = FALSE
  )
  df <- df %>% distinct(title, .keep_all = TRUE)
  df$text <- paste(df$title, df$description)
  df$sentiment <- sentiment_by(df$text)$ave_sentiment
  df$month <- floor_date(df$publishedAt, "month")
  df %>% group_by(month) %>% summarise(mean_sentiment = mean(sentiment, na.rm = TRUE), .groups = "drop") %>% arrange(month)
}

# EMAs
calculate_emas <- function(data) {
  data$EMA_50 <- EMA(Cl(data), n = 50)
  data$EMA_100 <- EMA(Cl(data), n = 100)
  data$EMA_200 <- EMA(Cl(data), n = 200)
  data
}

# Yahoo data (2 years) + memoised
fetch_yahoo <- function(symbol) {
  getSymbols(symbol, src = "yahoo",
             from = Sys.Date() - 730,
             to   = Sys.Date(),
             auto.assign = FALSE)
}
mem_fetch_yahoo <- memoise(fetch_yahoo)

# ---------- Forecast helper (matches cached feature names) ----------
forecast_next_year <- function(model, train_df) {
  if (is.null(model) || is.null(train_df) || nrow(train_df) == 0) return(rep(NA_real_, 365))
  train_df <- clean_train_df(train_df)
  last_close <- suppressWarnings(to_num(tail(train_df$close, 1)))
  last_sent  <- suppressWarnings(to_num(tail(train_df$sentiment, 1)))
  if (length(last_close) == 0 || is.na(last_close)) last_close <- NA_real_
  if (length(last_sent)  == 0 || is.na(last_sent))  last_sent  <- 0
  future_state <- data.frame(
    lag1 = last_close,
    lag2 = last_close,
    lag3 = last_close,
    sentiment = last_sent
  )
  preds <- numeric(365)
  for (i in seq_len(365)) {
    nd <- clean_newdata(future_state)
    needed <- c("lag1","lag2","lag3","sentiment")
    for (nm in setdiff(needed, names(nd))) nd[[nm]] <- 0
    nd <- nd[, needed, drop = FALSE]
    preds[i] <- as.numeric(predict(model, as.matrix(nd)))
    future_state$lag3 <- future_state$lag2
    future_state$lag2 <- future_state$lag1
    future_state$lag1 <- preds[i]
  }
  set.seed(42)
  preds + rnorm(365, 0, 2)
}

# Enhanced Investment Advice
generate_investment_advice <- function(fin_data, sentiment_score) {
  if (is.null(fin_data) || length(fin_data) == 0) {
    return("<b>No financial data available to generate investment advice.</b>")
  }
  name <- fin_data$Name
  sector <- fin_data$Sector
  industry <- fin_data$Industry
  pe_ratio <- as.numeric(fin_data$PERatio)
  dividend_yield <- as.numeric(fin_data$DividendYield)
  eps <- as.numeric(fin_data$EPS)
  market_cap <- as.numeric(fin_data$MarketCapitalization)
  market_cap_label <- if (!is.na(market_cap)) {
    if (market_cap >= 1e12) paste0(round(market_cap / 1e12, 2), " Trillion USD")
    else if (market_cap >= 1e9) paste0(round(market_cap / 1e9, 2), " Billion USD")
    else if (market_cap >= 1e6) paste0(round(market_cap / 1e6, 2), " Million USD")
    else paste0(round(market_cap, 2), " USD")
  } else { "N/A" }
  sentiment_msg <- if (!is.na(sentiment_score)) {
    if (sentiment_score > 0.3) "<span style='color:green;'>✅ Strong positive sentiment suggests optimism in the market.</span>"
    else if (sentiment_score < -0.3) "<span style='color:red;'>❌ Negative sentiment may indicate caution or lack of investor confidence.</span>"
    else "<span style='color:orange;'>⚠️ Neutral sentiment – the market appears uncertain or divided.</span>"
  } else { "<span style='color:gray;'>Sentiment data not available</span>" }
  valuation_msg <- if (!is.na(pe_ratio)) {
    if (pe_ratio < 15) "✅ The stock appears undervalued relative to earnings – possibly a good entry point."
    else if (pe_ratio > 25) "❌ The P/E ratio is quite high, suggesting overvaluation or future growth expectations already priced in."
    else "⚖️ The stock is fairly valued based on historical P/E norms."
  } else { "P/E ratio data is not available." }
  dividend_msg <- if (!is.na(dividend_yield)) {
    if (dividend_yield > 0.03) "✅ Offers a strong dividend yield, which may appeal to income-focused investors."
    else if (dividend_yield > 0) "ℹ️ Provides a modest dividend – consider if consistent payouts are a priority."
    else "❌ No dividends offered – focus is likely on reinvestment or growth rather than income."
  } else { "Dividend data not available." }
  eps_msg <- if (!is.na(eps)) {
    if (eps > 2) "✅ Healthy earnings per share – indicates solid profitability and financial stability."
    else if (eps > 0) "ℹ️ Positive but modest earnings – company may be growing or reinvesting heavily."
    else "❌ Negative EPS – the company is currently unprofitable, which increases investment risk."
  } else { "EPS data not available." }
  decision <- ""
  if (!is.na(sentiment_score) && sentiment_score > 0.3 &&
      !is.na(pe_ratio) && pe_ratio < 20 &&
      !is.na(eps) && eps > 2) {
    decision <- "<span style='color:green; font-weight:bold;'>✔️ Final Verdict: This stock presents a strong opportunity for investment based on current fundamentals and positive market sentiment. Consider buying or increasing position.</span>"
  } else if (!is.na(sentiment_score) && sentiment_score < -0.3 || (!is.na(eps) && eps < 0)) {
    decision <- "<span style='color:red; font-weight:bold;'>⚠️ Final Verdict: Investment not advisable at this time. Consider holding off due to risk signals in sentiment and/or profitability.</span>"
  } else {
    decision <- "<span style='color:orange; font-weight:bold;'>➖ Final Verdict: Hold and monitor. The company shows mixed signals – further analysis or time may be needed before making a confident move.</span>"
  }
  paste0(
    "<div style='font-size: 16px; line-height: 1.6;'>",
    "<h4><b>", name, "</b></h4>",
    "<b>Sector:</b> ", sector, " | <b>Industry:</b> ", industry, "<br>",
    "<b>Market Capitalization:</b> ", market_cap_label, "<br><br>",
    "<h5><b>📊 Sentiment Analysis:</b></h5>", sentiment_msg, "<br><br>",
    "<h5><b>📈 Valuation (P/E Ratio):</b></h5>", valuation_msg, "<br><br>",
    "<h5><b>💰 Dividend Profile:</b></h5>", dividend_msg, "<br><br>",
    "<h5><b>💹 Earnings Strength (EPS):</b></h5>", eps_msg, "<br><br>",
    "<h4><b>🧠 Investment Advice:</b></h4>", decision,
    "</div>"
  )
}

# ---------- UI (Auth Router + Main App UI) ----------
auth_ui <- fluidPage(
  theme = shinytheme(THEME_NAME),
  tags$head(tags$style(HTML("
    .auth-card { max-width: 520px; margin: 40px auto; padding: 28px; border-radius: 16px;
                 box-shadow: 0 10px 24px rgba(0,0,0,0.08); background: #fff; }
    .center { text-align: center; }
  "))),
  titlePanel("Welcome"),
  div(class = "auth-card",
      tabsetPanel(id = "auth_tabs", type = "tabs",
                  tabPanel("Sign Up",
                           br(),
                           textInput("su_username", "Username"),
                           passwordInput("su_password", "Password"),
                           passwordInput("su_password2", "Confirm Password"),
                           actionButton("btn_signup", "Create Account", class = "btn-primary"),
                           br(), br(),
                           textOutput("su_msg")
                  ),
                  tabPanel("Login",
                           br(),
                           textInput("login_username", "Username"),
                           passwordInput("login_password", "Password"),
                           actionButton("btn_login", "Login", class = "btn-success"),
                           br(), br(),
                           textOutput("login_msg")
                  )
      ),
      br(),
      div(class = "center text-muted", "Already have an account? Use the Login tab.")
  )
)

main_ui <- fluidPage(
  theme = shinytheme("flatly"),
  # --- Spinner CSS (used inside modal) ---
  tags$head(tags$style(HTML("
    .logout-topright {
      position: fixed; top: 12px; right: 16px; z-index: 9999;
    }
    .logout-topright .btn { padding: 6px 10px !important; font-size: 12px !important; border-radius: 6px !important; }
    .lds-ring { display: inline-block; position: relative; width: 64px; height: 64px; }
    .lds-ring div {
      box-sizing: border-box; display: block; position: absolute; width: 51px; height: 51px; margin: 6px;
      border: 6px solid #2c3e50; border-radius: 50%; animation: lds-ring 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite;
      border-color: #2c3e50 transparent transparent transparent;
    }
    .lds-ring div:nth-child(1) { animation-delay: -0.45s; }
    .lds-ring div:nth-child(2) { animation-delay: -0.3s; }
    .lds-ring div:nth-child(3) { animation-delay: -0.15s; }
    @keyframes lds-ring { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg);} }
  "))),
  div(class = "logout-topright",
      actionButton("btn_logout", "Logout", class = "btn btn-sm btn-danger")
  ),
  titlePanel("Stock Market Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stock", "Select Stock Symbol:", choices = stock_symbols, selected = "AAPL"),
      actionButton("update", "Update", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Stock Plot", plotlyOutput("stockPlot")),
        tabPanel("Dynamic Plot", plotlyOutput("dynamicPlot")),
        tabPanel("Company Overview",
                 fluidRow(
                   column(12, h4("Company Overview"), uiOutput("companyOverview")),
                   column(12, h4("Key Metrics"), dataTableOutput("companyMetrics"))
                 )),
        tabPanel("Investment Advice",
                 h4("Investment Advice"),
                 uiOutput("advice"),
                 br(),
                 plotlyOutput("sentimentTrend")
        ),
        tabPanel("Stock Price Prediction",
                 h4("Predicted Stock Prices for Next Year"),
                 div(
                   style = "margin: 8px 0 16px 0;",
                   actionButton("run_forecast", "Run Forecast", class = "btn btn-primary")
                 ),
                 plotlyOutput("predictionPlot"),
                 br(),
                 downloadButton("downloadPrediction", "Download Predicted Data")
        ),
        tabPanel("About",
                 div(style = "padding: 20px; font-size: 16px; line-height: 1.7;",
                     h3("📘 About This App"),
                     p("The Stock Market Prediction app is an intelligent, data-driven R Shiny platform built to assist investors, traders, and analysts in making smarter investment decisions."),
                     tags$hr(),
                     h4("🔍 Key Features:"),
                     tags$ul(
                       tags$li(HTML("<b>Real-time Stock Data:</b> Fetches up-to-date stock prices and technical indicators directly from Yahoo Finance.")),
                       tags$li(HTML("<b>Company Overview & Financials:</b> Presents structured, in-depth profiles of companies including key metrics, earnings, dividends, margins, and more.")),
                       tags$li(HTML("<b>Sentiment Analysis:</b> Analyzes recent news articles using NLP to gauge investor sentiment toward the selected stock.")),
                       tags$li(HTML("<b>Technical Analysis:</b> Visualizes Exponential Moving Averages (EMA 50/100/200) to help identify market trends and entry/exit points.")),
                       tags$li(HTML("<b>Machine Learning Predictions:</b> Leverages XGBoost to forecast stock prices based on historical data and sentiment inputs.")),
                       tags$li(HTML("<b>Download Predictions:</b> Users can download forecasted stock data for further analysis or offline use."))
                     ),
                     tags$hr(),
                     h4("💡 Technology Stack:"),
                     tags$ul(
                       tags$li("R + Shiny for interactive UI"),
                       tags$li("XGBoost for predictive modeling"),
                       tags$li("sentimentr for NLP-based sentiment scoring"),
                       tags$li("plotly for rich interactive charts"),
                       tags$li("Yahoo Finance & NewsAPI for live data feeds")
                     ),
                     tags$hr(),
                     h4("👨‍💻 Developer's Note:"),
                     p("This app is designed as a flexible research and decision-support tool. While the insights are powerful, they should complement—not replace—your own research and risk assessments."),
                     br(),
                     div(style = "color: gray; font-size: 13px;",
                         HTML("All financial data is for informational purposes only and not intended as financial advice.")
                     )
                 )
        )
      )
    )
  )
)

# Router UI — shows auth until logged in
ui <- uiOutput("router")

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  # Global auth state
  auth <- reactiveValues(authenticated = FALSE, username = NULL)
  
  # Render either auth UI or main app UI
  output$router <- renderUI({
    if (isTRUE(auth$authenticated)) {
      main_ui
    } else {
      auth_ui
    }
  })
  
  # --- SIGN UP ---
  observeEvent(input$btn_signup, {
    req(input$su_username, input$su_password, input$su_password2)
    if (nchar(input$su_username) < 3) {
      output$su_msg <- renderText("Username must be at least 3 characters.")
      return()
    }
    if (nchar(input$su_password) < 6) {
      output$su_msg <- renderText("Password must be at least 6 characters.")
      return()
    }
    if (!identical(input$su_password, input$su_password2)) {
      output$su_msg <- renderText("Passwords do not match.")
      return()
    }
    res <- create_user(input$su_username, input$su_password)
    output$su_msg <- renderText(res$msg)
    if (isTRUE(res$ok)) {
      updateTabsetPanel(session, "auth_tabs", selected = "Login")
      showNotification("Account created. Please log in.", type = "message")
    } else {
      showNotification(res$msg, type = "error")
    }
  })
  
  # --- LOGIN ---
  observeEvent(input$btn_login, {
    req(input$login_username, input$login_password)
    ok <- check_login(input$login_username, input$login_password)
    if (ok) {
      auth$authenticated <- TRUE
      auth$username <- input$login_username
      output$login_msg <- renderText("")
      showNotification(paste("Welcome,", auth$username, "🎉"), type = "message")
    } else {
      output$login_msg <- renderText("Invalid username or password.")
      showNotification("Invalid username or password.", type = "error")
    }
  })
  
  # --- LOGOUT ---
  observeEvent(input$btn_logout, {
    auth$authenticated <- FALSE
    auth$username <- NULL
    showNotification("You have been logged out.", type = "message")
  })
  
  # --- MAIN APP SERVER: Register AFTER successful login only ---
  observeEvent(auth$authenticated, {
    req(auth$authenticated)
    
    # For prediction results
    pred_df <- reactiveVal(NULL)
    
    # 0) INSTANT cache bundle for the selected symbol (no training, no API)
    cachedBundle <- reactive({
      b <- load_cached_bundle(input$stock)
      if (is.null(b$model) || is.null(b$train_df)) {
        showNotification(
          paste0("No cached files found for ", input$stock, ". Place ", input$stock, "_xgb_model and ", input$stock, "_training_data in /cache."),
          type = "error", duration = 6
        )
      } else {
        b$train_df <- clean_train_df(b$train_df)
      }
      b
    })
    
    # 1) Yahoo data (optional — for charts when user clicks Update)
    stockData <- eventReactive(input$update, {
      withProgress(message = paste("Fetching", input$stock, "prices..."), value = 0.2, {
        isolate({ mem_fetch_yahoo(input$stock) })
      })
    })
    
    # 2) Company overview (optional)
    financialData <- reactive({
      withProgress(message = "Loading company overview...", value = 0.1, {
        mem_get_financial_data(input$stock)
      })
    })
    
    # 3) Live news & sentiment (optional for display when clicking Update)
    newsData <- eventReactive(input$update, {
      withProgress(message = "Fetching latest news...", value = 0.2, {
        mem_get_news_data(input$stock)
      })
    })
    
    sentimentData <- reactive({
      nd <- newsData()
      req(nd)
      withProgress(message = "Analyzing sentiment...", value = 0.4, {
        perform_sentiment_analysis(nd)
      })
    })
    
    # ---- INSTANT sentiment from cache (used for advice + prediction) ----
    cachedSentimentMonthly <- reactive({
      b <- cachedBundle()
      req(b$train_df)
      if (!("date" %in% names(b$train_df)) || !("sentiment" %in% names(b$train_df))) return(NULL)
      b$train_df %>%
        mutate(month = floor_date(as.Date(date), "month"),
               sentiment = to_num(sentiment)) %>%
        group_by(month) %>%
        summarise(mean_sentiment = mean(sentiment, na.rm = TRUE), .groups = "drop") %>%
        arrange(month)
    })
    
    cachedSentimentScore <- reactive({
      sm <- cachedSentimentMonthly()
      if (is.null(sm)) return(NA_real_)
      mean(sm$mean_sentiment, na.rm = TRUE)
    })
    
    # 4) Historic Plot (from Yahoo only when fetched)
    output$stockPlot <- renderPlotly({
      req(stockData())
      data <- stockData()
      ohlc_data <- data.frame(
        Date = index(data),
        Open = as.numeric(Op(data)),
        High = as.numeric(Hi(data)),
        Low  = as.numeric(Lo(data)),
        Close= as.numeric(Cl(data))
      )
      plot_ly(ohlc_data, x = ~Date) %>%
        add_lines(y = ~Close, name = 'Close', line = list(color = 'black', width = 2)) %>%
        add_lines(y = ~Open,  name = 'Open',  line = list(color = 'green', width = 2)) %>%
        add_lines(y = ~High,  name = 'High',  line = list(color = 'purple', width = 2)) %>%
        add_lines(y = ~Low,   name = 'Low',   line = list(color = 'orange', width = 2)) %>%
        layout(
          title = paste("Historic Chart for", input$stock),
          xaxis = list(title = "Date", rangeslider = list(visible = TRUE), type = "date"),
          yaxis = list(title = "Price ($)", tickprefix = "$"),
          hovermode = "x unified",
          legend = list(orientation = 'h', x = 0.1, y = -0.3)
        )
    })
    
    # 5) EMAs Plot (from Yahoo only when fetched)
    output$dynamicPlot <- renderPlotly({
      req(stockData())
      data <- calculate_emas(stockData())
      df <- data.frame(
        Date   = index(data),
        Close  = as.numeric(Cl(data)),
        EMA_50 = as.numeric(data$EMA_50),
        EMA_100= as.numeric(data$EMA_100),
        EMA_200= as.numeric(data$EMA_200)
      )
      plot_ly(data = df, x = ~Date) %>%
        add_lines(y = ~Close,  name = 'Price',   line = list(color = 'blue', width = 2)) %>%
        add_lines(y = ~EMA_50, name = 'EMA 50',  line = list(color = 'green', dash = 'dash')) %>%
        add_lines(y = ~EMA_100,name = 'EMA 100', line = list(color = 'orange', dash = 'dot')) %>%
        add_lines(y = ~EMA_200,name = 'EMA 200', line = list(color = 'red', dash = 'dashdot')) %>%
        layout(
          title = list(text = paste("Technical Analysis for", input$stock), font = list(size = 20), x = 0.5),
          xaxis = list(title = "Date", rangeslider = list(visible = TRUE), type = "date"),
          yaxis = list(title = "Price ($)", tickprefix = "$"),
          hovermode = "x unified",
          legend = list(orientation = 'h', x = 0.1, y = -0.3)
        )
    })
    
    # ---- NEW: Run forecast on demand with a blocking modal + spinner ----
    observeEvent(input$run_forecast, {
      b <- cachedBundle()
      if (is.null(b$model) || is.null(b$train_df) || nrow(b$train_df) == 0) {
        showNotification("Cannot run forecast: missing cached model or training data for this symbol.", type = "error")
        return(NULL)
      }
      
      showModal(modalDialog(
        easyClose = FALSE, footer = NULL,
        title = paste("Running forecast for", input$stock),
        div(style = "display:flex; align-items:center; gap:16px;",
            div(class = "lds-ring", div(), div(), div(), div()),
            div(
              HTML("<b>Please wait for 6 minutes...</b><br/>Generating the next 365 days of predicted prices.")
            )
        )
      ))
      
      # Do the work
      tryCatch({
        preds <- forecast_next_year(b$model, b$train_df)
        last_date <- NA
        if ("date" %in% names(b$train_df)) {
          last_date <- suppressWarnings(max(as.Date(b$train_df$date), na.rm = TRUE))
        }
        if (is.infinite(last_date) || is.na(last_date)) last_date <- Sys.Date()
        future_dates <- seq(from = last_date + 1, by = "day", length.out = 365)
        df <- data.frame(Date = future_dates, Predicted = preds, check.names = FALSE)
        pred_df(df)
        showNotification("Forecast complete ✅", type = "message")
      }, error = function(e) {
        showNotification(paste("Forecast failed:", e$message), type = "error", duration = 8)
      }, finally = {
        removeModal()
      })
    })
    
    output$predictionPlot <- renderPlotly({
      df <- pred_df()
      req(!is.null(df), nrow(df) > 0)
      plot_ly(df, x = ~Date) %>%
        add_lines(y = ~Predicted, name = 'Predicted', line = list(color = 'blue', width = 1.3)) %>%
        layout(
          title = paste("Predicted Stock Prices for", input$stock),
          xaxis = list(title = "Date", rangeslider = list(visible = TRUE), type = "date"),
          yaxis = list(title = "Price ($)", tickprefix = "$"),
          hovermode = "x unified",
          legend = list(orientation = 'h', x = 0.5, y = -0.8)
        )
    })
    
    output$downloadPrediction <- downloadHandler(
      filename = function() paste0(input$stock, "_predicted_prices.csv"),
      content = function(file) {
        df <- pred_df()
        if (is.null(df) || !nrow(df)) {
          write.csv(data.frame(Info = "No forecast has been run yet."), file, row.names = FALSE)
        } else {
          write.csv(dplyr::rename(df, Predicted_Price = Predicted), file, row.names = FALSE)
        }
      }
    )
    
    # 7) Company Overview UI
    output$companyOverview <- renderUI({
      fin <- financialData()
      req(fin)
      if (length(fin) == 0 || is.null(fin$Name)) {
        return(HTML("<div style='font-size: 16px; color: gray;'>No overview available for this company.</div>"))
      }
      safe_value <- function(x) {
        if (is.null(x) || x == "" || is.na(x)) return("No data available")
        x
      }
      format_big_number <- function(x) {
        x <- as.numeric(x)
        if (is.na(x)) return("No data available")
        if (x >= 1e12) paste0(round(x / 1e12, 2), " Trillion USD")
        else if (x >= 1e9) paste0(round(x / 1e9, 2), " Billion USD")
        else if (x >= 1e6) paste0(round(x / 1e6, 2), " Million USD")
        else paste0(format(x, big.mark = ","), " USD")
      }
      overview_html <- paste0(
        "<div style='font-size: 16px; line-height: 1.7;'>",
        "<h4><b>🏢 ", safe_value(fin$Name), "</b></h4>",
        "<b>Sector:</b> ", safe_value(fin$Sector), "<br>",
        "<b>Industry:</b> ", safe_value(fin$Industry), "<br>",
        "<b>Exchange:</b> ", safe_value(fin$Exchange), "<br>",
        "<b>Country:</b> ", safe_value(fin$Country), "<br>",
        "<b>Market Capitalization:</b> ", format_big_number(fin$MarketCapitalization), "<br>",
        "<b>Currency:</b> ", safe_value(fin$Currency), "<br><br>",
        "<h5><b>📄 Description:</b></h5>",
        "<div style='text-align: justify;'>", safe_value(fin$Description), "</div><br>",
        "<h5><b>📊 Financial Highlights:</b></h5>",
        "<ul>",
        "<li><b>Book Value:</b> ", safe_value(fin$BookValue), "</li>",
        "<li><b>EPS (Earnings per Share):</b> ", safe_value(fin$EPS), "</li>",
        "<li><b>P/E Ratio:</b> ", safe_value(fin$PERatio), "</li>",
        "<li><b>Dividend per Share:</b> ", safe_value(fin$DividendPerShare), "</li>",
        "<li><b>Dividend Yield:</b> ", safe_value(fin$DividendYield), "</li>",
        "<li><b>Revenue (TTM):</b> ", format_big_number(fin$RevenueTTM), "</li>",
        "<li><b>Gross Profit (TTM):</b> ", format_big_number(fin$GrossProfitTTM), "</li>",
        "<li><b>Return on Equity (ROE):</b> ", safe_value(fin$ReturnOnEquityTTM), "</li>",
        "<li><b>Return on Assets (ROA):</b> ", safe_value(fin$ReturnOnAssetsTTM), "</li>",
        "<li><b>Operating Margin:</b> ", safe_value(fin$OperatingMarginTTM), "</li>",
        "</ul><br>",
        "<h5><b>🧭 Operational Info:</b></h5>",
        "<ul>",
        "<li><b>CEO:</b> ", safe_value(fin$CEO), "</li>",
        "<li><b>Full Time Employees:</b> ", safe_value(fin$FullTimeEmployees), "</li>",
        "<li><b>Fiscal Year End:</b> ", safe_value(fin$FiscalYearEnd), "</li>",
        "<li><b>Latest Quarter:</b> ", safe_value(fin$LatestQuarter), "</li>",
        "<li><b>Quarterly Revenue Growth (YoY):</b> ", safe_value(fin$QuarterlyRevenueGrowthYOY), "</li>",
        "<li><b>Quarterly Earnings Growth (YoY):</b> ", safe_value(fin$QuarterlyEarningsGrowthYOY), "</li>",
        "</ul>",
        "</div>"
      )
      HTML(overview_html)
    })
    
    # 8) Company Metrics table
    output$companyMetrics <- renderDataTable({
      fin <- financialData()
      req(fin)
      km <- data.frame(
        Metric = c(
          "Name","Sector","Industry","Market Cap (USD)","Currency","P/E Ratio","EPS",
          "Dividend/Share","Dividend Yield","Revenue (TTM)","Gross Profit (TTM)",
          "ROE (TTM)","ROA (TTM)","Operating Margin (TTM)"
        ),
        Value = c(
          fin$Name, fin$Sector, fin$Industry, fin$MarketCapitalization, fin$Currency, fin$PERatio, fin$EPS,
          fin$DividendPerShare, fin$DividendYield, fin$RevenueTTM, fin$GrossProfitTTM,
          fin$ReturnOnEquityTTM, fin$ReturnOnAssetsTTM, fin$OperatingMarginTTM
        ),
        stringsAsFactors = FALSE
      )
      datatable(km, rownames = FALSE, options = list(pageLength = 15, dom = 'tip'))
    })
    
    # 9) Investment Advice — uses INSTANT cached sentiment
    output$advice <- renderUI({
      HTML(generate_investment_advice(financialData(), cachedSentimentScore()))
    })
    
    # 10) Sentiment Trend — if user hasn’t fetched live news, show cached trend
    output$sentimentTrend <- renderPlotly({
      sm <- cachedSentimentMonthly()
      if (is.null(sm) || nrow(sm) == 0) {
        sd <- sentimentData()
        req(sd)
        plot_ly(sd, x = ~month, y = ~mean_sentiment, type = 'scatter', mode = 'lines+markers',
                name = 'Avg Sentiment') %>%
          layout(
            title = "News Sentiment (Monthly Average)",
            xaxis = list(title = "Month"),
            yaxis = list(title = "Average Sentiment")
          )
      } else {
        plot_ly(sm, x = ~month, y = ~mean_sentiment, type = 'scatter', mode = 'lines+markers',
                name = 'Avg Sentiment (Cached)') %>%
          layout(
            title = "News Sentiment (Monthly Average — Cached)",
            xaxis = list(title = "Month"),
            yaxis = list(title = "Average Sentiment")
          )
      }
    })
    
    # ---- stop here after updates ----
    
  }, ignoreInit = TRUE, once = TRUE)
}

# Run app
shinyApp(ui, server)
