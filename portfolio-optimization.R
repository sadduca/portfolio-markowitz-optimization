# =============================================================================
# MARKOWITZ EFFICIENT FRONTIER – 50 TECH STOCKS
# COMPLETE ANALYSIS + INTERACTIVE DASHBOARD FOR WEALTH MANAGEMENT
# =============================================================================
# Business Context:
#   This tool is designed for wealth managers and financial advisors to
#   visualize optimal portfolios based on the Markowitz mean‑variance model.
#   It analyzes 50 leading technology stocks over the 2021–2025 period,
#   identifies three key portfolios (Minimum Variance, Maximum Sharpe,
#   Maximum Return), and presents them in an interactive dashboard.
#
#   The dashboard can be used in client meetings to explain risk‑return
#   tradeoffs and to recommend personalised portfolios based on investor
#   profiles (conservative, balanced, aggressive). Allocations are shown
#   for a $1M investment.
#
#   The final output is a HTML file (wealth_management_dashboard.html) 
#
# Author: Santino Adduca
# =============================================================================

# -----------------------------------------------------------------------------
# 0. LOAD REQUIRED PACKAGES
# -----------------------------------------------------------------------------
library(fPortfolio)      # portfolio optimization (Markowitz)
library(tidyquant)       # download stock prices from Yahoo Finance
library(tidyverse)       # data manipulation and piping
library(timeSeries)      # time series format required by fPortfolio
library(plotly)          # interactive plots
library(scales)          # percentage formatting
library(htmlwidgets)     # save plotly graph as HTML
library(htmltools)       # assemble HTML dashboard

# -----------------------------------------------------------------------------
# 1. DEFINE TICKERS – 50 TECHNOLOGY COMPANIES
# -----------------------------------------------------------------------------
tickers <- c(
  # Core tech (9)
  "AAPL", "MSFT", "GOOGL", "AMZN", "META", "NVDA", "NFLX", "ADBE", "CRM",
  # Large cap tech (9)
  "ORCL", "IBM", "CSCO", "INTC", "AMD", "QCOM", "TXN", "AVGO", "MU",
  # Consumer & internet services (6)
  "UBER", "LYFT", "SNAP", "PINS", "SPOT", "RBLX",
  # Cloud & software (8)
  "NOW", "WDAY", "TEAM", "CRWD", "ZS", "DDOG", "MDB", "SNOW",
  # Semiconductors (5)
  "ADI", "NXPI", "MRVL", "ON", "STM",
  # More semiconductors & hardware (5)
  "MCHP", "SWKS", "QRVO", "HPQ", "DELL",
  # Cybersecurity & software (5)
  "PANW", "FTNT", "OKTA", "SPLK", "DOCU",
  # Internet & fintech (3)
  "SHOP", "PYPL", "SQ"
)

cat("📊 Downloading data for", length(tickers), "assets...\n")

# -----------------------------------------------------------------------------
# 2. DOWNLOAD PRICE DATA FROM YAHOO FINANCE (INCLUDING BENCHMARKS)
# -----------------------------------------------------------------------------
# Download tech stocks
prices <- tq_get(tickers, 
                 from = "2021-01-01", 
                 to   = "2025-12-31", 
                 get  = "stock.prices")

# Download benchmark indices: S&P 500 (^GSPC) and NASDAQ (^IXIC)
benchmarks <- tq_get(c("^GSPC", "^IXIC"),
                     from = "2021-01-01",
                     to   = "2025-12-31",
                     get  = "stock.prices")

# Keep only tickers with sufficient data (at least 30 monthly observations)
valid_tickers <- prices %>%
  group_by(symbol) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 30) %>%
  pull(symbol)

cat("✅ Assets with sufficient data:", length(valid_tickers), "\n")

# -----------------------------------------------------------------------------
# 3. COMPUTE MONTHLY RETURNS FOR STOCKS AND CONVERT TO timeSeries
# -----------------------------------------------------------------------------
returns <- prices %>%
  filter(symbol %in% valid_tickers) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               type = "arithmetic",
               col_rename = "return") %>%
  spread(key = symbol, value = return) %>%
  column_to_rownames("date") %>%
  as.timeSeries()

# Remove any columns with missing values (should be none now)
returns <- returns[, colSums(is.na(returns)) == 0]

cat("\n📊 Assets in final analysis:", ncol(returns), "\n")
print(colnames(returns))

# -----------------------------------------------------------------------------
# 4. COMPUTE MONTHLY RETURNS FOR BENCHMARKS AND ANNUALIZE
# -----------------------------------------------------------------------------
benchmark_returns <- benchmarks %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               type = "arithmetic",
               col_rename = "return") %>%
  spread(key = symbol, value = return) %>%
  column_to_rownames("date") %>%
  as.timeSeries()

# Annualize benchmark returns
benchmark_annual <- colMeans(benchmark_returns) * 12
sp500_return <- benchmark_annual["^GSPC"]
nasdaq_return <- benchmark_annual["^IXIC"]

# Calculate benchmark risk (annualized volatility)
sp500_risk <- sd(benchmark_returns[, "^GSPC"]) * sqrt(12)
nasdaq_risk <- sd(benchmark_returns[, "^IXIC"]) * sqrt(12)

# Calculate benchmark Sharpe ratios (using same risk-free rate)
rf <- 0.03
sp500_sharpe <- (sp500_return - rf) / sp500_risk
nasdaq_sharpe <- (nasdaq_return - rf) / nasdaq_risk

cat("\n📈 Benchmark annualized metrics (2021–2025):\n")
cat("   S&P 500  : Return:", percent(sp500_return, accuracy = 0.1), 
    " | Risk:", percent(sp500_risk, accuracy = 0.1),
    " | Sharpe:", round(sp500_sharpe, 2), "\n")
cat("   NASDAQ   : Return:", percent(nasdaq_return, accuracy = 0.1), 
    " | Risk:", percent(nasdaq_risk, accuracy = 0.1),
    " | Sharpe:", round(nasdaq_sharpe, 2), "\n")

# -----------------------------------------------------------------------------
# 5. SET UP PORTFOLIO SPECIFICATION
# -----------------------------------------------------------------------------
defaultSpec <- portfolioSpec()
setSolver(defaultSpec) <- "solveRquadprog"          # quadratic programming
setNFrontierPoints(defaultSpec) <- 100              # number of points on frontier

# -----------------------------------------------------------------------------
# 6. COMPUTE THE EFFICIENT FRONTIER
# -----------------------------------------------------------------------------
cat("\n📈 Calculating efficient frontier...\n")

frontier <- portfolioFrontier(
  data = returns,
  spec = defaultSpec,
  constraints = "LongOnly"          # no short selling
)

# -----------------------------------------------------------------------------
# 7. EXTRACT FRONTIER POINTS (MONTHLY) AND ANNUALIZE
# -----------------------------------------------------------------------------
frontier_points <- frontierPoints(frontier)
frontier_df <- as.data.frame(frontier_points)
colnames(frontier_df) <- c("risk_monthly", "return_monthly")

# Annualize: multiply return by 12, risk by sqrt(12)
frontier_df <- frontier_df %>%
  mutate(
    risk_annual   = risk_monthly * sqrt(12),
    return_annual = return_monthly * 12
  )

# -----------------------------------------------------------------------------
# 8. OBTAIN PORTFOLIO WEIGHTS FOR EACH FRONTIER POINT
# -----------------------------------------------------------------------------
weight_matrix <- getWeights(frontier)
colnames(weight_matrix) <- colnames(returns)

# -----------------------------------------------------------------------------
# 9. DEFINE RISK‑FREE RATE AND COMPUTE SHARPE RATIOS
# -----------------------------------------------------------------------------
rf <- 0.03
sharpe_values <- (frontier_df$return_annual - rf) / frontier_df$risk_annual

# -----------------------------------------------------------------------------
# 10. IDENTIFY KEY PORTFOLIOS ON THE FRONTIER
# -----------------------------------------------------------------------------
min_var_idx <- which.min(frontier_df$risk_annual)
max_sharpe_idx <- which.max(sharpe_values)
max_return_idx <- which.max(frontier_df$return_annual)

# -----------------------------------------------------------------------------
# 11. COMPUTE INDIVIDUAL ASSET METRICS (FOR REFERENCE)
# -----------------------------------------------------------------------------
assets_returns <- colMeans(returns) * 12
assets_risk    <- apply(returns, 2, sd) * sqrt(12)

assets_df <- data.frame(
  asset  = colnames(returns),
  return = assets_returns,
  risk   = assets_risk
) %>%
  arrange(desc(return))

# Display top 10 assets by return (console output)
cat("\n", strrep("=", 70), "\n", sep = "")
cat("📊 TOP 10 ASSETS BY ANNUALIZED RETURN\n")
cat(strrep("=", 70), "\n", sep = "")
print(head(assets_df, 10) %>%
        mutate(
          return_pct = percent(return, accuracy = 0.1),
          risk_pct   = percent(risk, accuracy = 0.1),
          sharpe     = round(return / risk, 2)
        ) %>%
        select(Asset = asset, Return = return_pct, Risk = risk_pct, Sharpe = sharpe))

# -----------------------------------------------------------------------------
# 12. SPLIT THE FRONTIER INTO TWO SEGMENTS FOR VISUALIZATION
# -----------------------------------------------------------------------------
inefficient_df <- frontier_df[1:min_var_idx, ]
efficient_df   <- frontier_df[min_var_idx:max_return_idx, ]

# -----------------------------------------------------------------------------
# 13. CREATE INTERACTIVE PLOT WITH PLOTLY
# -----------------------------------------------------------------------------
x_max_plot <- max(frontier_df$risk_annual) * 1.05
tangency_slope <- sharpe_values[max_sharpe_idx]

p <- plot_ly() %>%
  
  # ---- Inefficient frontier (red dashed line) ----
add_trace(x = inefficient_df$risk_annual, 
          y = inefficient_df$return_annual,
          type = 'scatter', mode = 'lines',
          line = list(color = 'red', width = 2, dash = 'dash'),
          name = 'Inefficient Frontier',
          hoverinfo = 'text',
          text = ~paste('Risk:', percent(inefficient_df$risk_annual, accuracy = 0.1),
                        '<br>Return:', percent(inefficient_df$return_annual, accuracy = 0.1))) %>%
  
  # ---- Efficient frontier (green solid line) ----
add_trace(x = efficient_df$risk_annual, 
          y = efficient_df$return_annual,
          type = 'scatter', mode = 'lines',
          line = list(color = 'darkgreen', width = 4),
          name = 'Efficient Frontier',
          hoverinfo = 'text',
          text = ~paste('Risk:', percent(efficient_df$risk_annual, accuracy = 0.1),
                        '<br>Return:', percent(efficient_df$return_annual, accuracy = 0.1),
                        '<br>Sharpe:', round((efficient_df$return_annual - rf) / efficient_df$risk_annual, 2))) %>%
  
  # ---- Individual assets (black circles with labels) ----
add_trace(x = assets_df$risk, 
          y = assets_df$return,
          type = 'scatter', mode = 'markers+text',
          text = assets_df$asset,
          textposition = 'top center',
          textfont = list(size = 9, color = 'gray40'),
          marker = list(size = 8, color = 'black', symbol = 'circle'),
          name = 'Individual Assets',
          hoverinfo = 'text',
          hovertext = ~paste(assets_df$asset,
                             '<br>Risk:', percent(assets_df$risk, accuracy = 0.1),
                             '<br>Return:', percent(assets_df$return, accuracy = 0.1),
                             '<br>Sharpe:', round(assets_df$return / assets_df$risk, 2))) %>%
  
  # ---- Minimum Variance portfolio (blue diamond) ----
add_trace(x = frontier_df$risk_annual[min_var_idx],
          y = frontier_df$return_annual[min_var_idx],
          type = 'scatter', mode = 'markers',
          marker = list(size = 16, color = '#1E3A8A', symbol = 'diamond'),
          name = 'Minimum Variance',
          hoverinfo = 'text',
          hovertext = paste('🔵 MINIMUM VARIANCE',
                            '<br>Risk:', percent(frontier_df$risk_annual[min_var_idx], accuracy = 0.1),
                            '<br>Return:', percent(frontier_df$return_annual[min_var_idx], accuracy = 0.1))) %>%
  
  # ---- Maximum Sharpe portfolio (green diamond) ----
add_trace(x = frontier_df$risk_annual[max_sharpe_idx],
          y = frontier_df$return_annual[max_sharpe_idx],
          type = 'scatter', mode = 'markers',
          marker = list(size = 16, color = '#10B981', symbol = 'diamond'),
          name = 'Maximum Sharpe',
          hoverinfo = 'text',
          hovertext = paste('🟢 MAXIMUM SHARPE',
                            '<br>Risk:', percent(frontier_df$risk_annual[max_sharpe_idx], accuracy = 0.1),
                            '<br>Return:', percent(frontier_df$return_annual[max_sharpe_idx], accuracy = 0.1),
                            '<br>Sharpe:', round(sharpe_values[max_sharpe_idx], 2))) %>%
  
  # ---- Maximum Return portfolio (red diamond) ----
add_trace(x = frontier_df$risk_annual[max_return_idx],
          y = frontier_df$return_annual[max_return_idx],
          type = 'scatter', mode = 'markers',
          marker = list(size = 20, color = '#DC2626', symbol = 'diamond'),
          name = 'Maximum Return',
          hoverinfo = 'text',
          hovertext = paste('🔴 MAXIMUM RETURN',
                            '<br>Risk:', percent(frontier_df$risk_annual[max_return_idx], accuracy = 0.1),
                            '<br>Return:', percent(frontier_df$return_annual[max_return_idx], accuracy = 0.1))) %>%
  
  # ---- Capital Market Line (orange dashed) ----
add_trace(x = c(0, x_max_plot),
          y = c(rf, rf + tangency_slope * x_max_plot),
          type = 'scatter', mode = 'lines',
          line = list(color = 'orange', width = 2, dash = 'dash'),
          name = 'Capital Market Line',
          hoverinfo = 'none') %>%
  
  # ---- Risk‑free rate line (gray dotted) ----
add_trace(x = c(0, x_max_plot),
          y = c(rf, rf),
          type = 'scatter', mode = 'lines',
          line = list(color = 'gray', width = 1, dash = 'dot'),
          name = 'Risk-free Rate (3%)',
          hoverinfo = 'none') %>%
  
  # ---- Layout ----
layout(
  title = list(
    text = paste("Markowitz Efficient Frontier –", ncol(returns), "Technology Stocks"),
    font = list(size = 16)
  ),
  xaxis = list(
    title = "Annualized Risk (Volatility)",
    tickformat = ".1%",
    range = c(0, x_max_plot),
    gridcolor = 'lightgray',
    zeroline = TRUE,
    zerolinecolor = 'black'
  ),
  yaxis = list(
    title = "Annualized Expected Return",
    tickformat = ".1%",
    range = c(0, max(frontier_df$return_annual) * 1.05),
    gridcolor = 'lightgray',
    zeroline = TRUE,
    zerolinecolor = 'black'
  ),
  hovermode = 'closest',
  legend = list(
    orientation = 'h',
    y = -0.2,
    x = 0.3,
    font = list(size = 10)
  ),
  margin = list(b = 100, l = 80, r = 40, t = 60)
)


# -----------------------------------------------------------------------------
# 14. CONSOLE SUMMARY OF KEY PORTFOLIOS
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("📊 EFFICIENT FRONTIER SUMMARY\n")
cat(strrep("=", 70), "\n", sep = "")

# Minimum Variance
cat("\n🔵 MINIMUM VARIANCE PORTFOLIO:\n")
weights_minvar <- weight_matrix[min_var_idx, ]
significant_minvar <- sort(weights_minvar[weights_minvar > 0.01], decreasing = TRUE)
minvar_return <- frontier_df$return_annual[min_var_idx]
minvar_risk <- frontier_df$risk_annual[min_var_idx]
minvar_sharpe <- (minvar_return - rf) / minvar_risk

cat("  Return:", percent(minvar_return, accuracy = 0.1), "\n")
cat("  Risk:"  , percent(minvar_risk, accuracy = 0.1), "\n")
cat("  Sharpe:" , round(minvar_sharpe, 2), "\n")
cat("  Top 5 weights (%):\n")
print(round(head(significant_minvar, 5) * 100, 2))

# Maximum Sharpe
cat("\n🟢 MAXIMUM SHARPE PORTFOLIO:\n")
weights_sharpe <- weight_matrix[max_sharpe_idx, ]
significant_sharpe <- sort(weights_sharpe[weights_sharpe > 0.01], decreasing = TRUE)
sharpe_return <- frontier_df$return_annual[max_sharpe_idx]
sharpe_risk <- frontier_df$risk_annual[max_sharpe_idx]
sharpe_value <- sharpe_values[max_sharpe_idx]

cat("  Return:", percent(sharpe_return, accuracy = 0.1), "\n")
cat("  Risk:"  , percent(sharpe_risk, accuracy = 0.1), "\n")
cat("  Sharpe:" , round(sharpe_value, 2), "\n")
cat("  Top 5 weights (%):\n")
print(round(head(significant_sharpe, 5) * 100, 2))

# Maximum Return
cat("\n🔴 MAXIMUM RETURN PORTFOLIO:\n")
weights_maxret <- weight_matrix[max_return_idx, ]
significant_maxret <- sort(weights_maxret[weights_maxret > 0.01], decreasing = TRUE)
maxret_return <- frontier_df$return_annual[max_return_idx]
maxret_risk <- frontier_df$risk_annual[max_return_idx]
maxret_sharpe <- (maxret_return - rf) / maxret_risk

cat("  Return:", percent(maxret_return, accuracy = 0.1), "\n")
cat("  Risk:"  , percent(maxret_risk, accuracy = 0.1), "\n")
cat("  Sharpe:" , round(maxret_sharpe, 2), "\n")
if (length(significant_maxret) > 0) {
  cat("  Top 5 weights (%):\n")
  print(round(head(significant_maxret, 5) * 100, 2))
} else {
  cat("  (All weights ≤ 1% – portfolio is extremely concentrated)\n")
}

# -----------------------------------------------------------------------------
# 15. PREPARE DATA FOR DASHBOARD TABLES
# -----------------------------------------------------------------------------
min_var_df <- data.frame(
  Asset = names(significant_minvar),
  Weight = significant_minvar,
  Weight_pct = percent(significant_minvar, accuracy = 0.1),
  Allocation = paste0("$", format(round(significant_minvar * 1000000), big.mark = ","))
) %>% arrange(desc(Weight))

sharpe_df <- data.frame(
  Asset = names(significant_sharpe),
  Weight = significant_sharpe,
  Weight_pct = percent(significant_sharpe, accuracy = 0.1),
  Allocation = paste0("$", format(round(significant_sharpe * 1000000), big.mark = ","))
) %>% arrange(desc(Weight))

maxret_df <- data.frame(
  Asset = names(significant_maxret),
  Weight = significant_maxret,
  Weight_pct = percent(significant_maxret, accuracy = 0.1),
  Allocation = paste0("$", format(round(significant_maxret * 1000000), big.mark = ","))
) %>% arrange(desc(Weight))

# -----------------------------------------------------------------------------
# 16. CREATE HTML DASHBOARD
# -----------------------------------------------------------------------------
# Save interactive plot
saveWidget(p, file = "index.html", selfcontained = TRUE)

# Helper function for allocation tables
create_html_table <- function(data, title, color) {
  tags$div(
    style = paste0("flex: 1; min-width: 280px; background: white; border-radius: 12px; 
                   padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); 
                   border-top: 4px solid ", color, ";"),
    
    tags$h4(title, style = "text-align: center; margin: 0 0 20px 0; color: #333;"),
    
    tags$div(
      style = "width: 100%;",
      lapply(1:nrow(data), function(i) {
        weight_num <- as.numeric(gsub("%", "", data$Weight_pct[i])) / 100
        tags$div(
          style = "margin-bottom: 15px;",
          
          tags$div(
            style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
            tags$span(data$Asset[i], style = "font-weight: 500;"),
            tags$span(data$Weight_pct[i], style = "color: #666;")
          ),
          
          tags$div(
            style = "width: 100%; background-color: #f0f0f0; border-radius: 4px; height: 8px; overflow: hidden;",
            tags$div(
              style = paste0("width: ", weight_num * 100, "%; background-color: ", color, 
                             "; height: 8px; border-radius: 4px;")
            )
          ),
          
          tags$div(
            style = "text-align: right; margin-top: 5px; color: #666; font-size: 0.9em;",
            data$Allocation[i]
          )
        )
      })
    )
  )
}

# Build dashboard 
dashboard <- div(
  style = "font-family: 'Segoe UI', Roboto, Arial, sans-serif; 
           max-width: 1400px; margin: 0 auto; padding: 30px; 
           background: white;",  
  
  # Header 
  div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 30px;",
      h1("📊 Wealth Management Portfolio Optimizer", style = "color: #2c3e50; margin: 0;"),
      div(style = "text-align: right;",
          p("Santino Adduca", style = "margin: 0; font-weight: 500;"),
          p("Data Analyst", style = "margin: 0; color: #666;")
      )
  ),
  
  # Investor profile cards with full benchmark comparisons
  div(style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 20px; margin-bottom: 30px;",
      
      # Conservative (dark blue)
      div(style = "background: #1E3A8A; border-radius: 15px; padding: 20px; color: white;",
          h3("🔵 Conservative", style = "margin: 0 0 15px 0; text-align: center;"),
          
          # Returns row
          div(style = "display: flex; justify-content: space-around; align-items: center; 
                     background: rgba(255,255,255,0.1); border-radius: 8px; padding: 8px; margin-bottom: 8px;",
              div(style = "text-align: center; width: 33%;",
                  p("Return", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(percent(minvar_return, accuracy = 0.1), 
                    style = "margin: 0; font-size: 1.2em; font-weight: 700;")
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs S&P", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(minvar_return - sp500_return >= 0, "+", ""), 
                           percent(minvar_return - sp500_return, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(minvar_return - sp500_return >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs NDX", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(minvar_return - nasdaq_return >= 0, "+", ""), 
                           percent(minvar_return - nasdaq_return, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(minvar_return - nasdaq_return >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              )
          ),
          
          # Risk row - Red when positive (higher risk), Green when negative (lower risk)
          div(style = "display: flex; justify-content: space-around; align-items: center; 
                     background: rgba(255,255,255,0.05); border-radius: 8px; padding: 8px; margin-bottom: 8px;",
              div(style = "text-align: center; width: 33%;",
                  p("Risk", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(percent(minvar_risk, accuracy = 0.1), 
                    style = "margin: 0; font-size: 1.2em; font-weight: 700;")
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs S&P", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(minvar_risk - sp500_risk >= 0, "+", ""), 
                           percent(minvar_risk - sp500_risk, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(minvar_risk - sp500_risk >= 0, "#fed7d7", "#a7f3d0"), ";")
                  )
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs NDX", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(minvar_risk - nasdaq_risk >= 0, "+", ""), 
                           percent(minvar_risk - nasdaq_risk, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(minvar_risk - nasdaq_risk >= 0, "#fed7d7", "#a7f3d0"), ";")
                  )
              )
          ),
          
          # Sharpe row - 2 decimal places
          div(style = "display: flex; justify-content: space-around; align-items: center; 
                     background: rgba(255,255,255,0.05); border-radius: 8px; padding: 8px; margin-bottom: 8px;",
              div(style = "text-align: center; width: 33%;",
                  p("Sharpe", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(round(minvar_sharpe, 2), 
                    style = "margin: 0; font-size: 1.2em; font-weight: 700;")
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs S&P", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(minvar_sharpe - sp500_sharpe >= 0, "+", ""), 
                           round(minvar_sharpe - sp500_sharpe, 2)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(minvar_sharpe - sp500_sharpe >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs NDX", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(minvar_sharpe - nasdaq_sharpe >= 0, "+", ""), 
                           round(minvar_sharpe - nasdaq_sharpe, 2)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(minvar_sharpe - nasdaq_sharpe >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              )
          ),
          
          # Description
          p("Capital preservation", style = "margin: 10px 0 0 0; text-align: center; opacity: 0.9; font-size: 0.9em;")
      ),
      
      # Balanced (green) 
      div(style = "background: #10B981; border-radius: 15px; padding: 20px; color: white;",
          h3("🟢 Balanced", style = "margin: 0 0 15px 0; text-align: center;"),
          
          # Returns row
          div(style = "display: flex; justify-content: space-around; align-items: center; 
                     background: rgba(255,255,255,0.1); border-radius: 8px; padding: 8px; margin-bottom: 8px;",
              div(style = "text-align: center; width: 33%;",
                  p("Return", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(percent(sharpe_return, accuracy = 0.1), 
                    style = "margin: 0; font-size: 1.2em; font-weight: 700;")
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs S&P", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(sharpe_return - sp500_return >= 0, "+", ""), 
                           percent(sharpe_return - sp500_return, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(sharpe_return - sp500_return >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs NDX", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(sharpe_return - nasdaq_return >= 0, "+", ""), 
                           percent(sharpe_return - nasdaq_return, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(sharpe_return - nasdaq_return >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              )
          ),
          
          # Risk row - Red when positive (higher risk), Green when negative (lower risk)
          div(style = "display: flex; justify-content: space-around; align-items: center; 
                     background: rgba(255,255,255,0.05); border-radius: 8px; padding: 8px; margin-bottom: 8px;",
              div(style = "text-align: center; width: 33%;",
                  p("Risk", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(percent(sharpe_risk, accuracy = 0.1), 
                    style = "margin: 0; font-size: 1.2em; font-weight: 700;")
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs S&P", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(sharpe_risk - sp500_risk >= 0, "+", ""), 
                           percent(sharpe_risk - sp500_risk, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(sharpe_risk - sp500_risk >= 0, "#fed7d7", "#a7f3d0"), ";")
                  )
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs NDX", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(sharpe_risk - nasdaq_risk >= 0, "+", ""), 
                           percent(sharpe_risk - nasdaq_risk, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(sharpe_risk - nasdaq_risk >= 0, "#fed7d7", "#a7f3d0"), ";")
                  )
              )
          ),
          
          # Sharpe row - 2 decimal places
          div(style = "display: flex; justify-content: space-around; align-items: center; 
                     background: rgba(255,255,255,0.05); border-radius: 8px; padding: 8px; margin-bottom: 8px;",
              div(style = "text-align: center; width: 33%;",
                  p("Sharpe", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(round(sharpe_value, 2), 
                    style = "margin: 0; font-size: 1.2em; font-weight: 700;")
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs S&P", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(sharpe_value - sp500_sharpe >= 0, "+", ""), 
                           round(sharpe_value - sp500_sharpe, 2)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(sharpe_value - sp500_sharpe >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs NDX", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(sharpe_value - nasdaq_sharpe >= 0, "+", ""), 
                           round(sharpe_value - nasdaq_sharpe, 2)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(sharpe_value - nasdaq_sharpe >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              )
          ),
          
          p("Optimal risk-adjusted", style = "margin: 10px 0 0 0; text-align: center; opacity: 0.9; font-size: 0.9em;")
      ),
      
      # Aggressive (red) 
      div(style = "background: #DC2626; border-radius: 15px; padding: 20px; color: white;",
          h3("🔴 Aggressive", style = "margin: 0 0 15px 0; text-align: center;"),
          
          # Returns row
          div(style = "display: flex; justify-content: space-around; align-items: center; 
                     background: rgba(255,255,255,0.1); border-radius: 8px; padding: 8px; margin-bottom: 8px;",
              div(style = "text-align: center; width: 33%;",
                  p("Return", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(percent(maxret_return, accuracy = 0.1), 
                    style = "margin: 0; font-size: 1.2em; font-weight: 700;")
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs S&P", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(maxret_return - sp500_return >= 0, "+", ""), 
                           percent(maxret_return - sp500_return, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(maxret_return - sp500_return >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs NDX", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(maxret_return - nasdaq_return >= 0, "+", ""), 
                           percent(maxret_return - nasdaq_return, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(maxret_return - nasdaq_return >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              )
          ),
          
          # Risk row - Red when positive (higher risk), Green when negative (lower risk)
          div(style = "display: flex; justify-content: space-around; align-items: center; 
                     background: rgba(255,255,255,0.05); border-radius: 8px; padding: 8px; margin-bottom: 8px;",
              div(style = "text-align: center; width: 33%;",
                  p("Risk", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(percent(maxret_risk, accuracy = 0.1), 
                    style = "margin: 0; font-size: 1.2em; font-weight: 700;")
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs S&P", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(maxret_risk - sp500_risk >= 0, "+", ""), 
                           percent(maxret_risk - sp500_risk, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(maxret_risk - sp500_risk >= 0, "#fed7d7", "#a7f3d0"), ";")
                  )
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs NDX", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(maxret_risk - nasdaq_risk >= 0, "+", ""), 
                           percent(maxret_risk - nasdaq_risk, accuracy = 0.1)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(maxret_risk - nasdaq_risk >= 0, "#fed7d7", "#a7f3d0"), ";")
                  )
              )
          ),
          
          # Sharpe row - 2 decimal places
          div(style = "display: flex; justify-content: space-around; align-items: center; 
                     background: rgba(255,255,255,0.05); border-radius: 8px; padding: 8px; margin-bottom: 8px;",
              div(style = "text-align: center; width: 33%;",
                  p("Sharpe", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(round(maxret_sharpe, 2), 
                    style = "margin: 0; font-size: 1.2em; font-weight: 700;")
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs S&P", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(maxret_sharpe - sp500_sharpe >= 0, "+", ""), 
                           round(maxret_sharpe - sp500_sharpe, 2)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(maxret_sharpe - sp500_sharpe >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              ),
              div(style = "text-align: center; width: 33%;",
                  p("vs NDX", style = "margin: 0; font-size: 0.7em; opacity: 0.7;"),
                  p(paste0(ifelse(maxret_sharpe - nasdaq_sharpe >= 0, "+", ""), 
                           round(maxret_sharpe - nasdaq_sharpe, 2)), 
                    style = paste0("margin: 0; font-size: 1.1em; font-weight: 600; color: ", 
                                   ifelse(maxret_sharpe - nasdaq_sharpe >= 0, "#a7f3d0", "#fed7d7"), ";")
                  )
              )
          ),
          
          p("Maximum growth", style = "margin: 10px 0 0 0; text-align: center; opacity: 0.9; font-size: 0.9em;")
      )
  ),
  
  # Interactive plot
  div(style = "margin: 40px 0; border-radius: 15px; overflow: hidden; 
               box-shadow: 0 10px 30px rgba(0,0,0,0.15); height: 600px;",
      tags$iframe(
        src = "index.html",
        style = "width: 100%; height: 100%; border: none;",
        allowfullscreen = "true"
      )
  ),
  
  # Portfolio allocation tables
  h2("📋 Portfolio Allocations", style = "margin: 40px 0 20px 0;"),
  p("Based on $1,000,000 investment", style = "color: #666; margin: -10px 0 30px 0;"),
  
  div(style = "display: flex; gap: 25px; flex-wrap: wrap;",
      create_html_table(min_var_df, "🔵 Conservative Portfolio", "#1E3A8A"),
      create_html_table(sharpe_df, "🟢 Balanced Portfolio", "#10B981"),
      create_html_table(maxret_df, "🔴 Aggressive Portfolio", "#DC2626")
  ),
  
  # Footer with metadata
  div(style = "margin-top: 50px; padding: 20px; background: #f8f9fa; 
             border-radius: 10px; text-align: center; color: #666;",
      
      HTML(paste0(
        "Assets: ", ncol(returns), " tech stocks | 2021–2025 | Risk-free rate: 3%<br>",
        
        "Benchmarks: S&P 500 ", percent(sp500_return, accuracy = 0.1),
        " (Risk: ", percent(sp500_risk, accuracy = 0.1),
        ", Sharpe: ", round(sp500_sharpe, 2), ") | ",
        
        "NASDAQ ", percent(nasdaq_return, accuracy = 0.1),
        " (Risk: ", percent(nasdaq_risk, accuracy = 0.1),
        ", Sharpe: ", round(nasdaq_sharpe, 2), ")<br>",
        
        "Data source: Yahoo Finance (via tidyquant) | ",
        "Generated on ", Sys.Date(), "<br>",
        
        "Santino Adduca • Data Analyst"
      ))
  )
)

# Save the complete dashboard as an HTML file
save_html(dashboard, file = "wealth_management_dashboard.html")
