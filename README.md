# Portfolio Optimization with Market Data

This repository implements a **portfolio optimization analysis using real market data**, based on the **Mean–Variance framework introduced by Harry Markowitz**.

The project demonstrates how quantitative models can support **optimal asset allocation under risk–return trade-offs**, combining data retrieval, portfolio optimization, and interactive visualization.

Full project report available in my portfolio: **[View Report](https://www.notion.so/Data-Driven-Portfolio-Optimization-32143fe0ba2e80ce8106d4fa676ad3f2)**

---

# Interactive Dashboard

Interactive visualization available here: **[View Dashboard](https://sadduca.github.io/portfolio-markowitz-optimization/)**

The dashboard allows exploration of:

* portfolio allocations
* efficient frontier portfolios
* risk–return trade-offs
* comparison with market benchmarks
* volatility and Sharpe ratios

Benchmarks include:

* S&P 500
* NASDAQ Composite

---

# Project Overview

Organizations frequently need to allocate resources across competing opportunities while balancing **expected return and risk**.

This project implements a **portfolio optimization engine** that:

* retrieves real market data
* estimates expected returns and volatility
* computes asset correlations
* generates the efficient frontier
* identifies optimal portfolios

The results are presented in an **interactive dashboard for exploratory analysis**.

---

# Key Features

* End-to-end quantitative workflow
* Real market data retrieved via API
* Mean–Variance portfolio optimization
* Efficient frontier visualization
* Benchmark comparison with major market indices
* Interactive dashboard for exploratory analysis

---

# Methodology

Historical asset prices are retrieved programmatically from **Yahoo Finance** using the **tidyquant** package in R.

Because the data retrieval is performed through an API interface, the entire workflow can be **automated and periodically updated with new market data**.

The analytical workflow consists of:

1. Download historical asset prices
2. Compute daily returns
3. Estimate expected returns and covariance matrix
4. Apply mean–variance optimization
5. Generate the efficient frontier
6. Identify optimal portfolios
7. Compare results with market benchmarks

---

# Repository Structure

```
portfolio-markowitz-optimization
│
├── index.html
├── efficient_frontier_plot.html
│
├── R
│   └── portfolio_optimization.R
│
└── README.md
```

* **R script**: performs the portfolio optimization analysis.
* **HTML files**: contain the interactive dashboard and visualizations.
* **GitHub Pages** hosts the dashboard.

---

# Requirements

R packages used in the project include:

````
# Portfolio optimization
library(fPortfolio)      # Mean–Variance portfolio optimization

# Data acquisition
library(tidyquant)       # Download financial data from Yahoo Finance

# Data manipulation
library(tidyverse)       # Data wrangling and functional pipelines

# Time series handling
library(timeSeries)      # Time-series format required by fPortfolio

# Visualization
library(plotly)          # Interactive financial visualizations
library(scales)          # Percentage and numeric formatting

# Dashboard generation
library(htmlwidgets)     # Export interactive plots to HTML
library(htmltools)       # Assemble the HTML dashboard
````

---

# Reproducibility

To reproduce the analysis:

1. Clone the repository.
2. Install required R packages.
3. Run the main R script.
4. The script will retrieve market data via API and regenerate the dashboard.

---

# Author

**Santino Adduca**

Data Analytics | Finance | Business Intelligence

If you would like to discuss this project or explore potential collaboration opportunities, feel free to reach out via email or connect with me on [LinkedIn](https://www.linkedin.com/in/santino-adduca/).
