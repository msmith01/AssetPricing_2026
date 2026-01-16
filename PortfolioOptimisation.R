rm(list=ls())
options(scipen=999)

library(tidyverse)
library(tidyquant)
library(tsibble)
library(rsample)
library(Matrix)
library(scales)
library(glue)
library(corrplot)
start_date <- "2020-01-01"
end_date <- "2023-12-31"
symbols <- c("AAPL", "ABBV", "A",  "APD", "AA", "CF", "NVDA", "HOG", "WMT", "AMZN"
             ,"MSFT", "INTC", "ADBE", "AMG", "AKAM", "ALB", "ALK", "V", "PG", "COST", "ADBE", "KO", "BAC", "HD", "VZ", "AMGN", "TXN"
             , "UBER", "LOW", "NKE", "AMAT", "GS", "LMT", "PGR", "ADP", "ETN", "CDNS", "CME", "FCX", "APH"
)

portfolio_prices <- tq_get(
  symbols,
  from = start_date,
  to = end_date,
) %>% 
  dplyr::select(c(symbol, date, adjusted))

portfolio_prices_wide = portfolio_prices %>%
  pivot_wider(names_from = symbol, values_from = adjusted)


portfolio_daily_returns <- portfolio_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn, # R = (P[t] - P[t-1])/P[t-1]
    period = "daily",                              
    type = "log",
  )
portfolio_daily_returns_wide = portfolio_daily_returns %>% 
  pivot_wider(names_from = symbol, values_from = daily.returns)


# Calculate the mean returns
# Calculate variance-covariance matrix
# Create series of random weights
# Create series of returns and risks
# Create plot

portfolio_daily_returns_matrix = portfolio_daily_returns_wide %>% 
  column_to_rownames("date") %>% 
  as.matrix()

numberAssets = ncol(portfolio_daily_returns_matrix)
mu = colMeans(portfolio_daily_returns_matrix)
varianceCovariance = cov(portfolio_daily_returns_matrix)

heatmap(
  varianceCovariance,
  symm = TRUE,
  Colv = NA,
  Rowv = NA,
  scale = "none"
)

# Same but different style
ggplot(melt(varianceCovariance), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", fill = "Covariance")


# Plot correlations

corr <- cov2cor(varianceCovariance)

# Notes: 
# Most tickers co-move meaningfully with each other
# do not see strong square blocks (e.g. tech vs defensives vs financials) - Sector diversification exists but is weak
# Naive equal-weight ≠ diversified
corrplot(
  corr,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45
)



randomPortfolios = 3000
risk = NULL  #vector to store risk
rets = NULL  #vector to store returns

for (i in 1:randomPortfolios) {
  w = diff(c(0, sort(runif(numberAssets - 1)), 1))  # random weights
  r1 = t(w) %*% mu  #matrix multiplication
  sd1 = t(w) %*% varianceCovariance %*% w
  rets = rbind(rets, r1)
  risk = rbind(risk, sd1)
}

risk_reward = data.frame(
  Ret = rets*100, Risk = risk*100
  )

risk_reward %>% 
  ggplot(aes(Risk, Ret, colour = Ret)) +
  geom_point() +
  geom_point() + 
  geom_hline(yintercept = c(
    max(risk_reward$Ret), 
    median(risk_reward$Ret), 
    min(risk_reward$Ret)), colour = c("darkgreen", "darkgray", "darkred"), size = 1) +
  geom_vline(xintercept = risk_reward[(risk_reward$Risk == min(risk_reward$Risk)), ][, 2]) +
  labs(colour = "Portfolio Return", x = "Portfolio Risk", y = "Portfolio Return",
       title = glue("{randomPortfolios} Random Feasible Portfolios"),
       subtitle = "Fronteir Portfolios") + 
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

################################################################################
################################################################################
################################################################################

# Efficient (Minimum Variance) Portfolio
# Markowitz portfolio optimisation

library(PortfolioAnalytics)
# initialise with asset names uses time series data

portfolio_daily_returns_matrix_zoo = zoo(portfolio_daily_returns_matrix, order.by = as.Date(rownames(portfolio_daily_returns_matrix)))

# initialise the portfolio
port = portfolio.spec(assets = colnames(portfolio_daily_returns_matrix_zoo))
# add long only constraint
port = add.constraint(portfolio = port, type = "long_only")
# add full investment contraint
port = add.constraint(portfolio = port, type = "full_investment") # we can add weights here etc.

port

# Add objectives
# Objective is the "minimise risk" and "maximise reward"

# objective: minimise risk
port_rnd = add.objective(portfolio = port, type = "risk", name = "StdDev") # can add variance here
# objective: maximise return - Using random portfolios
port_rnd = add.objective(portfolio = port_rnd, type = "return", name = "mean")

# 1. optimise random portfolios - here we ass random to the optimise method
rand_p = optimize.portfolio(
  R = portfolio_daily_returns_matrix_zoo, 
  portfolio = port_rnd, 
  optimize_method = "random", 
  trace = TRUE, 
  search_size = 3000)
# plot
# NOTES: plot risk vs return
# - circles are single stocks
# - grey points are random portfolios from random optimization
# - blue point is the optimial portfolio
# - NVDA, FCX, UBER etc have high volatility
# KO, PG, COST - Are low return, low risk (defensive stocks)
# Optimal portfolio, minimum volatility and sits on the efficient boundary of the random portfolio cloud - AKA: global minimum-variance portfolio - no combination of assets gives a lower risk without sacrificing return
chart.RiskReward(rand_p, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)  #also plots the equally weighted portfolio
rand_p
###########################################################
# 2. Optimise for the minimum risk (min standard deviation)
# now we construct portfolios using min standard deviations
port_msd = add.objective(portfolio = port, type = "risk", name = "StdDev")
# here we pass ROI to the optimise method
minvar1 = optimize.portfolio(
  R = portfolio_daily_returns_matrix_zoo, 
  portfolio = port_msd, 
  optimize_method = "ROI", 
  trace = TRUE)
minvar1
# these are the optimal weights for our portfolio based on minimising the risk
# plot
plot(minvar1, risk.col = "StdDev", main = "Mean Variance Portfolio", chart.assets = TRUE)


# efficient frontier
minvar_ef = create.EfficientFrontier(R = portfolio_daily_returns_matrix_zoo, portfolio = port_msd,
                                     type = "mean-StdDev")

# NOTES
# - Any portfolio below the curve is suboptimal
# - curved line is the 'feasible' portfolios - for each given risk level this is the maximum achievable expected return
# - dashed streight line: Capital Market Line - rf = 0, tangent to the frontier and it's slope / Sharpe Ratio = 0.0754
# Black dot - tangency point - is the maximum Sharpe (tangency) portfolio - i.e. portfolio with the highest return per unit of risk
# efficient investors should hold: risk-free asset + this portfolio
# Also plotted are individual stocks (i.e. risk vs return if we held just single stocks (holding portfolio reduces risk))
# i.e. risk-adjusted returns come from diversifications, not signle stocks.
chart.EfficientFrontier(minvar_ef, match.col = "StdDev", type = "l", tangent.line = TRUE,
                        chart.assets = TRUE)

################################################################################

################################################################################
################################################################################
################################################################################

