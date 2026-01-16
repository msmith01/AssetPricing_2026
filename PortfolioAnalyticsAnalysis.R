rm(list=ls())

library(quantmod)
library(PerformanceAnalytics)

# symbol <- "AAPL"
# start_date <- "2015-01-01"
# end_date <- Sys.Date()

symbol <- "GME"
start_date <- "2020-01-01"
end_date <- "2021-06-30"

# symbol <- "C"
# start_date <- "2006-01-01"
# end_date <- "2009-12-31"

symbol <- "MRNA"
start_date <- "2019-01-01"
end_date <- "2021-12-31"

# Case: Moderna (COVID era)
#Stock value hinged on:
# - FDA approvals
# - clinical trial results


getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

prices <- Ad(get(symbol))
returns <- dailyReturn(prices, type = "log")
returns <- na.omit(returns)

mu <- mean(returns)
sigma <- sd(returns)

x <- seq(min(returns), max(returns), length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)

hist(as.numeric(returns), breaks = 50, probability = TRUE, col = "lightgray", border = "white")
lines(x, y, lwd = 2)


library(PerformanceAnalytics)

# QQ plot
# NOTES: For the Maderna case - with fat tails
# NOTES: S shaped - plot reaffirms fat-tails
# extreme returns occur more frequency than a normal model predicts
# - Economics: large negative/positive shocks are structural - not outliers
# risk is dominated by tail events, not variance
# Any model assuming normality will systematically underprice risk
qqnorm(as.numeric(returns))
qqline(as.numeric(returns))


library(PerformanceAnalytics)
library(zoo)

# Did not cover this plot
# Expected shortfall - conditional value at risk
# Case: Maderna case
rolling_ES <- rollapply(
  returns,
  width = 252,
  FUN = function(x) ES(x, p = 0.95, method = "historical"),
  by.column = FALSE,
  align = "right"
)

plot(rolling_ES)
