rm(list=ls())
library(quantmod)
library(tidyverse)
# install.packages("FinCal")
##########################
# NPV analysis
library(FinCal)


investment = 700
cash_flows = c(-investment, 500, 400, 200, 100, 100)
t = 5
r = 0.12

npv(r = r, cf = cash_flows)


r_seq = seq(0.01, 0.12, by = 0.01)
r_seq = seq(0.01, 0.99, by = 0.01)


map(r_seq, npv, cf = cash_flows) %>% 
  unlist() %>% 
  plot()
# so an increase in the discount rate decreases the NPV - the higher the interest the less valuable future cash flows become when converted to their PV
# - because of time value of money - when IRs are higher the opportunity cost of not having money today is greater, making future cash inflows less valuable
# Higher interest rates often reflect higher perceived risk.
# Higher interest rates can also be indicative of higher expected inflation in the future - inflation erodes the value of future cash flows.


###########################

######## Yield curve analysis



stock = c("AAPL", "MSFT", "GOOG")
stock = "AAPL"
getSymbols(stock)

###
#stock = c("AAPL", "MSFT", "GOOG")
#getSymbols(stock)

#stocks = do.call(merge, lapply(stock, function(x){Cl(get(x))}))
#stocks = stocks[year(index(stocks)) >= 2015 & year(index(stocks)) <= 2017]

###


dividends = getDividends(stock)
returns = dailyReturn(AAPL)
plot(returns)
x = na.locf(merge(AAPL, dividends))

plot(dividends)

#### Calculate the price of one share of AAPL considering that expected dividends per share for the next two years are:

# compute the yearly growth rate in dividends

dividends$year = year(dividends)
dividends$dividend_growth = dividends$AAPL / lag(dividends$AAPL) - 1

tail(AAPL)

0.245 / (1+0.088) + (0.245+186) / (1+0.088)^2


### Gordon Growth Model
# Assume dividend of $2.50, a dividend growth rate of 4% and a discount rate of 7%
gordonGrowthModel <- function (dividend,growthrate,discountrate){
  dividend * (1+growthrate)/(discountrate-growthrate)
}

# So, a fair value of the stock would be:
gordonGrowthModel(2.50,.04,.07)

### Using AAPL 
gordonGrowthModel(dividend = 0.240, growthrate = 0.045, discountrate = 0.07)
