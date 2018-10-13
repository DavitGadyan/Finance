library(quantmod)
library(zoo)
library(xts)
# install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)


getSymbols('KO')
KO
ko<-Cl(KO)
getSymbols('P')
P
pep<-Cl(P)

# Define ko_pep 
ko_pep <- ko/pep

# Make a time series plot of ko_pep
plot.zoo(ko_pep)  

# Add as a reference, a horizontal line at 1
abline(h=1)

# Coca Cola stock's closing price always outperformed Pepsi's stock price

# Define the vector values
values<-c(4000,4000,2000)

# Define the vector weights
weights<-values/sum(values)

# Print the resulting weights
print(weights)


# Define marketcaps
marketcaps <- c(5, 8, 9, 20, 25, 100, 100, 500, 700, 2000)

# Compute the weights
weights <- marketcaps/sum(marketcaps)

# Inspect summary statistics 
summary(weights)

# Create a barplot of weights
barplot(weights)

# Vector of initial value of the assets
in_values <- c(1000,5000,2000)

# Vector of final values of the assets
fin_values <-c(1100,4500,3000)

# Weights as the proportion of total value invested in each assets
weights <-in_values/sum(in_values)

# Vector of simple returns of the assets 
returns <- (fin_values - in_values)/in_values

# Compute portfolio return using the portfolio return formula
preturns <- sum(weights*returns)

# 
# The simple return R expresses the percentage change in value of an investment. 
# The corresponding so-called "gross" return is defined as the future value of 1 USD invested 
# in the asset for one period, and is thus equal to 1+R.
# The gross return over two periods is obtained similarly. Let $R1$ be the return in the first period 
# and $R2$ the return in the second period. Then the end-value of a 1 USD investment is (1+$R1$)∗(1+$R2$).
# 
# The corresponding simple return over those two periods is: (1+$R1$)∗(1+$R2$)−1


### Calculating returns with PerformanceAnalytics

getSymbols(c('A','MSFT'),src='yahoo')

Return.calculate(A) # For one stock/asset

#### Doing for multiple stocks

# Create new environment
data_env <- new.env()
# Use getSymbols to load data into the environment
getSymbols(c("A", "MSFT"), env = data_env, auto.assign = TRUE)

# Look at a few rows of the SPY data
head(data_env$A)

# Extract volume column from each object
adjusted_list <- lapply(data_env, Ad)
# Merge each list element into one object
prices <- do.call(merge, adjusted_list)
head(prices)

returns<-Return.calculate(prices)
head(returns)

returns<-returns[-c(1),]



# Create the weights
eq_weights <- c(0.5, 0.5)

# Create a portfolio using buy and hold
pf_bh <- Return.portfolio(R = returns, weights = eq_weights)

# Create a portfolio rebalancing monthly 
pf_rebal <- Return.portfolio(R = returns, weights = eq_weights,rebalance_on = "months")

# Plot the time-series
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(pf_bh)
plot.zoo(pf_rebal)


# Create the weights
eq_weights <- c(0.5, 0.5)

# Create a portfolio using buy and hold
pf_bh <- Return.portfolio(returns, weights = eq_weights, verbose = TRUE)

# Create a portfolio that rebalances monthly
pf_rebal <- Return.portfolio(returns, weights = eq_weights, rebalance_on = "months", verbose = TRUE)

# Create end of eop_weight_bh
eop_weight_bh <- pf_bh$EOP.Weight

# Create eop_weight_reabl
eop_weight_rebal <- pf_rebal$EOP.Weight

# Plot end of period weights
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(eop_weight_bh$A.Adjusted)
plot.zoo(eop_weight_rebal$A.Adjusted)

# Analyzing performance
# Data for S&P 500 (used in chapter 2)
library(tseries)
sp500 <- get.hist.quote(instrument="^GSPC",start=as.Date("1985-12-31"),end=Sys.Date(),quote="AdjClose",quiet=T,compression="d")


# Convert the daily frequency of sp500 to monthly frequency: sp500_monthly
sp500_monthly <- to.monthly(sp500)

sp500_returns <- na.omit(Return.calculate(sp500_monthly[,4]))


# Print the first six rows of sp500_monthly
head(sp500_monthly)

options(scipen = 666)


# Time series plot
plot.zoo(sp500_returns)

# Compute the mean monthly returns
mean(sp500_returns)

# Compute the geometric mean of monthly returns
mean.geometric(sp500_returns)

# Compute the standard deviation
sd(sp500_returns)

library(Quandl)
library(dplyr)
rf<-Quandl('FRED/GS3M',type='zoo',collapse='monthly',start_date="1986-01-01")


rf<-rf/100
length(rf)

# Compute the annualized risk free rate
annualized_rf <- (1 + rf)^12 - 1

# Plot the annualized risk free rate
plot.zoo(annualized_rf )

# Compute the series of excess portfolio returns 
sp500_excess <- sp500_returns - rf

# Compare the mean
mean(sp500_excess)
mean(sp500_returns)

# Compute the Sharpe ratio
sp500_sharpe <- mean(sp500_excess) / sd(sp500_returns)

sp500_sharpe

plot.zoo(sp500_returns)
abline(h=0)
## As we may observe sharp ratio is negative for this period by taking 3-Month Treasury Constant Maturity Rate (optimal).. After analyzing the period taken from 2001 to 2018 we understand 
## the reasons for this situation as we may see that that after 2008 the return on S&P was negative which resulted into negative sharp ratio

## Sharpe Ratio is calculated by taking the mean of excess returns and dividing by the volatility of those returns.

# Compute the annualized mean
Return.annualized(sp500_returns,scale =12 )
??Return.annualized
# Compute the annualized standard deviation
StdDev.annualized(sp500_returns)

# Compute the annualized Sharpe ratio: ann_sharpe
ann_sharpe <- Return.annualized(sp500_returns,scale = 12)/StdDev.annualized(sp500_returns)

# Compute all of the above at once using table.AnnualizedReturns()
table.AnnualizedReturns(sp500_returns)

# Calculate the mean, volatility, and sharpe ratio of sp500_returns
returns_ann <- Return.annualized(sp500_returns,scale=12)
sd_ann <- StdDev.annualized(sp500_returns)
sharpe_ann <- SharpeRatio.annualized(sp500_returns, Rf = rf)

# Plotting the 12-month rolling annualized mean
chart.RollingPerformance(R = sp500_returns, width = 12,scale=12, FUN = "Return.annualized")
abline(h = returns_ann)

# Plotting the 12-month rolling annualized standard deviation
chart.RollingPerformance(R = sp500_returns, width = 12, scale=12, FUN = "StdDev.annualized")
abline(h = sd_ann)

# Plotting the 12-month rolling annualized Sharpe ratio
chart.RollingPerformance(R = sp500_returns, width = 12,FUN = "SharpeRatio.annualized", Rf = rf)
abline(h = sharpe_ann)
