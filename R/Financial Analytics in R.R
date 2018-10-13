book_assumptions<-data.frame(year=c(1,2,3,4,5),sales=c(175,200,180,100,50))

# Define function: calc_business_model
calc_business_model <- function(assumptions, price, print_cost, ship_cost){
  cashflow <- assumptions
  cashflow$revenue <- cashflow$sales * price
  cashflow$direct_expense <- cashflow$sales * (print_cost + ship_cost) 
  cashflow$gross_profit <- cashflow$revenue - cashflow$direct_expense
  cashflow
}

# Call calc_business_model function for different sales prices
calc_business_model(book_assumptions, 20, 0.5, 2)$gross_profit
calc_business_model(book_assumptions, 25, 0.5, 2)$gross_profit

cashflow<-calc_business_model(book_assumptions, 20, 0.5, 2)
library(ggplot2)

production<-data.frame(Month=c(1:60),Units=sample.int(30, 60, replace = TRUE))

# Inputs
cost <- 100000
life <- 60
salvage <- 10000

# Compute depreciation
production$Depr_Straight <- (cost - salvage)/life
production$Depr_UnitsProd <- (cost - salvage)*(production$Units) / sum(production$Units)

# Plot two depreciation schedules
ggplot(production, aes(x = Month)) + 
  geom_line(aes(y = Depr_Straight)) + 
  geom_line(aes(y = Depr_UnitsProd))

# Business model
cashflow$revenue <- cashflow$revenue + 2 * cashflow$sales
cashflow$gross_profit <- cashflow$revenue - cashflow$direct_expense

# Income statement
cashflow$depr_sl <- (1000 - 0) / 5
cashflow$operating_profit <- cashflow$gross_profit - cashflow$depr_sl
cashflow$tax <- cashflow$operating_profit * 0.3
cashflow$net_income <- cashflow$operating_profit - cashflow$tax

# Inspect dataset
cashflow


# Assign input variables
fv <- 100
r <- 0.08

# Calculate PV if receive FV in 1 year
pv_1 <- fv/(1+r)^1
pv_1

# Calculate PV if receive FV in 5 years
pv_5 <- fv/(1+r)^5
pv_5

# Calculate difference
pv_1 - pv_5

library(dplyr)
# Define PV function: calc_pv
calc_pv <- function(fv, r, n){
  pv <- fv/(1+r)^n
  pv
}

# Use PV function for 1 input
calc_pv(100, 0.08, 5)

# Use PV function for range of inputs
n_range <- c(1:10)
pv_range <- calc_pv(100, 0.08, n_range)
pv_range
# Calculate present values in dataframe
present_values <- data.frame(n = 1:10) %>% mutate(pv = calc_pv(100, 0.08, n))

# Plot relationship between time periods versus present value
ggplot(present_values, 
       aes(x = n, y = pv)) +
  geom_line() +
  geom_label(aes(label = paste0("$",round(pv,0)))) +
  ylim(0,100) +
  labs(
    title = "Discounted Value of $100 by Year Received",
    x = "Number of Years in the Future",
    y = "Present Value ($)"
  )


# Calculate present values over range of time periods and discount rates
present_values <- 
  expand.grid(n = 1:10, r = seq(0.05,0.12,0.01)) %>%
  mutate(pv = calc_pv(100, r, n))

# Plot present value versus time delay with a separate colored line for each rate
ggplot(present_values, aes(x = n, y = pv, col = factor(r))) +
  geom_line() +
  ylim(0,100) +
  labs(
    title = "Discounted Value of $100 by Year Received",
    x = "Number of Years in the Future",
    y = "Present Value ($)",
    col = "Discount Rate"
  )

# Convert monthly to other time periods
r1_mth <-0.005
r1_quart <- (1 + r1_mth)^3 - 1
r1_semi <- (1 + r1_mth)^6 - 1
r1_ann <- (1 + r1_mth)^12 - 1

# Convert years to other time periods
r2_ann <-0.08
r2_mth <- (1 + r2_ann)^(1/12) - 1
r2_quart <- (1 + r2_ann)^(1/4) - 1

# Convert real to nominal
r1_real <- 0.08
inflation1 <- 0.03
(r1_nom <- (1 + r1_real)*(1+inflation1) - 1)

# Convert nominal to real
r2_nom <- 0.2
inflation2 <- 0.05
(r2_real <- (1+r2_nom)/(1+inflation2) - 1)


# Define cashflows
cashflow_a <- c(5000, rep(0,6))
cashflow_b <- c(0, rep(1000, 6))

# Calculate pv for each time period
disc_cashflow_a <- calc_pv(cashflow_a, 0.06, 0:6)
disc_cashflow_b <- calc_pv(cashflow_b,0.06,0:6)

# Calculate and report total present value for each option
(pv_a <- sum(disc_cashflow_a))
(pv_b <- sum(disc_cashflow_b))

# Define cashflows
cashflow_old <- rep(-500, 11)
cashflow_new <- c(-2200,rep(-300,10))
options <- 
  data.frame(time = rep(0:10, 2),
             option = c(rep("Old",11),rep("New",11)),
             cashflow = c(cashflow_old, cashflow_new))

# Calculate total expenditure with and without discounting
options %>%
  group_by(option) %>%
  summarize(sum_cashflow = sum(cashflow),
            sum_disc_cashflow = sum(calc_pv(cashflow,0.12,time) ))

cashflows<-c(-50000,1000,5000,5000,5000,10000,10000,10000,10000,10000,10000)

# Calculate cumulative cashflows
cum_cashflows <- cumsum(cashflows)

# Identify payback period
payback_period <- min(which(cum_cashflows >= 0)) - 1

# View result
payback_period

# Define payback function: calc_payback
calc_payback <- function(cashflows) {
  
  cum_cashflows <- cumsum(cashflows)
  payback_period <- min(which(cum_cashflows>=0)) - 1
  payback_period
  
}

# Test out our function
cashflows <- c(-100, 50, 50, 50)
calc_payback(cashflows) == 2


# normal payback period
payback_period <- calc_payback(cashflows)

# discounted payback period
discounted_cashflows <- calc_pv(cashflows, r = 0.06, n = 0:(length(cashflows)-1) )
payback_period_disc <- calc_payback(discounted_cashflows)

# compare results
payback_period
payback_period_disc

# Define NPV function: calc_npv
calc_npv <- function(cashflows, r) {
  
  n <- 0:(length(cashflows) - 1)
  npv <- sum( calc_pv(cashflows, r, n) )
  npv
  
}

# Define IRR function: calc_irr
calc_irr <- function(cashflows) {
  
  uniroot(calc_npv, 
          interval = c(0, 1), 
          cashflows = cashflows)$root
  
}

# Try out function on valid input
cashflows <- c(-100, 20, 20, 20, 20, 20, 20, 10, 5)
calc_irr(cashflows)


# Define profitability index function: calc_profitability_index
calc_profitability_index <- function(init_investment, future_cashflows, r) {
  discounted_future_cashflows <- calc_npv(future_cashflows, r)
  discounted_future_cashflows / abs(init_investment)
}

# Try out function on valid input
init_investment <- -100
cashflows <- c(0, 20, 20, 20, 20, 20, 20, 10, 5)
calc_profitability_index(init_investment, cashflows, 0.06)


# Pull last year cashflow from vector of cashflows
last_year_cashflow <- cashflow[length(cashflow)]
last_period_n <- length(cashflow) - 1

# Calculate terminal value for different discount raes
terminal_value_1 <- last_year_cashflow / ((0.15 - 0.10)*(1 + 0.15)^last_period_n)
terminal_value_2 <- last_year_cashflow / ((0.15 - 0.01)*(1 + 0.15)^last_period_n)
terminal_value_3 <- last_year_cashflow / ((0.15 + 0.05)*(1 + 0.15)^last_period_n)

# Inspect results
terminal_value_1 
terminal_value_2
terminal_value_3


assumptions<-data.frame(year=c(0:5),unit_sales_per_day=c(0,10,12,14,15,16),capex=c(5000,0,0,0,0,0),pct_cannibalization=c(0.00,0.25,0.25,0.25,0.25,0.25),
                                                                                                                         maintenance_cost=c(0,250,250,250,250,250),depreciation_cost=c(0,500,500,500,500,500),
                                                                                                                         profit_margin_per_nitro=c(3,3,3,3,3,3),profit_margin_per_regular=c(1,1,1,1,1,1),labor_cost_per_hour=c(8,8,8,8,8,8),days_open_per_year=c(250,250,250,250,250,250))
# Plot the trend of unit_sales_per_day by year
ggplot(assumptions, 
       aes(x = year, y = unit_sales_per_day)) + 
  geom_line()

tax_rate<-0.36
# Create the cashflow_statement dataframe
cashflow_statement <-
  mutate(assumptions,
         sales_per_year = unit_sales_per_day * days_open_per_year,
         sales_revenue = sales_per_year * profit_margin_per_nitro,
         total_revenue = sales_revenue,
         labor_cost = days_open_per_year * 0.5 * labor_cost_per_hour,
         cannibalization_cost = sales_per_year * pct_cannibalization * profit_margin_per_regular,
         direct_expense = labor_cost + cannibalization_cost + maintenance_cost,
         gross_profit = total_revenue - direct_expense,
         operating_income = gross_profit - depreciation_cost,
         net_income = operating_income * (1 - tax_rate), 
         cashflow = net_income + depreciation_cost - capex    
  )

# build individual scenarios
optimist <- mutate(assumptions, unit_sales_per_day = unit_sales_per_day * 1.2, pct_cannibalization = 0.1)
pessimist <- mutate(assumptions, unit_sales_per_day = unit_sales_per_day * 0.8, profit_margin_per_nitro = 1)

library(tidyverse)
library(purrr)
# combine into one dataset
scenarios <-
  bind_rows(
    mutate(pessimist, scenario = "pessimist"),
    mutate(assumptions, scenario = 'realist'),
    mutate(optimist, scenario = "optimist")
  )



calc_model<-function(assumptions){
  mutate( assumptions,
          sales_per_year = unit_sales_per_day * days_open_per_year,
          sales_revenue = sales_per_year * profit_margin_per_nitro,
          total_revenue = sales_revenue,
          labor_cost = days_open_per_year * 0.5 * labor_cost_per_hour,
          cannibalization_cost = sales_per_year * pct_cannibalization * profit_margin_per_regular,
          direct_expense = labor_cost + cannibalization_cost + maintenance_cost,
          gross_profit = total_revenue - direct_expense,
          operating_income = gross_profit - depreciation_cost,
          net_income = operating_income * (1 - 0.36), 
          cashflow = net_income + depreciation_cost - capex    
  )
}

calc_npv_from_cashflow<-function(cashflow, r){
  cashflow_line <- cashflow$cashflow
  sum(calc_pv(cashflow_line, r, 0:(length(cashflow_line)-1)))
}
# calculate scenario NPVs
scenario_analysis <-
  scenarios %>%
  nest(-scenario) %>%
  mutate(cashflow = map(data, calc_model)) %>%
  mutate(npv = map_dbl(cashflow, calc_npv_from_cashflow, 0.2))

# inspect results
select(scenario_analysis, scenario, npv)

# scenario analysis bar chart
ggplot(data = scenario_analysis, 
       aes(x = scenario, y = npv, fill = scenario)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "NPV Scenario Analysis of Nitro Coffee Expansion") +
  guides(fill = FALSE)

# define sensitivity factor function
factor_data <- function(data, metric, factor){
  data[[metric]] <- data[[metric]] * factor
  data
}

ggplot(sensitivity,
       aes(x = factor, y = npv, col = metric)
) +
  geom_line() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Sensivity Analysis",
    x = "Factor on Original Assumption",
    y = "Projected NPV",
    col = "Metric"
  )

# create sensitivity analysis
sensitivity <-
  expand.grid(
    factor = seq(0.5,1.5,0.1), 
    metric = c("profit_margin_per_nitro", "labor_cost_per_hour", "pct_cannibalization", "unit_sales_per_day")) %>%
  mutate(scenario = map2(metric, factor, ~factor_data(assumptions, .x, .y))) %>%
  mutate(cashflow = map(scenario, calc_model)) %>% 
  mutate(npv = map_dbl(cashflow, calc_npv_from_cashflow, r = 0.2))


# examine current cashflow strucutre
cashflow<-data.frame(Metric=c('Received','Spent'),'1'=c(100,150),'2'=c(200,175),'3'=c(300,200),'4'=c(400,225),'5'=c(500,250),'6'=c(500,250))
rownames(cashflow)<-c(1,2)

# load tidyr
library(tidyr)

# create long_cashflow with gather
long_cashflow <- gather(cashflow, key = Month, value = Value, -Metric)

# create tidy_cashflow with spread
tidy_cashflow <- spread(long_cashflow, key = Metric, value = Value)

# examine results
tidy_cashflow

# examine current cashflow strucutre
tidy_cashflow

# create long_cashflow with gather
long_cashflow <- gather(tidy_cashflow, key = Metric, value = Value, -Month)

# create untidy_cashflow with spread
untidy_cashflow <- spread(long_cashflow, key = Month, value = Value)

# examine results
untidy_cashflow

gross_profit_summary<-data.frame(metric=c('Sales Revenue','Keg Cost','Cannibalization Cost','Labor Cost','Maintenance Cost'),value=c(187200,-78240,-31200,-10000,-2500))
# compute min and maxes for each line item
# compute min and maxes for each line item
waterfall_items <-
  mutate(gross_profit_summary,
         end = cumsum(value), 
         start = lag(cumsum(value),1,default = 0))

# compute totals row for waterfall metrics
waterfall_summary <- 
  data.frame(metric = "Gross Profit", 
             end = sum(gross_profit_summary$value), 
             start = 0)

# combine line items with summary row
waterfall_data <-
  bind_rows(waterfall_items, waterfall_summary) %>%
  mutate(row_num = row_number())

# Plot waterfall diagram
ggplot(waterfall_data, aes(fill = (end > start))) +
  geom_rect(aes(xmin = row_num - 0.25, xmax = row_num + 0.25, 
                ymin = start, ymax = end)) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = waterfall_data$row_num, labels = waterfall_data$Metric) +
  # Styling provided for you - check out a ggplot course for more information!
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  guides(fill = FALSE) +
  labs(
    title = "Gross Profit for Proposed Nitro Coffee Expansion",
    subtitle = "Based on pro forma 10-year forecast")
