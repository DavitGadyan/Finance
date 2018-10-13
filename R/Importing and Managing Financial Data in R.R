# Load the quantmod package
library(quantmod)

# Import QQQ data from Yahoo! Finance
getSymbols('QQQ',auto.assign=TRUE)

# Look at the structure of the object getSymbols created
str(QQQ)

# Look at the first few rows of QQQ
head(QQQ)

# Import QQQ data from Alpha Vantage
getSymbols('QQQ',src='av',auto.assign=TRUE)

# Look at the structure of QQQ
str(QQQ)

# Import GDP data from FRED
getSymbols('GDP',src='FRED',auto.assign=TRUE)

# Look at the structure of GDP
str(GDP)

# Assign SPY data to 'spy' using auto.assign argument
spy<-getSymbols('SPY',auto.assign=FALSE)

# Look at the structure of the 'spy' object
str(spy)

# Assign JNJ data to 'jnj' using env argument
jnj<-getSymbols('JNJ',env=NULL)

# Look at the structure of the 'jnj' object
str(jnj)

# Load the Quandl package
library(Quandl)

# Import GDP data from FRED
gdp<-Quandl(code='FRED/GDP')

# Look at the structure of the object returned by Quandl
str(gdp)

# Import GDP data from FRED as xts
gdp_xts<-Quandl('FRED/GDP',type='xts')

# Look at the structure of gdp_xts
str(gdp_xts)

# Import GDP data from FRED as zoo
gdp_zoo<-Quandl('FRED/GDP',type='zoo')

# Look at the structure of gdp_zoo
str(gdp_zoo)

## Example
dj<-getSymbols('^DJI',auto.assign=FALSE)


# Create an object containing the Pfizer ticker symbol
symbol='PFE'

# Use getSymbols to import the data
getSymbols(symbol,auto.assign=TRUE)

# Look at the first few rows of data
head(PFE)

### OANDA Foreign Exchange

# quantmod::oanda.currencies contains a list of currencies provided by Oanda.com.
quantmod::oanda.currencies

# Create a currency_pair object
currency_pair <- "GBP/CAD"

# Load British Pound to Canadian Dollar exchange rate data
getSymbols(currency_pair,auto.assign=TRUE,src='oanda')

# Examine object using str()
str(GBPCAD)

# Try to load data from 190 days ago
getSymbols(currency_pair, from = Sys.Date() - 190, to = Sys.Date(), src = "oanda")


# Create a series_name object
series_name='UNRATE'

# Load the data using getSymbols
getSymbols(series_name,src='FRED')

# Create a quandl_code object
quandl_code='FRED/UNRATE'

# Load the data using Quandl
unemploy_rate<-Quandl(quandl_code)

# Extract the close column
qqq_close<-Cl(QQQ)

# Look at the head of dc_close
head(qqq_close)

# Extract the volume column
qqq_volume<-Vo(QQQ)

# Look at the head of dc_volume
head(qqq_volume)

# Extract the high, low, and close columns
qqq_hlc<-HLC(QQQ)

# Look at the head of dc_hlc
head(qqq_hlc)

# Extract the open, high, low, close, and volume columns
qqq_ohlcv<-OHLCV(QQQ)

# Look at the head of dc_ohlcv
head(qqq_ohlcv)

# Download CME data for CL and BZ as an xts object
oil_data <- Quandl(code = c("CME/CLH2016", "CME/BZH2016"), type = "xts")

# Look at the column names of the oil_data object
colnames(oil_data)

# Extract the Open price for CLH2016
cl_open <- getPrice(oil_data, symbol = "CLH2016", prefer = "Open$")

# Look at January, 2016 using xts' ISO-8601 subsetting
cl_open["2016-01"]

quandl_codes<-c('CME/CLH2016','CME/BZH2016')


# Download quarterly CL and BZ prices
qtr_price<-Quandl(quandl_codes,collapse='quarterly',type='xts')

# View the high prices for both series
Hi(qtr_price)

# Download quarterly CL and BZ returns
qtr_return<-Quandl(quandl_codes,collapse='quarterly',transform='rdiff',type='xts')

# View the settle price returns for both series
getPrice(qtr_return,prefer='Settle')


# Create new environment
data_env <- new.env()
# Use getSymbols to load data into the environment
getSymbols(c("SPY", "QQQ"), env = data_env, auto.assign = TRUE)

# Look at a few rows of the SPY data
head(data_env$SPY)

# Extract volume column from each object
adjusted_list <- lapply(data_env, Ad)
# Merge each list element into one object
adjusted <- do.call(merge, adjusted_list)
head(adjusted)


#### or

# Call head on each object in data_env using eapply
data_list<-eapply(data_env,head)

# Merge all the list elements into one xts object
data_merged<-do.call(merge,data_list)

# Ensure the columns are ordered: open, high, low, close
data_ohlc<-OHLC(data_merged)

data_ohlc


# Symbols
symbols <- c("AAPL", "MSFT", "IBM")

# Create new environment
data_env<-new.env()

# Load symbols into data_env
getSymbols(symbols,env=data_env,auto.assign=TRUE)

# Extract the close column from each object and combine into one xts object
close_data <- do.call(merge, eapply(data_env, Cl))

# View the head of close_data
head(close_data)

# Call head on each object in data_env using eapply
data_list<-eapply(data_env,head)

# Merge all the list elements into one xts object
data_merged<-do.call(merge,data_list)

# Ensure the columns are ordered: open, high, low, close
data_ohlc<-OHLC(data_merged)

head(data_ohlc)

# Set the default to pull data from Alpha Vantage
setDefaults(getSymbols,src='av')

# Get GOOG data
getSymbols('GOOG',api.key='VK450TMTMRB4JR9C')

# Verify the data was actually pulled from Alpha Vantage
str(GOOG)

args(getSymbols)


# Look at getSymbols.yahoo arguments
args(getSymbols.yahoo)

# Set default 'from' value for getSymbols.yahoo
setDefaults(getSymbols.yahoo, from = "2000-01-01")

# Confirm defaults were set correctly
getDefaults('getSymbols.yahoo')

# Load BRK-A data
getSymbols("BRK-A",api.key='VK450TMTMRB4JR9C')

# Use backticks and head() to look at the loaded data
head(`BRK-A`)

# Use get() to assign the BRK-A data to an object named BRK.A
BRK.A<-get('BRK-A')

# Create BRK.A object
BRK.A<-getSymbols("BRK-A",auto.assign=FALSE,api.key='VK450TMTMRB4JR9C')

# Create col_names object with the column names of BRK.A
col_names<-colnames(BRK.A)

# Set BRK.A column names to syntactically valid names
colnames(BRK.A) <- make.names(col_names)

# Set name for BRK-A to BRK.A
setSymbolLookup(BRK.A = list(name = "BRK-A"))

# Set name for T (AT&T) to ATT
setSymbolLookup(ATT = list(name = "T"))

# Load BRK.A and ATT data
getSymbols(c("BRK.A", "ATT"),api.key='VK450TMTMRB4JR9C')