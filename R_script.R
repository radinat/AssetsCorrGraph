
# Load libraries ----
library(readxl)
library(quantmod)
library(igraph)
library(dplyr)
library(lubridate)
library(shiny)
library(imputeTS)
library(tidyverse)

setwd("//Users//radinatalanova//Downloads")
# # Import the available list of tickers ----
equity.tickers = read_excel("List_of_assets.xlsx", sheet = 1)
fixed.income.tickers = read_excel("List_of_assets.xlsx", sheet = 2)
commodities.tickers = read_excel("List_of_assets.xlsx", sheet = 3)
#
# # Create Environments
equity = list()
equity$tickers = equity.tickers$Ticker
equity$env = new.env()

fixed_income = list()
fixed_income$tickers = fixed.income.tickers$Ticker
fixed_income$env = new.env()

commodities = list()
commodities$tickers = commodities.tickers$Ticker
commodities$env = new.env()

# Download historical information ----
# For stocks:
getSymbols(
    equity$tickers,
    env = equity$env,
    from = "2006-01-01",
    "getSymbols.warning4.0" = FALSE
)
rm(.getSymbols, envir = equity$env)
save(list = ls(equity$env),
     file = 'equity.RData',
     envir = equity$env)

# There is no data for ticker AGN in yahoo finance. Removed it from Excel
# list of assets, because it brings an error when we load the data.

# For fixed income ETFs:
getSymbols(
    fixed_income$tickers,
    env = fixed_income$env,
    from = "2006-01-01",
    "getSymbols.warning4.0" = FALSE
)
rm(.getSymbols, envir = fixed_income$env)
save(
    list = ls(fixed_income$env),
    file = 'fixed_income.RData',
    envir = fixed_income$env
)

# For commodity ETFs:
getSymbols(
    commodities$tickers,
    env = commodities$env,
    from = "2006-01-01",
    "getSymbols.warning4.0" = FALSE
)
rm(.getSymbols, envir = commodities$env)
save(
    list = ls(commodities$env),
    file = 'commodities.RData',
    envir = commodities$env
)


# # Load data (not downloading it every time) ----
#load("equity.RData", envir=equity$env)
#load('fixed_income.RData', envir=fixed_income$env)
#load('commodities.RData', envir=commodities$env)

# Create data frames with adjusted prices ----
eq = lapply(names(equity$env), get, envir = equity$env)
fi = lapply(names(fixed_income$env), get, envir = fixed_income$env)
com = lapply(names(commodities$env), get, envir = commodities$env)

# Equity data frame
eq_dd = list()
for (i in 1:length(eq)) {
    eq_dd[[i]] = eq[[i]][, 6]
}
eq_dd = do.call(merge, eq_dd)
colnames(eq_dd) = gsub(".Adjusted", "", colnames(eq_dd))
eq_dd = as.data.frame(eq_dd)

# Fixed income ETFs data frame
fi_dd = list()
for (i in 1:length(fi)) {
    fi_dd[[i]] = fi[[i]][, 6]
}
fi_dd = do.call(merge, fi_dd)
colnames(fi_dd) = gsub(".Adjusted", "", colnames(fi_dd))
fi_dd = as.data.frame(fi_dd)

# Commodities data frame
com_dd = list()
for (i in 1:length(com)) {
    com_dd[[i]] = com[[i]][, 6]
}
com_dd = do.call(merge, com_dd)
colnames(com_dd) = gsub(".Adjusted", "", colnames(com_dd))
com_dd = as.data.frame(com_dd)

rm(eq, fi, com, i)
rm(commodities.tickers, equity.tickers, fixed.income.tickers)

# Bind data frames
all_dd = cbind(eq_dd, fi_dd, com_dd)

all_dd$row_names = rownames(all_dd)
all_dd$date = as.Date(all_dd$row_names)
all_dd = all_dd[,!(names(all_dd) %in% c("row_names"))] # remove row_names column

rownames(all_dd) = all_dd$date

testdates = all_dd
all_dd$date = as.Date(all_dd$date)
all_dd = all_dd %>%
    complete(date = seq(min(all_dd$date), max(all_dd$date), by = "day"))

# #inpute missing dates
testdates$date = as.Date(testdates$date)
testdates = testdates %>%
    complete(date = seq(min(testdates$date), max(testdates$date), by = "day"))
# missing values interpolation
testdates <- na_interpolation(testdates, option = "linear")
testdates = as.data.frame(testdates)
rownames(testdates) = as.character(testdates$date)
#remove date from the set
final = testdates[, !names(testdates) %in% c("date")]
rownames(final) = rownames(testdates)

# Perform rolling sample estimation of the correlation matrix ---
date = ymd(rownames(final))
rdate = date[-1]

# Function for calculation of returns
rets = function(x) {
    lx = dplyr::lag(x)
    r = (x - lx) / lx
    return (r[-1])
}

rr = as.data.frame(sapply(final, rets))
rownames(rr) = rdate


n = 120 # length of the sliding window
test_cc = list()
suppressWarnings(for (i in 1:(nrow(rr) - n + 1)) {
    test_cc[[i]] = cor(rr[i:(i + n - 1), ], use = "pairwise.complete.ob")
})
names(test_cc) = as.character(date[n:nrow(rr)])
