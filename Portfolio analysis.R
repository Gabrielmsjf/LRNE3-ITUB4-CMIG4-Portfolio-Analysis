library(xts)
library(quantmod)
library(Amelia)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(tidyverse)
library(lubridate)

###########################Get the Data#######################################################################
Itau <- read.csv("ITUB4 Historical Data.csv")
IBOV <- read.csv("Bovespa Historical Data.csv")
Renner <- read.csv("LREN3 Historical Data.csv")
Cemig <- read.csv("CMIG4 Historical Data.csv")

#########################Clean data to transform to a xts object to work with R packages###########################

DATA_i <- Itau$Date
DATA_r <- Renner$Date
DATA_c <- Cemig$Date
DATA_I <- IBOV$Date
DATA_i <- gsub(",", "", DATA_i)
DATA_r <- gsub(",", "", DATA_r)
DATA_c <- gsub(",", "", DATA_c)
DATA_I <- gsub(",", "", DATA_I)

DATA_i <- as.POSIXct(DATA_i, format = "%h %d %Y")
DATA_r <- as.POSIXct(DATA_r, format = "%h %d %Y")
DATA_c <- as.POSIXct(DATA_c, format = "%h %d %Y")
DATA_I <- as.POSIXct(DATA_I, format = "%h %d %Y")

Itau <- Itau[ ,c(1, 3, 4, 5, 2, 6, 7)]
names(Itau) <- c("Date", "Open", "High", "Low", "Itau.Close", "Vol.", "Return")
Renner <- Renner[ ,c(1, 3, 4, 5, 2, 6, 7)]
names(Renner) <- c("Date", "Open", "High", "Low", "Renner.Close", "Vol.", "Return")
Cemig <- Cemig[ ,c(1, 3, 4, 5, 2, 6, 7)]
names(Cemig) <- c("Date", "Open", "High", "Low", "Cemig.Close", "Vol.", "Return")
IBOV <- IBOV[ ,c(1, 3, 4, 5, 2, 6, 7)]
names(IBOV) <- c("Date", "Open", "High", "Low", "Close", "Vol.", "Return")

IBOV[ ,2:5] <- lapply(IBOV[,2:5], as.character)
IBOV[ ,2:5] <- lapply(IBOV[,2:5], function(X) gsub(",", "", X))
IBOV[ ,2:5] <- lapply(IBOV[,2:5], as.numeric)


#######################Transforming data into xts object################################################
Itau_x <- xts(Itau[ ,2:5], order.by = DATA_i)
Renner_x <- xts(Renner[ ,2:5], order.by = DATA_r)
Cemig_x <- xts(Cemig[ ,2:5], order.by = DATA_c)
IBOV_x <- xts(IBOV[ ,2:5], order.by = DATA_I)

###########################Get Returns and build the portfolio returns##################################
###########################Giving arbitrary weights to protfolio########################################
Ibov_returns <- na.omit(ROC(IBOV_x[, 4]))

Itau_rets <- na.omit(ROC(Itau_x[,4]))
Renner_rets <- na.omit(ROC(Renner_x[,4]))
Cemig_rets <- na.omit(ROC(Cemig_x[,4]))

Portfolio_Returns <- cbind(Renner_rets, Cemig_rets, Itau_rets)["2006::"]
rename(Portfolio_Returns, c("Renner", "Cemig", "Itau"), c("close", "close.1", "close.2"))
weights <- c( .25, .50, .25)
Portfolio_return <- Return.portfolio(Portfolio_Returns, weights = weights)

charts.PerformanceSummary(Portfolio_return, main = "Portfolio Performance")

######################### Plot returns and compare them ######################################

charts.PerformanceSummary(Portfolio_return["2006::"], main = "Portfolio Performance"
charts.PerformanceSummary(Ibov_returns["2006::"], main = "IBOV Performance")
Port_Ibov_rets <- merge(Ibov_returns["2006::"], Portfolio_return["2006::"])
names(Port_Ibov_rets) <- c("Ibovespa", "Portfolio")

chart.CumReturns(Port_Ibov_rets, legend.loc = "topleft")

#########################Get the Portfolio ratios ################################################
CAPM.beta(Portfolio_return["2006::"], Ibov_returns, Rf = .055/252)
CAPM.jensenAlpha(Portfolio_return["2006::"], Ibov_returns["2006::"], Rf = .055/252)
SharpeRatio(Portfolio_return["2006::"])
table.AnnualizedReturns(Portfolio_return["2006::"], Rf = .055/252)
table.CalendarReturns(Portfolio_return["2006::"])
SharpeRatio(Portfolio_return["2006::"], Rf = .055/252)

################################ Portfolio Optimizations###############################################

port1 <- portfolio.spec(colnames(Portfolio_Returns))

port1 <- add.constraint(port1, type = "weight_sum", min_sum = 1, max_sum = 1)
port1 <- add.constraint(port1, type = "box", min = .05, max = .35)
port1 <- add.objective(port1, type = "return", name = "mean")
port1 <- add.objective(port1, type = "risk", name = "StdDev")

optport1 <- optimize.portfolio(Portfolio_Returns, portfolio = port1, optimize_method = "ROI", trace = TRUE)
optport1
chart.Weights(optport1)
chart.EfficientFrontier(optport1,
                        match.col = "StdDev", n.portfolios = 20, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "Risk Adjusted", rf = .055/252, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 1)

########################## Visualization Optimum Portfolio based on Return ############################

Optweights <- c(.35, .30, .35)
Portfolio_return_opt <- Return.portfolio(Portfolio_Returns, Optweights)
charts.PerformanceSummary(Portfolio_return_opt, main = "Optimum Portfolio")

########## Create a data frame to compare Ibovespa, the Arbitrary Portfolio and Optimal Portfolio ##################
Optport_port_Ibov <- merge(Ibov_returns["2006::"], Portfolio_return["2006::"], Portfolio_return_opt["2006::"])
names(Optport_port_Ibov) <- c("Optimum Portfolio", "Portfolio", "Optimum Portfolio")

chart.CumReturns(Optport_port_Ibov, legend.loc = "topleft")


######################## Build a rebalancing Portfolio and compare all of then with IBOV ############## 

port_reb <- portfolio.spec(colnames(Portfolio_Returns))

port_reb <- add.constraint(port_reb, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
port_reb <- add.constraint(port_reb, type = "box", min = .05, max = .60)
port_reb <- add.constraint(port_reb, type = "transaction_cost", ptc = .001)
port_reb <- add.objective(port_reb, type = "return", name = "mean")
port_reb <- add.objective(port_reb, type = "risk", name = "StdDev", target = 0.005)

rp <- random_portfolios(port_reb, permutations = 10000, rp_method = "sample")

opt_port_reb <- optimize.portfolio.rebalancing(Portfolio_Returns, portfolio = port_reb, 
                                               optimize_method = "random", rp = rp, 
                                               rebalance_on = "months", training_period = 1, 
                                               rolling_window = 10)

  
opt_port_reb
chart.Weights(object = opt_port_reb, main = "Rebanced Portfolio")

#############################Creating benchmark comparisson portfolios##################################
######################Get weights from rebalanced portfolio and optimized portfolio########################

rebal_weights <- extractWeights(opt_port_reb)
rebal_returns <- Return.portfolio(Portfolio_Returns, rebal_weights)

Opt_weights <- extractWeights(optport1)
opt1_returns <- Return.portfolio(Portfolio_Returns, Opt_weights)

#########################Gather all Returns in one data frame and compare them all###########################
rets_portfolios <- cbind(rebal_returns, opt1_returns, Portfolio_return, Ibov_returns)["2006::"]
names(rets_portfolios) <- c("Rebal_portfolio", "Optimal_Portfolio", "Portfolio", "Ibovespa")
charts.PerformanceSummary(rets_portfolios, main = "Returns over time")
charts.PerformanceSummary(rebal_returns)
charts.PerformanceSummary(opt1_returns)
charts.PerformanceSummary(Portfolio_return)
charts.PerformanceSummary(Ibov_returns)


