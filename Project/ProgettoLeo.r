#FRAQUELLI LEONARDO BISF 2018-2019

#Library
library(curl)
library(zoo)
library(tseries)
library(PerformanceAnalytics)
library(quantmod)
library(dygraphs)
#forecasting
library(forecast)

#Portfolio Management
library(tidyverse)
library(tidyquant)
library(PortfolioAnalytics)
library(plotly)
library(ggthemes)
library(timetk)
library(ROI)
#Should pf management not work: these packages MIGHT be missing
#library(ROI.plugin.glpk)
#library(ROI.plugin.quadprog)
#library(DEoptim)
#library(quadprog)
#library(rgplk)
#library(Rsymphony)


start_date <- '2009-01-01'
end_date<- '2019-01-01'
#####Chosen Stocks  #####


#Conversion to monthly data
PM_M<- as.xts(na.omit(get.hist.quote(instrument ="PM",start =start_date,
                                       end =end_date, quote ="AdjClose", provider ="yahoo", compression ="m", retclass ="zoo")))
UVV_M<- as.xts(na.omit(get.hist.quote(instrument ="UVV",start =start_date,
                            end =end_date, quote ="AdjClose", provider ="yahoo", compression ="m", retclass ="zoo")))
ATVI_M<- as.xts(na.omit(get.hist.quote(instrument ="ATVI",start =start_date,
                            end =end_date, quote ="AdjClose", provider ="yahoo", compression ="m", retclass ="zoo")))
EA_M<- as.xts(na.omit(get.hist.quote(instrument ="EA",start =start_date,
                            end =end_date, quote ="AdjClose", provider ="yahoo", compression ="m", retclass ="zoo")))
GOOG_M<- as.xts(na.omit(get.hist.quote(instrument ="GOOG",start =start_date,
                            end =end_date, quote ="AdjClose", provider ="yahoo", compression ="m", retclass ="zoo")))
AMZN_M<- as.xts(na.omit(get.hist.quote(instrument ="AMZN",start =start_date,
                            end =end_date, quote ="AdjClose", provider ="yahoo", compression ="m", retclass ="zoo")))
########Dataset#######

#Monthly Version
Dataset_M<-merge(UVV_M,ATVI_M,GOOG_M,EA_M,AMZN_M,PM_M)
colnames(Dataset_M)<-c("Universal","Activision","Google","EA","Amazon","Philip Morris")
dygraph(Dataset_M,main="Adjusted Close")

#The data values are  too wide: We scale them so that we can compare them more easily
Dataset_M_N<-scale(Dataset_M)
#Normalized Graph:
## Monthly
dygraph(Dataset_M_N) %>%
  dySeries(name="Universal", label = "Universal ADJUSTING CLOSING PRICES") %>%
  dySeries(name="Amazon", label = "Amazon ADJUSTING CLOSING PRICES") %>%
  dySeries(name="Activision", label = "Activision ADJUSTING CLOSING PRICES") %>%
  dySeries(name="EA", label = "EA ADJUSTING CLOSING PRICES") %>%
  dySeries(name="Google", label = "Google ADJUSTING CLOSING PRICES") %>%
  dySeries(name="Philip Morris", label = "Philip Morris ADJUSTING CLOSING PRICES") %>%
  dyOptions(stackedGraph = FALSE,drawPoints = TRUE,pointSize = 1.2,colors = c("#669966","#FFCC00","#0000FF","#003333","#FF9933","#666600")) %>%
  dyRangeSelector(height = 20)

####Calculate simple and compounded returns ####
#We use Performance Analytics
#Return.Calculate is more recent than CalculateReturns
# na.omit(CalculateReturns(Dataset_M, method="simple"))
simpleR<- Return.calculate(Dataset_M,"discrete")
compoundedR <- Return.calculate(Dataset_M,"log")
#Remove NA values
simpleR<- na.omit(simpleR)
compoundedR <- na.omit(compoundedR)

#####SimpleR for every asset:  ####
simpleR_Universal <- simpleR[,"Universal"]
simpleR_Amazon <- simpleR[,"Amazon"]
simpleR_EA <- simpleR[,"EA"]
simpleR_Activision <- simpleR[,"Activision"]
simpleR_Google <- simpleR[,"Google"]
simpleR_Philip <- simpleR[,"Philip Morris"]
#Compounded R for every asset
compounded_Universal <- compoundedR[,"Universal"]
compounded_Amazon <- compoundedR[,"Amazon"]
compounded_EA <- compoundedR[,"EA"]
compounded_Activision <- compoundedR[,"Activision"]
compounded_Google <- compoundedR[,"Google"]
compounded_PhilipMorris <- compoundedR[,"Philip Morris"]
#compounded_UniversalT<-diff(log(UVV_M))
#Plots: unreadable
dygraph(simpleR)%>%
  dyRangeSelector(height = 20)
dygraph(compoundedR)%>%
  dyRangeSelector(height = 20)

###########################Plots of Stocks in similar sectors #####
#GOOG-AMZN#
dygraph(cbind(compounded_Google,compounded_Amazon),main="Google-Amazon")%>%
  dyOptions(colors = c("blue","red"),stackedGraph = FALSE)%>%
  dyRangeSelector(height = 20)
#EA-ATVI#
dygraph(cbind(compounded_EA,compounded_Activision),main="EA-Activision")%>%
  dyOptions(colors = c("blue","red"),stackedGraph = FALSE)%>%
  dyRangeSelector(height = 20)
#PM-UVV#
dygraph(cbind(compounded_PhilipMorris,compounded_Universal),main="PhilipMorris-Universal")%>%
  dyOptions(colors = c("blue","red"),stackedGraph = FALSE)%>%
  dyRangeSelector(height = 20)

#
par(mfrow=c(3,2)) #We put a matrix to lay the Histograms in
#Histograms
hist(compounded_Universal,freq=FALSE,main="Distribuzione dei Return di Universal",ylim=c(0,10))
points(density(compounded_Universal),type = "S", col="black")
#
hist(compounded_PhilipMorris,freq=FALSE,main="Distribuzione dei Return di PhilipMorris",ylim=c(0,10))
points(density(compounded_PhilipMorris),type = "S", col="blue")
#
hist(compounded_Amazon,freq=FALSE,main="Distribuzione dei Return di Amazon", ylim=c(0,8))
points(density(compounded_Amazon),type = "S", col="orange")
#
hist(compounded_Google,freq=FALSE,main="Distribuzione dei Return di Google")
points(density(compounded_Google),type = "S", col="green")
#
hist(compounded_Activision,freq=FALSE,main="Distribuzione dei Return di Activision")
points(density(compounded_Activision),type = "S", col="blue")
#
hist(compounded_EA,freq=FALSE,main="Distribuzione dei Return di EA")
points(density(compounded_EA),type = "S", col="red")

#######################Creating multiple plots to analyse single stocks #####
#UVV

#chart.boxplot is buggy
#Grid of plotsZ
par(mfrow=c(2,2))
#Histogram
hist(compounded_Universal,probability = TRUE,main=NULL)
#Smoothed Density
#hist(compounded_Universal,probability=TRUE,ylim = c(0,10))
#Density plots show the distribution shape in a better way than histograms do
plot(density(compounded_Universal),type="l",col="black",main="")
#QQPlot
qqnorm(compounded_Universal,main=NULL,col="blue")
qqline(compounded_Universal)
#Boxplot
boxplot(coredata(compounded_Universal),outlier.symbol = "O",main = NULL)

#PM
#Grid of plots
par(mfrow=c(2,2))
#Histogram
hist(compounded_PhilipMorris,probability = TRUE,main="")
#Smoothed Density
#hist(compounded_Universal,probability=TRUE,ylim = c(0,10))
#Density plots show the distribution shape in a better way than histograms do
plot(density(compounded_PhilipMorris),type="S",col="black",main="")
#QQPlot
qqnorm(compounded_PhilipMorris,main=NULL,col="blue")
qqline(compounded_PhilipMorris)
#Boxplot
boxplot(coredata(compounded_PhilipMorris),outlier.symbol = "O",main = NULL)
#GOOG
#Grid of plots
par(mfrow=c(2,2))
#Histogram
hist(compounded_Google,probability = TRUE,main="")
#Smoothed Density
#hist(compounded_Universal,probability=TRUE,ylim = c(0,10))
#Density plots show the distribution shape in a better way than histograms do
plot(density(compounded_Google),type="S",col="black",main="")
#QQPlot
qqnorm(compounded_Google,main=NULL,col="blue")
qqline(compounded_Google)
#Boxplot
boxplot(coredata(compounded_Google),outlier.symbol = "O",main =NULL)
#repeat

#ATVI
#Grid of plots
par(mfrow=c(2,2))
#Histogram?
hist(compounded_Activision,probability = TRUE,main="")
#Smoothed Density
#hist(compounded_Universal,probability=TRUE,ylim = c(0,10))
#Density plots show the distribution shape in a better way than histograms do
plot(density(compounded_Activision),type="S",col="black",main="")
#QQPlot
qqnorm(compounded_Activision,main=NULL,col="blue")
qqline(compounded_Activision)
#Boxplot
boxplot(coredata(compounded_Activision),outlier.symbol = "O",main =NULL)

#AMZN
#Grid of plots
par(mfrow=c(2,2))
#Histogram
hist(compounded_Amazon,probability = TRUE,main="")
#Smoothed Density
#hist(compounded_Universal,probability=TRUE,ylim = c(0,10))
#Density plots show the distribution shape in a better way than histograms do
plot(density(compounded_Amazon),type="S",col="black",main="")
#QQPlot
qqnorm(compounded_Amazon,main=NULL,col="blue")
qqline(compounded_Amazon)
#Boxplot
boxplot(coredata(compounded_Amazon),outlier.symbol = "O",main =NULL)

#EA
#Grid of plots
par(mfrow=c(2,2))
#Histogram
hist(compounded_EA,probability = TRUE,main="")
#Smoothed Density
#hist(compounded_Universal,probability=TRUE,ylim = c(0,10))
#Density plots show the distribution shape in a better way than histograms do
plot(density(compounded_EA),type="S",col="black",main="")
#QQPlot
qqnorm(compounded_EA,main=NULL,col="blue")
qqline(compounded_EA)
#Boxplot
boxplot(coredata(compounded_EA),outlier.symbol = "O",main =NULL)

###################### Statistical Indexes ##############

StatsPrint<-function(datasets){
  
  for(i in 1:length(colnames(datasets))){
    print("Statistical Indexes of:")
    print(colnames(datasets)[i])
    print("*******")
    print("Mean:")
    print(mean((datasets)[,i]))
    print("*******")
    print("Variance:")
    print(var((datasets)[,i]))
    print("*******")
    print("Standard Deviation:")
    print(sd((datasets)[,i]))
    print("*******")
    print("Skewness:")
    print(skewness((datasets)[,i]))
    print("*******")
    print("Kurtosis:")
    print(kurtosis((datasets)[,i]))
    print("*******")
    print("Quantile:")
    print(quantile((datasets)[,i]))
    print("_________________________________________")
  }
}

StatsPrint(compoundedR)

#How much two stocks are related
Covariance<-cov(cbind(compounded_Universal,compounded_Amazon,compounded_Google,compounded_Activision,compounded_EA,compounded_PhilipMorris))
#How the variation of the first value affects the variation of the second
Correlation<-cor(cbind(compounded_Universal,compounded_Amazon,compounded_Google,compounded_Activision,compounded_EA,compounded_PhilipMorris))

#plot(compoundedR,type = "p")
#Correlation Matrix
my_cols <- c("red","green3") 

#We are able to use the lower panel to represent the correlation vlaues throguh these functions
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r+0.5)
}
#We distinguish between the values of the two stocks
upper<-function(x,y){
  points(x,y,pch=19, col=my_cols)
}
compounded_Google_Num<-as.numeric(compounded_Google)
compounded_Universal_Num<-as.numeric(compounded_Universal)
compounded_Amazon_Num<-as.numeric(compounded_Amazon)
compounded_Activision_Num<-as.numeric(compounded_Activision)
compounded_Ea_Num<-as.numeric(compounded_EA)
compounded_PhilipMorris_Num<-as.numeric(compounded_PhilipMorris)
#Correlation Matrix
pairs(cbind(compounded_Universal_Num,compounded_Amazon_Num,compounded_Google_Num,compounded_Activision_Num,compounded_Ea_Num,compounded_PhilipMorris_Num),
      main="Correlation Matrix",lower.panel=panel.cor,upper.panel=upper,labels=c("UVV","AMZN","GOOG","ATVI","EA","PM"))   

#### BETA #####

#Maybe ADD NASDAQ S&P500? 

#Spy(NYSE)Market Index
#It's an S&P ETF
SPY<-getSymbols("SPY",from=start_date,to=end_date,src='yahoo',auto.assign = FALSE)
SPY<-to.monthly(SPY)
SPY<-na.omit(diff(log(SPY$SPY.Adjusted)))
colnames(SPY)<-c("SPY")

#Nasdaq Market
NASDAQ<-getSymbols("^IXIC",from=start_date,to=end_date,src='yahoo',auto.assign = FALSE)
NASDAQ<-to.monthly(NASDAQ)
NASDAQ<-na.omit(diff(log(NASDAQ$NASDAQ.Adjusted)))
colnames(NASDAQ)<-c("NASDAQ")

#We create a function to calculate the beta of every stock compared to the SPY / NASDAQ
#x=> Stock index, y=> Market index
beta_calc<-function(x,y){
  betaVal=cov(x,y)/var(y)
  return (betaVal)
}


EA.xts<- NULL
ATVI.xts<- NULL
PM.xts<- NULL
UVV.xts<- NULL
AMZN.xts<- NULL
GOOG.xts<- NULL

delta_t <- 20 # Beta value calculated in this window
length_period = dim(SPY)[1] #

start <- delta_t+1 # We calculate beta after delta_t months

for (i in start:length_period){
  beta_value_EA <- beta_calc(compounded_EA[(i-delta_t):(i-1)], NASDAQ[(i-delta_t):(i-1)])
  beta_value_ATVI <- beta_calc(compounded_Activision[(i-delta_t):(i-1)], NASDAQ[(i-delta_t):(i-1)])
  beta_value_UVV <- beta_calc(compounded_Universal[(i-delta_t):(i-1)], SPY[(i-delta_t):(i-1)])
  beta_value_GOOG <- beta_calc(compounded_Google[(i-delta_t):(i-1)], NASDAQ[(i-delta_t):(i-1)])
  beta_value_AMZN <- beta_calc(compounded_Amazon[(i-delta_t):(i-1)], NASDAQ[(i-delta_t):(i-1)])
  beta_value_PM <- beta_calc(compounded_PhilipMorris[(i-delta_t):(i-1)], SPY[(i-delta_t):(i-1)])
  #  Beta Ts creation
  beta_EA <- as.xts(beta_value_EA, order.by = index(compounded_EA[(i-1)]))
  beta_ATVI <- as.xts(beta_value_ATVI, order.by = index(compounded_Activision[(i-1)]))
  beta_UVV <- as.xts(beta_value_UVV, order.by = index(compounded_Universal[(i-1)]))
  beta_GOOG <- as.xts(beta_value_GOOG, order.by = index(compounded_Universal[(i-1)]))
  beta_AMZN<- as.xts(beta_value_AMZN, order.by = index(compounded_Amazon[(i-1)]))
  beta_PM <- as.xts(beta_value_PM, order.by = index(compounded_PhilipMorris[(i-1)]))
  if(is.null(EA.xts)){
    EA.xts <- beta_EA
    ATVI.xts <- beta_ATVI
    UVV.xts <- beta_UVV
    GOOG.xts <- beta_GOOG
    AMZN.xts <- beta_AMZN
    PM.xts <- beta_PM
  }else{
    EA.xts <- rbind(EA.xts,beta_EA)
    ATVI.xts <- rbind(ATVI.xts,beta_ATVI)
    UVV.xts <- rbind(UVV.xts,beta_UVV)
    GOOG.xts <- rbind(GOOG.xts,beta_GOOG)
    AMZN.xts <- rbind(AMZN.xts,beta_AMZN)
    PM.xts <- rbind(PM.xts,beta_PM)
  }
  # Printing values in the given time window
  print('time windows:')
  print(paste("Start of the time window:", index(compounded_Ea_Num)[i-delta_t]))
  print(paste("End of the time window:  ", index(compounded_Ea_Num)[i-1]))
  print('beta values:')
  print(paste("Time index: ", index(compounded_Ea_Num)[i]))
  print(paste("EA beta:", beta_value_EA))
  print(paste("ATVI beta:", beta_value_ATVI))
  print(paste("UVV beta:", beta_value_UVV))
  print(paste("GOOG beta:", beta_value_GOOG))
  print(paste("AMZN beta:", beta_value_AMZN))
  print(paste("PM beta:", beta_value_PM))
}

#Plotting Betas And Returns for the market
#EA
par(mfrow=c(2,1))
plot(NASDAQ,type="l", main="Nasdaq")
plot(EA.xts,type="l",main="Beta EA")
#ATVI
par(mfrow=c(2,1))
plot(NASDAQ,type="l", main="Nasdaq")
plot(ATVI.xts,type="l",main="Beta ATVI")
#GOOG
par(mfrow=c(2,1))
plot(NASDAQ,type="l", main="Nasdaq")
plot(GOOG.xts,type="l",main="Beta GOOG")
#AMZN
par(mfrow=c(2,1))
plot(SPY,type="l", main="Nasdaq")
plot(AMZN.xts,type="l",main="Beta AMZN")
#PM
par(mfrow=c(2,1))
plot(SPY,type="l", main="SPY")
plot(PM.xts,type="l",main="Beta PM")
#UVV
par(mfrow=c(2,1))
plot(SPY,type="l", main="SPY")
plot(UVV.xts,type="l",main="Beta UVV")

#### Forecasting ####

##Forecasting function##
 par(mfrow=c(1,1))
forecasting<-function(stockName,arimaMethod){
  
  end_f="2019-01-01"
  start_f="2009-01-01"
  
  dataset <- get.hist.quote( instrument=stockName, start=start_f ,end=end_f,quote="AdjClose", 
                             provider="yahoo", origin="1970-01-01", compression="month")
  
  index(dataset)<- as.yearmon(index(dataset)) #The format date index neeeds to change format
  colnames(dataset)<- stockName
  #We now decompose the ts in its 4 components
  fitVal <- stl(dataset[,1], s.window = "periodic")
  mainName<-paste("Decomposition of",stockName)
  plot(fitVal,main=mainName)
  #Compounded Returns
  returns <- diff(log(dataset[,1]))
  #Dataset Training for Forecasting
  returnsTrain <- returns[1:(0.8*length(returns))]#60% of returns is used for training
  returnsTest <- returns[(0.8*length(returns)+1):(0.9*length(returns))]#The remainder 60-80% is used for testing
  returnsValidation<-returns[(0.9*length(returns)+1):length(returns)]#Then 80%-100% is used for validating
  #ar,ma are the first values we attemp to use in our arima function
  #max is the threshold for mistakes
  ar=0;
  ma=0;
  max=100;
  for (p in 1:6){
    for(f in 1:6){
      fit <- arima(returnsTrain, order = c(p, 0, f),method=arimaMethod) #without ML the UVV stock didn't work
      arma.predictions <- predict(fit, n.ahead = (length(returns) - (0.8 * length(returns))))$pred
      s=accuracy(arma.predictions, returnsTest)[2]
      if(s<max){
        max=s;
        ar=p;
        ma=f;
      }
    }
  }
  #chosen values
  ar
  ma
  
  fit <- arima(returnsTrain, order = c(ar, 0, ma))
  arma.predictions <- predict(fit, n.ahead = (length(returns) - (0.8 * length(returns))))$pred
  arma.forecast <- forecast(fit, h = (length(returns)-length(returnsTrain)),level = c(95,80))
  #arma.forecast
  mainName<- paste("ARMA forecast for returns of", stockName)
  plot(arma.forecast, main = mainName)
  #We don't want all the accuracy values
  #accuracy(arma.predictions, returnsTest) 
  #Accuracy compared to 60-80%
  accuracy(arma.predictions, returnsTest)[2]
  #Accuracy compared to last 10%
  accuracy(arma.predictions, returnsValidation)[2]
  lines(returns)
  lines(returnsValidation, col = "red")
  
  
}

##END FUNCTION##


forecasting("EA","ML")
#few warnings
forecasting("ATVI","ML")
#many warnings
#forecasting("ATVI","CSS")
#few warnings
forecasting("GOOG","ML")
#many warnings
#forecasting("GOOG","CSS")
forecasting("AMZN","ML")
#forecasting("AMZN","CSS")
#few warnings
forecasting("PM","ML")
#many warnings
#forecasting("PM","CSS")
#many warnings
#forecasting("UVV","CSS")
forecasting("UVV","ML")

#Transaction Costs

#### Portfolio Management ####


start_date <- '2009-01-01'
end_date<- '2019-01-01'

Symbols=c("AMZN","GOOG","EA","ATVI","PM","UVV")
#Adjusted_price <- merge.zoo(SWX[,6], SPY[,6], ICUI[,6], MSFT[,6])

# Get stock pairs
# I discovered tidyquant as an alternative to get.hist.quote
# Downloads a tibble, which is just another type of data.frame
# It is also nicer to print compared to DF's
stock_prices2 <- Symbols %>%
  tq_get(get  = "stock.prices",
         from = start_date,
         to   = end_date) %>%
  group_by(symbol) 
stock_prices2%>%head()
# Calculate Returns
stock_returns <- stock_prices2 %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "arithmetic",
               col_rename = "returns") %>%
  spread(key = symbol, value = returns)
stock_returns%>%head()

stock_returns.quarterly <- stock_prices2 %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "quarterly",
               type       = "arithmetic",
               col_rename = "returns") %>%
  spread(key = symbol, value = returns)

#convert to xts
stock_returns=stock_returns%>%tk_xts()
#stock_returns%>%tk_zoo(),tk_ts(),tk_xts()
stock_returns%>%head()

stock_returns.quarterly=stock_returns.quarterly%>%tk_xts()
stock_returns.quarterly%>%head()
# Save mean return vector and sample covariance matrix
returns.portfolio<-na.omit(stock_returns)


meanReturns <- colMeans(returns.portfolio)
covMat <- cov(returns.portfolio)

# Get a character vector of the fund names
fund.names <- colnames(stock_returns)

# Specify a portfolio object by passing a character vector for the
# assets argument.

p <- portfolio.spec(assets=fund.names)
#The default weights without using any constraints
print.default(p)

rp.seq <- generatesequence(min = 0, max = 1, by = 0.002)
p <- portfolio.spec(assets = fund.names, 
                    weight_seq = rp.seq)
p <- add.constraint(portfolio = p, type = 'weight_sum', 
                    min_sum = 0.99, max_sum = 1.01)
#p <- add.constraint(portfolio = p, type = 'box', min = 0, max = 1)
p <- add.constraint(portfolio = p, type = 'box', 
                    min = 0.05, max = 0.50, indexnum = 2)
p <- add.objective(portfolio = p, type = 'return', name = 'mean', 
                   multiplier = 0)
p <- add.objective(portfolio = p, type = 'risk', name = 'StdDev')

#transaction costs = 1%
#p <- add.constraint(portfolio=p, type="transaction_cost", ptc=0.01)


#optimize the xts with the portfolio
#optimum<-optimize.portfolio(R=returns.portfolio,portfolio = p, optimize_method = "ROI",trace=TRUE)
#optimumBal<-optimize.portfolio.rebalancing(R=returns.portfolio,portfolio=p, optimize_method="ROI",rebalance_on ="quarters",training_period = 60, rolling_window = 60 )
#print(optimum)

# generate the random portfolios
rp <- random_portfolios(portfolio = p, permutations = length(rp.seq), 
                        method = 'sample')
rp <- scale(rp)


# run the optimizations
opt.base <- optimize.portfolio(R=tail(returns.portfolio, 36), portfolio = p, 
                               rp = rp, optimize_method = 'ROI',
                               trace = TRUE)


opt.base.rebal <- optimize.portfolio.rebalancing(R = returns.portfolio, portfolio = p,
                                                 optimize_method = 'ROI',
                                                 rp = rp, trace = TRUE,
                                                 rebalance_on = 'quarters',
                                                 training_period = 36,
                                                 rolling_window = 36)
#opt.base.rebal.r <- Return.portfolio(returns.portfolio, weights = extractWeights(opt.base.rebal))
colnames(opt.base.rebal.r) <- 'base'

chart.Weights(opt.base.rebal)

x=0 #counter
val=1000 #initial $
plotVal=c() #usedForPlot
dateVal=c() #usedForPlot
for(i in opt.base.rebal$opt_rebalancing){
  #print("Weights")
  #print(extractWeights(i))
  y=1 #stocksCounter
  Rtot=0 #total monthly return
  #print(stock_returns.quarterly[x])
  for(j in stock_returns.quarterly[x]){
    Rtemp=extractWeights(i)[y]*j #extract the portfolio's weights to calculate the return
    y=y+1
    # print("Rtemp")
    #  print(Rtemp)
    Rtot=Rtot+Rtemp #quarterly return
  }
  x=x+1
  val=val+(val*as.numeric(Rtot))
  val=val- (1/100)*val #transaction cost = 1%
  currentQuarter<-date(stock_returns.quarterly[x])
  #Used for the Budget's growth
  plotVal<-c(plotVal,val)
  dateVal<-c(dateVal,currentQuarter)
}

#formula for portfolio Value:
#Val(t) = Val*(1+portfolioReturns*weights)

dates<-as.Date(dateVal)
valsOverTime<-data.frame(dates,plotVal)
plot((valsOverTime),type="l",col="black",main="Budget's growth")
print(val)

