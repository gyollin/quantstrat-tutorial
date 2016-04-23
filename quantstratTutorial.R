## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2016 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#########################################################################

## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(tidy=FALSE,cache=FALSE,size='scriptsize',
  fig.path='figures/',fig.show='hide',fig.keep='last',
  fig.align='center',	fig.width=7,	fig.height=5,
  message=FALSE,warning=FALSE)

## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=81,continue=" ",digits=8)

## ----eval=FALSE----------------------------------------------------------
## #
## # install these packages from CRAN (or r-forge)
## #
## install.packages("xts")
## install.packages("PerformanceAnalytics")
## install.packages("quantmod")
## install.packages("TTR")
## #
## # Install these package from r-forge
## #
## install.packages("FinancialInstrument", repos = "http://R-Forge.R-project.org")
## install.packages("blotter", repos = "http://R-Forge.R-project.org")
## install.packages("quantstrat", repos = "http://R-Forge.R-project.org")

## ----eval=FALSE----------------------------------------------------------
## getSymbols(Symbols = NULL, env = parent.frame(), src = "yahoo",
##   auto.assign = getOption('getSymbols.auto.assign',TRUE), ...)

## ----eval=FALSE----------------------------------------------------------
## getSymbols.yahoo(Symbols, env, return.class = 'xts', index.class  = 'Date',
##   from = "2007-01-01", to = Sys.Date(), ...)

## ----cache=FALSE,size='tiny'---------------------------------------------
library(quantmod)
ls()

## ----results='hide',size='tiny'------------------------------------------
getSymbols("^GSPC")

## ----size='tiny'---------------------------------------------------------
ls()
class(GSPC)
class(index(GSPC))
dim(GSPC)

## ----echo=FALSE----------------------------------------------------------
options(width=120)

## ----size='tiny'---------------------------------------------------------
tail(GSPC,4)
tail(Cl(GSPC),4)
tail(Ad(GSPC),4)

## ----echo=FALSE----------------------------------------------------------
options(width=81)

## ----eval=FALSE----------------------------------------------------------
## chartSeries(x, type = c("auto", "candlesticks", "matchsticks",
##     "bars", "line"), subset = NULL, show.grid = TRUE, name = NULL,
##     time.scale = NULL, log.scale = FALSE, TA = "addVo()", TAsep = ";",
##     line.type = "l", bar.type = "ohlc", theme = chartTheme("black"),
##     layout = NA, major.ticks = "auto", minor.ticks = TRUE, yrange = NULL,
##     plot = TRUE, up.col, dn.col, color.vol = TRUE, multi.col = FALSE, ...)

## ----GSPC1,cache=FALSE---------------------------------------------------
chartSeries(GSPC,subset="2015",theme="white")

## ----GSPC0,cache=FALSE,size='tiny'---------------------------------------
whiteTheme <- chartTheme("white")
names(whiteTheme)
whiteTheme$bg.col <- "white"
whiteTheme$dn.col <- "pink"
whiteTheme$up.col <- "lightgreen"
whiteTheme$border <- "lightgray"
x <- chartSeries(GSPC,subset="last 3 months",theme=whiteTheme,TA=NULL)
class(x)

## ----size='tiny'---------------------------------------------------------
myStr <- "7/4/2014"
class(myStr)
args(getS3method("as.Date","character"))
myDate <- as.Date(myStr,format="%m/%d/%Y")
myDate
class(myDate)
as.numeric(myDate)

## ------------------------------------------------------------------------
format(myDate,"%m/%d/%Y")
format(myDate,"%m/%d/%y")
format(myDate,"%Y%m%d")

## ----size='tiny'---------------------------------------------------------
d <- Sys.time()
class(d)
unclass(d)
sapply(unclass(as.POSIXlt(d)), function(x) x)

## ----GSPC2,cache=FALSE---------------------------------------------------
chartSeries(GSPC["2015"],theme=whiteTheme,name="S&P 500")

## ----GSPC3,cache=FALSE---------------------------------------------------
chartSeries(GSPC["2015/2016"],theme=whiteTheme,name="S&P 500")

## ----GSPC4,cache=FALSE---------------------------------------------------
chartSeries(GSPC["2015-10::2016-03"],theme=whiteTheme,name="S&P 500")

## ----GSPC5,cache=FALSE---------------------------------------------------
chartSeries(GSPC["201506::"],theme=whiteTheme,name="S&P 500")

## ------------------------------------------------------------------------
library(PerformanceAnalytics)
library(quantmod)
library(lattice)
startDate <- '2010-01-01'  # start of data
endDate <-  '2015-05-01'   # end of data
Sys.setenv(TZ="UTC")       # set time zone
symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")

## ----echo=FALSE----------------------------------------------------------
if(file.exists("XLX.RData"))
{
  load("XLX.RData")
} else {
  getSymbols(symbols, from=startDate, to=endDate, index.class="POSIXct")
  for(symbol in symbols) {
      x<-get(symbol)
      x<-adjustOHLC(x,symbol.name=symbol)
      x<-to.weekly(x,indexAt='lastof',drop.time=TRUE)
      indexFormat(x)<-'%Y-%m-%d'
      colnames(x)<-gsub("x",symbol,colnames(x))
      assign(symbol,x)
  }
  save(list=symbols,file="XLX.RData")
}

## ----eval=FALSE----------------------------------------------------------
##   getSymbols(symbols, from=startDate, to=endDate, index.class="POSIXct")
##   for(symbol in symbols) {
##       x<-get(symbol)
##       x<-adjustOHLC(x,symbol.name=symbol)
##       x<-to.weekly(x,indexAt='lastof',drop.time=TRUE)
##       indexFormat(x)<-'%Y-%m-%d'
##       colnames(x)<-gsub("x",symbol,colnames(x))
##       assign(symbol,x)
##   }

## ----plotETF,echo=TRUE, fig.keep='all', tidy=FALSE-----------------------
prices <- NULL
for(i in 1:length(symbols))
  prices <- cbind(prices,Cl(get(symbols[i])))
colnames(prices) <- symbols
returns <- diff(log(prices))[-1, ]
num.ass <- ncol(returns)

xyplot(prices, xlab = "", layout = c(3, 3),type=c("l","g"))

stacked.df <- stack(as.data.frame(returns))
colnames(stacked.df) <- c("returns", "symbol")

densityplot(~returns | symbol, stacked.df, cex = 0.25, xlab="",type=c("l","g"))

## ----XLFBB,cache=FALSE---------------------------------------------------
args(BBands)
b <- BBands(HLC=HLC(XLF["2013"]), n=20, sd=2)
tail(b)
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chart_Series(XLF,TA='add_BBands(lwd=2)',theme=myTheme,name="XLF")

## ----results='hide'------------------------------------------------------
library(quantstrat)
initDate <- '2009-12-31'
initEq <- 1e6
currency("USD")
stock(symbols, currency="USD", multiplier=1)

## ----results='hide'------------------------------------------------------
rm.strat("multiAsset.bb1") # remove portfolio, account, orderbook if re-run
initPortf(name="multiAsset.bb1", symbols, initDate=initDate)
initAcct(name="multiAsset.bb1", portfolios="multiAsset.bb1",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multiAsset.bb1", initDate=initDate)

## ----results='hide'------------------------------------------------------
strategy("bbands", store=TRUE)

## ------------------------------------------------------------------------
args(add.indicator)

## ------------------------------------------------------------------------
args(BBands)

## ----results='hide'------------------------------------------------------
add.indicator("bbands", name = "BBands",
  arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'), label='bbInd')

## ------------------------------------------------------------------------
args(add.signal)

## ----results='hide'------------------------------------------------------
add.signal("bbands", name="sigCrossover",
  arguments=list(columns=c("High","up"),relationship="gt"),
  label="H.gt.UpperBand")

## ----results='hide'------------------------------------------------------
add.signal("bbands", name="sigCrossover",
  arguments=list(columns=c("Low","dn"),relationship="lt"),
  label="L.lt.LowerBand")

## ------------------------------------------------------------------------
args(add.rule)

## ------------------------------------------------------------------------
args(ruleSignal)

## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="H.gt.UpperBand",sigval=TRUE,
    orderqty=+100, ordertype='market', orderside='long'),
  type='enter',
  label='LongEntry')

## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="L.lt.LowerBand",sigval=TRUE,
    orderqty= 'all', ordertype='market', orderside='long'),
  type='exit',
  label='LongExit')

## ------------------------------------------------------------------------
args(applyStrategy)

## ----results='hide'------------------------------------------------------
nSD = 2
nMA = 20

## ----results='hide'------------------------------------------------------
out <- applyStrategy("bbands",
  portfolios="multiAsset.bb1",parameters=list(sd=nSD,n=nMA))

## ----echo=F--------------------------------------------------------------
options(width=120)

## ----size='tiny'---------------------------------------------------------
getTxns(Portfolio="multiAsset.bb1", Symbol="XLK")

## ----echo=F--------------------------------------------------------------
options(width=81)

## ----echo=FALSE----------------------------------------------------------
options(width=180,digits=6)

## ----size='Tiny'---------------------------------------------------------
mktdata["2015"]

## ----echo=FALSE----------------------------------------------------------
options(width=81,digits=8)

## ----results='hide'------------------------------------------------------
updatePortf("multiAsset.bb1")
updateAcct("multiAsset.bb1")
updateEndEq("multiAsset.bb1")

## ----size='Tiny'---------------------------------------------------------
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
    text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  if( !isTRUE(all.equal(port.tot,port.sum.tot)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match sum of symbols P&L")
  }
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in account summary")
  }
  return(ok)
}
checkBlotterUpdate("multiAsset.bb1","multiAsset.bb1")

## ----CHARTPOSNPANEL,fig.width=24,fig.height=18---------------------------
par(mfrow=c(3,3))
for(symbol in symbols)
{
chart.Posn(Portfolio="multiAsset.bb1",Symbol=symbol,theme=myTheme,
	TA="add_BBands(n=20,sd=2)")
}
par(mfrow=c(1,1))

## ----XLBCP,cache=FALSE---------------------------------------------------
chart.Posn("multiAsset.bb1","XLU",TA="add_BBands(n=20,sd=2)",theme=myTheme)

## ----MULTITRADESTATS,echo=TRUE,fig.width=12,fig.height=9-----------------
textplot(t(tradeStats("multiAsset.bb1")))

## ----echo=FALSE----------------------------------------------------------
options(width=78)

## ----IASSRET,cache=FALSE-------------------------------------------------
rets.multi <- PortfReturns("multiAsset.bb1")
colnames(rets.multi) <- sort(symbols)
round(tail(rets.multi,5),6)
chart.CumReturns(rets.multi, colorset= rich10equal, legend.loc = "topleft",
  main="SPDR Cumulative Returns",minor.ticks=FALSE)

## ----echo=FALSE----------------------------------------------------------
options(width=81)

## ------------------------------------------------------------------------
args(ruleSignal)

## ----results='hide'------------------------------------------------------
enable.rule("bbands",type="enter",label="LongEntry",enabled=FALSE)

## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="H.gt.UpperBand",sigval=TRUE,
    orderqty=+100, ordertype='market', orderside='long',
    osFUN='osMaxPos'),
  type='enter',
  label='LimitedLongEntry')

## ------------------------------------------------------------------------
args(addPosLimit)

## ----results='hide'------------------------------------------------------
rm.strat("multi.bb.limit") # remove portfolio, account, orderbook if re-run
initPortf(name="multi.bb.limit", symbols, initDate=initDate)
initAcct(name="multi.bb.limit", portfolios="multi.bb.limit",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.bb.limit", initDate=initDate)

## ------------------------------------------------------------------------
for(symbol in symbols)
{
  addPosLimit("multi.bb.limit", symbol, initDate, 100, 1 )
}

## ----results='hide'------------------------------------------------------
out <- applyStrategy("bbands",
  portfolios="multi.bb.limit",parameters=list(sd=2,n=20))

## ----results='hide'------------------------------------------------------
updatePortf("multi.bb.limit")
updateAcct("multi.bb.limit")
updateEndEq("multi.bb.limit")

## ------------------------------------------------------------------------
checkBlotterUpdate("multi.bb.limit","multi.bb.limit")

## ----XLBCPLIM,cache=FALSE------------------------------------------------
chart.Posn("multi.bb.limit","XLU",TA="add_BBands(n=20,sd=2)",theme=myTheme)

## ------------------------------------------------------------------------
args(osNoOp)

## ----results='hide'------------------------------------------------------
osFixedDollar <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...)
{
  pos <- getPosQty(portfolio, symbol, timestamp)
  if( isTRUE(all.equal(pos,0)) )
  {
    ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
    orderqty <- sign(orderqty)*round(tradeSize/ClosePrice,-2)
  } else {
    orderqty <- 0
  }
  return(orderqty)
}

## ----results='hide'------------------------------------------------------
enable.rule("bbands",type="enter",label="LimitedLongEntry",enabled=FALSE)

## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="H.gt.UpperBand",sigval=TRUE,
    orderqty=+100, ordertype='market', orderside='long',
    osFUN='osFixedDollar'),
  type='enter',
  label='FixedLongEntry')

## ----results='hide'------------------------------------------------------
rm.strat("fixed.dollar") # remove portfolio, account, orderbook if re-run
initPortf(name="fixed.dollar", symbols, initDate=initDate)
initAcct(name="fixed.dollar", portfolios="fixed.dollar",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="fixed.dollar", initDate=initDate)

## ----results='hide'------------------------------------------------------
tradeSize <- 100000
out <- applyStrategy("bbands",
  portfolios="fixed.dollar",parameters=list(sd=2,n=20))

## ----results='hide'------------------------------------------------------
updatePortf("fixed.dollar")
updateAcct("fixed.dollar")
updateEndEq("fixed.dollar")

## ------------------------------------------------------------------------
checkBlotterUpdate("fixed.dollar","fixed.dollar")

## ----echo=FALSE----------------------------------------------------------
options(width=105)

## ----size='tiny'---------------------------------------------------------
perTradeStats("fixed.dollar","XLF")

## ----echo=FALSE----------------------------------------------------------
options(width=82)

## ----results='hide'------------------------------------------------------
strategy("bbands", store=TRUE)

## ----results='hide'------------------------------------------------------
add.indicator("bbands", name = "BBands",
  arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'), label='bbInd')

## ----results='hide'------------------------------------------------------
add.signal("bbands", name="sigCrossover",
  arguments=list(columns=c("High","up"),relationship="gt"),
  label="H.gt.UpperBand")

## ----results='hide'------------------------------------------------------
add.signal("bbands", name="sigCrossover",
  arguments=list(columns=c("Low","dn"),relationship="lt"),
  label="L.lt.LowerBand")

## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="H.gt.UpperBand",sigval=TRUE,
    orderqty=+100,
    ordertype='market',
    orderside='long',
    osFUN='osFixedDollar',
    orderset='ocolong'),
  type='enter',
  label='LongEntry')

## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="L.lt.LowerBand",sigval=TRUE,
    orderqty= 'all',
    ordertype='market',
    orderside='long',
    orderset='ocolong'),
  type='exit',
  label='LongExit')

## ------------------------------------------------------------------------
stopLossPercent <- 0.03

## ----results='hide'------------------------------------------------------
add.rule("bbands",name='ruleSignal',
  arguments = list(sigcol="H.gt.UpperBand", sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='stoplimit',
    tmult=TRUE,
    threshold=quote( stopLossPercent ),
    orderqty='all',
    orderset='ocolong'
  ),
  type='chain', parent="LongEntry",
  label='StopLossLong'
)

## ------------------------------------------------------------------------
trailingStopPercent <- 0.07

## ----results='hide'------------------------------------------------------
add.rule("bbands", name = 'ruleSignal',
  arguments=list(sigcol="H.gt.UpperBand" , sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='stoptrailing',
    tmult=TRUE,
    threshold=quote(trailingStopPercent),
    orderqty='all',
    orderset='ocolong'
  ),
  type='chain', parent="LongEntry",
  label='StopLossTrailing'
)

## ----results='hide'------------------------------------------------------
rm.strat("bb.stop") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="bb.stop", symbols, initDate=initDate)
initAcct(name="bb.stop", portfolios="bb.stop",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="bb.stop", initDate=initDate)

## ----results='hide',echo=FALSE-------------------------------------------
enable.rule("bbands",type="chain",label="StopLoss")

## ----results='hide'------------------------------------------------------
tradeSize <- 100000
out<-applyStrategy("bbands" , portfolios="bb.stop",
  parameters=list(sd=2,n=20))

## ----results='hide'------------------------------------------------------
updatePortf("bb.stop")
updateAcct("bb.stop")
updateEndEq("bb.stop")

## ------------------------------------------------------------------------
checkBlotterUpdate("bb.stop","bb.stop")

## ------------------------------------------------------------------------
library(parallel)
detectCores()

## ----results='hide'------------------------------------------------------
if( Sys.info()['sysname'] == "Windows" )
{
  library(doParallel)
  registerDoParallel(cores=detectCores())
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}

## ------------------------------------------------------------------------
foreach(i=1:8, .combine=c) %dopar% sqrt(i)

## ------------------------------------------------------------------------
args(add.distribution)

## ------------------------------------------------------------------------
stopLossPercentRange <- seq(0.01,0.10,by=0.01)

## ----results='hide'------------------------------------------------------
add.distribution("bbands",
  paramset.label = "STOPOPT",
  component.type = "chain",
  component.label = "StopLossLong",
  variable = list( threshold = stopLossPercentRange ),
  label = "StopLossLongDist"
)

## ------------------------------------------------------------------------
trailingPercentRange <- seq(0.01,0.10,by=0.01)

## ----results='hide'------------------------------------------------------
add.distribution("bbands",
  paramset.label = "STOPOPT",
  component.type = "chain",
  component.label = "StopLossTrailing",
  variable = list( threshold = trailingPercentRange ),
  label = "StopLossTrailingDist"
)

## ------------------------------------------------------------------------
args(add.distribution.constraint)

## ----results='hide'------------------------------------------------------
add.distribution.constraint("bbands",
	paramset.label = 'STOPOPT',
	distribution.label.1 = 'StopLossLongDist',
	distribution.label.2 = 'StopLossTrailingDist',
	operator = '<',
	label = 'StopCon'
)

## ----results='hide'------------------------------------------------------
rm.strat("bb.opt") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="bb.opt", symbols, initDate=initDate)
initAcct(name="bb.opt", portfolios="bb.opt",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="bb.opt", initDate=initDate)

## ------------------------------------------------------------------------
args(apply.paramset)

## ----results='hide'------------------------------------------------------
if( Sys.info()['sysname'] == "Windows" )
{
  library(doParallel)
#  registerDoParallel(cores=detectCores())
  registerDoSEQ()
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}

## ----echo=FALSE, results='hide'------------------------------------------
if( file.exists("resultsStopOpt.RData") )
{
  load("resultsStopOpt.RData")
} else {
  results <- apply.paramset("bbands", paramset.label = "STOPOPT",
    portfolio="bb.opt", account="bb.opt", nsamples=0)
  save(list="results",file="resultsStopOpt.RData")
}

## ----eval=FALSE----------------------------------------------------------
## results <- apply.paramset("bbands", paramset.label = "STOPOPT",
##   portfolio="bb.opt", account="bb.opt", nsamples=0)

## ------------------------------------------------------------------------
names(results)

## ----PROFITMDDHEAT,fig.width=7,fig.height=7,dev='png',dpi=300------------
z <- tapply(X=results$tradeStats$Profit.To.Max.Draw,
  INDEX=list(results$tradeStats$StopLossTrailingDist,results$tradeStats$StopLossLongDist),
  FUN=median)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

filled.contour(x=x,y=y,z=z,color = heat.colors,
  xlab="Trailing Stop",ylab="Stop Loss")
title("Return to MaxDrawdown")

