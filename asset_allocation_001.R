con=gzcon(file('sit.gz','rb') )
source(con)
close(con)

symbols <- spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
symbol.names = spl('S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year
Treasury,U.S. Real Estate,Gold')

if (exists('SPY')){
    cat('Data already exists so will use it\n')
    load('./data/asset_allocation_001.RData')
} else {
    cat('Getting data from Yahoo:\n')
    getSymbols(symbols, from = '1980-01-01', auto.assign=TRUE)
    save(SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD, file = "./data/asset_allocation_001.RData") 
}

hist.prices <- merge(SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD)

month.ends <- endpoints(hist.prices, 'months')
hist.prices <- Cl(hist.prices)[month.ends, ]
colnames(hist.prices) <- symbols
hist.prices <- na.omit( hist.prices['1995:2014'])

hist.returns <- na.omit( ROC(hist.prices, type = 'discrete'))

ana <- list()
ana$expected.return <- apply( hist.returns, 2, mean, na.rm = TRUE)
ana$risk <- apply( hist.returns, 2, sd, na.rm = TRUE)
ana$correlation <- cor(hist.returns, use = 'complete.obs', method = 'pearson')

ana$symbols <- symbols
ana$symbol.names <- symbols
ana$hist.returns <- hist.returns
ana$n <- len(symbols)
# convert to annual
annual.factor <- 12

ana$expected.return <- annual.factor * ana$expected.return
ana$risk <- sqrt(annual.factor) * ana$risk

ana$risk <- iif(ana$risk == 0, 1e-6, ana$risk)
ana$cov <-  ana$correlation * ( ana$risk %*% t(ana$risk) )

# plotting
#plot.ia(ana) # this plot.ia is not wokring
layout(1)
par(mar = c(4,4,2,1), cex = 0.8)
x <- 100 * ana$risk
y <- 100 * ana$expected.return

plot(x, y, xlim = range(c(0,x)), ylim = range(c(0,y)),
     xlab = 'Risk', ylab = 'Return', main = 'Risk vs Return', col = 'black')
grid()
text(x, y, symbols, col = 'blue', adj = c(1,1), cex = 0.8 )

n <- ana$n
constraints <- new.constraints(n, lb = 0, ub = 0.8)
constraints <- add.constraints(rep(1,n), 1, type = '=', constraints )
ef <- portopt( ana, constraints, 50, 'Efficient Portfolio Frontier')

print(ef)
plot.ef( ana, list(ef)) # not working




