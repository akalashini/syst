con=gzcon(file('sit.gz','rb') )
source(con)
close(con)

load.packages('quantmod,car')
url <- 'http://finance.yahoo.com/q/cp?s=^DJI+Components'
txt <- join(readLines(url))
#temp <- extract.table.from.webpage(txt,'Symbol', hasHeader=T)
#Symbols <- temp[,'Symbol']
#the extrat is not working anymore, need to manually overwrite
#Symbols <- spl('AXP,BA,CSCO,IBM,GS,GE,KO,MCD,NKE,VZ,V,XOM,T,JPM,WMT')
Symbols <- spl('CSCO,IBM,GS,GE,T,JPM,WMT')
cat(Symbols)

#to store grades
up.down.stats <- matrix( NA, nrow = len(Symbols), ncol = 3 )
rownames(up.down.stats) <- Symbols
colnames(up.down.stats) <- spl('N,Return,Risk')

#get upgrades and downgrades from yahoo
for( Symbol in Symbols ) {
    cat('downloading', Symbol, '\n')
    url <- paste('http://finance.yahoo.com/q/ud?s=', Symbol, sep='')
    txt <- join( readLines(url) )
    temp <- extract.table.from.webpage(txt, 'Research Firm', hasHeader=T )
    
    #find number of grades between 2010:2011
    event.year <- format( as.Date(temp[,'Date'],'%d-%b-%y' ), '%Y' )
    up.down.stats[Symbol,'N'] <- sum( event.year =='2010' | event.year=='2011')
    
    data <- getSymbols(Symbol, from='1980-01-01', auto.assign=FALSE)
    returns <- ROC(Cl(data['2010::2011']), type='discrete')
    
    returns <- na.omit(returns)
    cat('returns for ', Symbol, ':\n')
    #cat(returns)
    
    up.down.stats[Symbol,'Return'] <- 252 * mean( returns )
    up.down.stats[Symbol,'Risk']   <- sqrt(252) * sd(returns)
}

#look data
up.down.stats <- up.down.stats[ order(up.down.stats[, 'N']), , drop=FALSE ]
up.down.stats[, spl('Return,Risk')] <- round(100 * up.down.stats[, spl('Return,Risk')])

#plot table
plot.table( up.down.stats )
par(mar=c(5,5,5,5), cex=0.8)
barplot( up.down.stats[, 'N'], 
         xlab = 'Symbol',
         ylab = '# of Upgrades & Downgrades in 2010-2011',
         main = '# of Upgrades & Downgrades',
         names.arg = rownames( up.down.stats ),
         las  = 2 )


# run linear regression
layout(c(1,1,2,2))

for( measure in spl('Risk,Return') ) {
    x <- up.down.stats[, 'N']
    y <- up.down.stats[,  measure]
    
    fit <- lm( y ~ x )
    print( summary(fit))
    
    par(mar = c(5,4,2,1))
    plot(x, y, 
         xlab = '# of upgrades and downgrades',
         ylab = paste(measure, ' Measure'),
         main = paste( sprintf('%.2f', cor(x,y)), ' between ', measure, ' and # events'),
         )
    
    grid()
    text(x, y, rownames(up.down.stats), col = 'blue', adj = c(1,2), cex = 0.8)
    abline(coef = coef(fit), lty = 2)
    
    d <- dataEllipse(x, y, levels = c(0.5), draw = FALSE)
    lines(d, col = 'red', lty = 3)
}
