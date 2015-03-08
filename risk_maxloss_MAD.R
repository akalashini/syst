# minimize the max-loss portfolio

MinMaxLossPortfolio <- function
(
  inp,
  constraints
) {
  # max-loss defined as max_1<j<T { - sum(r_ij * xi) }
  # i: the ith asset
  # j: the jth return scenario
  # convert to linear programming
  # min w
  # - sum_i=1,...,n r_ij * xi <= w, j=1,...T
  #
  # inp:
  #     n
  #     hist.returns: nXT matrix, total T scenarios
  #     
  n  <- inp$n
  nt <- nrow(inp$hist.returns)
  
  f.obj <- c(rep(0, n), 1)
  
  # constraint matirx
  # constraint for sum r_ij*xi + w >= 0
  cons.mat <- cbind( matrix(0, nt, n), 1)
  cons.mat[, 1:n] <- inp$hist.returns
  cons.dir <- rep(">=", nt)
  cons.rhs <- rep(0, nt)
  
  # also the sum of weights = 1
  wtConst <- c(rep(1, n), 0)
  cons.mat <- rbind(cons.mat, wtConst)
  cons.dir <- c(cons.dir, "=")
  cons.rhs <- c(cons.rhs, 1.)
  
  # note that 0 <= x <= 1
  
  sol <- optimize.LP( "min",          
                   f.obj,        
                   cons.mat,            
                   cons.dir,           
                   cons.rhs,   
                   lb     = 0,         
                   lb.vec = 1:n,
                   ub     = 1 )
  return(sol)
}

#MinMADPortfolio <- function
#(
#  
#){
#  
#}

sampledata <- function(){
  # tst run
  require(quantmod)
  
  symbols      <- spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
   
  load('./data/asset_allocation_001.RData')   
  hist.prices  <- merge(SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD)
  
  month.ends   <- endpoints(hist.prices, 'months')
  hist.prices  <- Cl(hist.prices)[month.ends, ]
  colnames(hist.prices) <- symbols
  hist.prices  <- na.omit( hist.prices['1995:2014'])
  
  hist.returns <- na.omit( ROC(hist.prices, type = 'discrete'))
  
  inp   <- list()
  inp$n <- len(symbols)
  inp$hist.returns    <- hist.returns
  inp$expected.return <- apply(hist.returns, 2, mean, na.rm = TRUE)
  inp$risk <- apply(hist.returns, 2, sd, na.rm = TRUE)
  
  inp$expected.return <- 12 * inp$expected.return
  inp$risk <- sqrt(12) * inp$risk
  
  #sol <- MinMaxLossPortfolio( inp )
  return(inp)
  
}
