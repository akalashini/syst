# minimize the max-loss portfolio

MinMaxLossPortfolio <- function
(
  inp,
  constraints = NULL
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
  
  #constr <- create.new.constraints(n+1, cons.mat, cons.dir, cons.rhs, 
  #                                 lb = 0, lb.vec = seq(1,n),
  #                                 ub = 1, ub.vec = seq(1,n) )
  #cons.mat <- inp$hist.returns
  #cons.dir <- rep(">=", nt)
  #cons.rhs <- rep(0, nt)
  
  # also the sum of weights = 1
  #wtConst <- rep(1, n)
  #constr  <- add.constraints( wtConst, "=", 1, constraints )
  constr <- constraints
  
  # constraint matirx
  # constraint for sum r_ij*xi + w >= 0
  cons.mat <- cbind( matrix(0, nt, n), 1)
  cons.mat[, 1:n] <- inp$hist.returns
  cons.dir <- rep(">=", nt)
  cons.rhs <- rep(0, nt)
  
  constr  <- add.variables( 1, constr )
  constr  <- add.constraints( cons.mat, cons.dir, cons.rhs, constr )
  constr$lb <- 0
  constr$lb.vec <- seq(1,n)
  constr$ub <- 1
  constr$ub.vec <- seq(1,n)
   
  sol <- optimize.LP.wrap( "min",
                           f.obj,
                           constr )
#   sol <- optimize.LP( "min",          
#                    f.obj,        
#                    cons.mat,            
#                    cons.dir,           
#                    cons.rhs,   
#                    lb     = 0,         
#                    lb.vec = 1:n,
#                    ub     = 1 )
  sol$Risk   <- sol$Sol[n+1]
  sol$Sol <- sol$Sol[1:n]
  sol$Return <- sum(sol$Sol * inp$expected.return )
  
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
  
  symbols      <- splstr('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
   
  load('./data/asset_allocation_001.RData')   
  hist.prices  <- merge(SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD)
  
  month.ends   <- endpoints(hist.prices, 'months')
  hist.prices  <- Cl(hist.prices)[month.ends, ]
  colnames(hist.prices) <- symbols
  hist.prices  <- na.omit( hist.prices['1995::2010'])
  
  hist.returns <- na.omit( ROC(hist.prices, type = 'discrete'))
  
  inp   <- list()
  inp$n <- length(symbols)
  inp$hist.returns    <- hist.returns
  inp$expected.return <- apply(hist.returns, 2, mean, na.rm = TRUE)
  inp$risk <- apply(hist.returns, 2, sd, na.rm = TRUE)
  
  inp$expected.return <- 12 * inp$expected.return
  inp$risk <- sqrt(12) * inp$risk
  
  inp$corr <- cor(inp$hist.returns, use = 'complete.obs', method = 'pearson')
  inp$cov  <- inp$corr * (inp$risk %*% t(inp$risk))
  #sol <- MinMaxLossPortfolio( inp )
  return(inp)
  
}
