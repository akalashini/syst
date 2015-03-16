# minimize the max-loss portfolio

portfolio.risk.maxloss <- function
(
  weight,
  inp
)
{
  weight <- weight[, 1:inp$n, drop=F]
  portfolio.returns <- weight %*% t(inp$hist.returns)
  return( -apply(portfolio.returns, 1, min) )
}



## minimize max loss portfolio
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
 
sampledata <- function( local = T ){
  # tst run
  require(quantmod)
  if(local)
  {
    symbols      <- splstr('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
    load('./data/asset_allocation_001.RData')       
    hist.prices  <- merge(SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD)
  } else {
    symbols      <- splstr('SPY,QQQ,JPM,IBM,MSFT,FORD,CSCO,GLD')
    getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)
    hist.prices  <- merge(SPY,QQQ,JPM,IBM,MSFT,FORD,CSCO,GLD)
  }
  month.ends   <- endpoints(hist.prices, 'months')
  hist.prices  <- Cl(hist.prices)[month.ends, ]
  colnames(hist.prices) <- symbols
  hist.prices  <- na.omit( hist.prices['1995::2010'])
  
  hist.returns <- na.omit( ROC(hist.prices, type = 'discrete'))
  
  inp   <- list()
  inp$n <- length(symbols)
  inp$symbols <- symbols
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


###############################################################################
#### portfolio’s Mean-Absolute Deviation (MAD)
###############################################################################
## 1/T sum^T_j=1 |  sum^n_i=1 r_ij*x_i - 1/T sum^T_k=1 sum^n_i=1 r_ik * x_i |
## refer to paper:
## Comparative Analysis of Linear Portfolio Rebalancing Strategies: 
##    An Application to Hedge Funds by Krokhmal, P., S. Uryasev, and G. Zrazhevsky (2001)
portfolio.risk.mad <- function
(
  weight,
  inp
)
{
  if(is.null(dim(weight))) dim(weight) = c(1, length(weight))
  weight <- weight[, 1:inp$n, drop=F]
  portfolio.returns <- weight %*% t(inp$hist.returns)
  return( apply(portfolio.returns, 1, function(x) mean(abs(x - mean(x))) ) )
}

## minimize MAD portfolio


## minimize max loss portfolio
MinMADPortfolio <- function
(
  inp,
  constraints = NULL
) {
  # min MAD  
  # i: the ith asset
  # j: the jth return scenario
  # convert to linear programming
  # see my notes on MAD
  #
  # inp:
  #     n
  #     hist.returns: nXT matrix, total T scenarios
  #     
  n  <- inp$n
  nt <- nrow(inp$hist.returns)
  
  f.obj <- c(rep(0, n), (1/nt) * rep(1, 2 * nt) )
   
  constr <- constraints
   
  # add u+ and u- dummy variables: u+, u- > 0
  constr  <- add.variables(2 * nt, constr, lb = 0)
  # see note for the compact format of constraints
  # source("./utils.R") to use repmat
  cons.mat <- cbind( matrix(0, nt, n), -diag(nt), diag(nt) )
  cons.mat[, 1:n] <- inp$hist.returns - repmat(colMeans(inp$hist.returns), nt, 1 )
  
  cons.dir <- rep("=", nt)
  cons.rhs <- rep(0, nt)
  constr  <- add.constraints( cons.mat, cons.dir, cons.rhs, constr )
  
  sol <- optimize.LP.wrap( "min",
                           f.obj,
                           constr )
   
  sol$Sol <- sol$Sol[1:n]
  sol$Return <- sum(sol$Sol * inp$expected.return )
  sRisk <- portfolio.risk.mad( sol$Sol, inp )
  sol$Risk <- sRisk[1]
  
  return(sol)
}


###############################################################################
#### Minimize portfolio standard deviation
###############################################################################
MinStdPortfolio <- function
(
  inp,
  constraints = NULL
) {
  
  sol <- optimize.QP.wrap(
    "min",
    inp$cov,
    rep(0, nrow(inp$cov)),
    constraints )
  
  return(sol)
}