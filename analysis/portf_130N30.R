#--------------------------------------------------------------------------
# Create Efficient Frontier for 130/30 Fund
#--------------------------------------------------------------------------
p.ls.fundname <- function( long )
{
  short <- long - 1.
  fundname <- sprintf('%.0f/%.0f Fund', long * 100, short * 100)
  return(fundname)
}

## minimize max loss portfolio using long - short 
L130S30Portfolio_LS <- function
(
  inp,
  long = 1.3
) {
  ## this constraint is not enough, eg, let long = 2
  ## then it is not giving you +200, -100 at lower risk region
  
   
  n <- inp$n

  # let each x = x_Long - x_Short 
  # x_L >0, x_S >0, X_L < 0.8, x_S < 0.5
  constraints <- create.new.constraints( 2*n, lb = 0, ub = c(rep(0.8,n), rep(0.5,n)) ) 

  # SUM x.i = 1
  constraints <- add.constraints( c(rep(1, n), -rep(1, n)), '=', 1.0, constraints)
  
  # constrain sum x_S + x_L = 1.6
  constraints <- add.constraints( c(rep(1, n), rep(1, n)), '=', 2*long - 1.0 , constraints)
  
  # eff portfolio
  # need to reshape input
  inp.temp <- p.represent.var.long.short(inp)
  
  effStd <- efficient.port.gen(
    inp.temp, 
    constraints,
    MinStdPortfolio,  
    name = '130/30 StdRisk',
    npoints = 25
  )
  
  # back shape from long,short to original x
  effStd$weight <- effStd$weight[,1:n] - effStd$weight[,(n+1):(2*n)]
  
  efficient.port.plot( inp, list(effStd), portfolio.risk.std, asset.points = T)
  plot.transition.map( effStd$weight, effStd$risk, type="l", name = p.ls.fundname(long))
}
 

L130S30Portfolio_good <- function
(
  inp,
  long = 2.
) {
  ## additional constraint to fix low risk region
  ## use binary constaint for long,short
  ## L < b, S < (1-b)
  
  
  n <- inp$n
  
  # let each x = x_Long - x_Short 
  # x_L >0, x_S >0, X_L < 0.8, x_S < 0.5
  constraints <- create.new.constraints( 2*n, lb = c(rep(0, n), rep(0,n)) , ub = c(rep(0.8,n), rep(0.5,n)) ) 
  
  # SUM x.i = 1
  constraints <- add.constraints( c(rep(1, n), -rep(1, n)), '=', 1.0, constraints)
  
  # constrain sum x_S + x_L = 1.6
  constraints <- add.constraints( c(rep(1, n), rep(1, n)), '=', 2*long - 1.0 , constraints)
  
  constraints <- add.variables(n, constraints, binary.vec = 1:n)
  # index of binary variables b.i 
  
  # x.long < b.i
  constraints <- add.constraints(cbind(diag(n), 0*diag(n), -diag(n)), '<=', rep(0, n), constraints)
  
  # x.short.i < (1-b.i)
  constraints <- add.constraints(cbind(0*diag(n), diag(n), diag(n)), "<=", rep(1, n), constraints)
  
  # L < b
  #constraints <- add.constraints( c(rep(1, n), rep(0, n), -rep(1, n)), '<=', 0 , constraints)
  
  # S < 1-b
  #constraints <- add.constraints( c(rep(0, n), rep(1, n), rep(1, n)), '<=', 1, constraints)
  
  # eff portfolio
  # need to reshape input
  inp.temp <- p.represent.var.long.short(inp)
  
  effStd <- efficient.port.gen(
    inp.temp, 
    constraints,
    MinMADPortfolio,  
    name = p.ls.fundname(long),
    npoints = 25
  )
  
  # back shape from long,short to original x
  effStd$weight <- effStd$weight[,1:n] - effStd$weight[,(n+1):(2*n)]
  
  efficient.port.plot( inp, list(effStd), portfolio.risk.std, asset.points = T)
  plot.transition.map( effStd$weight, effStd$risk, type="l", name = p.ls.fundname(long))
}

  