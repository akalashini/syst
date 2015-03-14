# private tool for asset allocation research
# by Jian Tang 2015
# all rights reserved
# packages needed to use this module:
# lpSolveAPI 

##################################################################################
# some convenient functions
##################################################################################
##
splstr <- function(s, delim = ',')
{
  # split a string and return a vector
  return(unlist(strsplit(s,delim)))
}

##
isnone <- function(var) {
  # if var is a non-valid object then return true, such as NA,NULL,FALSE
  if (length(var) == 0)
    return(TRUE)
  if (length(var) == 1) {
    if(is.na(var) || is.nan(var)) {
      return(TRUE)
    }  
  } 
  return(FALSE)
}

##
iif <- function (cond, truepart, falsepart) {
  # if cond is TRUE then return true part else return falsepart
  return(ifelse(isnone(cond), falsepart, truepart))
}

##
load.packages <- function(
  packages,
  repos = "http://cran.r-project.org",
  dependencies = c("Depends", "Imports"),
  ...
) {
  packages = splstr(packages)
  for( ipackage in packages ) {
    if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
      install.packages(ipackage, repos=repos, dependencies=dependencies, ...)
      if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
        stop("package", sQuote(ipackage), 'is needed.  Stopping')
      }
    }
  }
}

##
p.force.be.matrix <- function( var, byrow = TRUE ) {
  # force a var to be a matirx
  if(!is.null(dim(var))) { return(var) }
  else {
    if(byrow) { return(matrix(var, 1, length(var), byrow=TRUE)) }
    else { return(matrix(var, length(var), 1, byrow=FALSE)) }    
  }
}

#### constraints utils ####
# here is how the structure of a constraint look like
# $n     : num of variables
# $mat   : constraint matrix
# $dir   : direction list: =, >= (need >= since QP interface uses this, does not matter for LP)
# $rhs   : right side of constraint
# $meq   : first #meq of constraints are "=", need by QP, thought no need for LP
# $lb    : lower bounds of variables
# $ub    : upper bounds of variables
# $lb.vec: vector for variables that have lower bounds
# $ub.vec: vector for variables that have upper bounds
####

p.reorg.constraints <- function 
(
  const.mat,
  const.dir,
  const.rhs
) {
  # let the first constraints to be "=", the rest to be ">=", to comply wiht QP package
  # [TESTED]
  
  if (is.null(dim(const.mat))) dim(const.mat) <- c(1, length(const.mat))
  
  # get constraints with "=" first
  meq   <- sum(const.dir == "=")
  mineq <- length(const.dir) - meq
  
  # for QP, need to convert constraint to >= mode
  #lt.mat.tm <- const.mat[const.dir == "<=", ]
  const.mat[const.dir == "<=", ] <- p.force.be.matrix(- const.mat[const.dir == "<=", ])
  const.rhs[const.dir == "<="]   <- - const.rhs[const.dir == "<="]
  
  eq.const.mat <- p.force.be.matrix(const.mat[const.dir == "=", ])
  eq.const.rhs <- const.rhs[const.dir == "="]
  
  ineq.const.mat <- p.force.be.matrix(const.mat[!(const.dir == "="), ])
  ineq.const.rhs <- const.rhs[!(const.dir == "=")]
  
  # for QP, need to convert constraint to >= mode
  #ineq.const.mat[dir == "<=", ] <- p.force.be.matrix(- ineq.const.mat[const.dir == "<=", ])
  #ineq.const.rhs[dir == "<="] <- - ineq.const.rhs[dir == "<="]
  
  anew.mat <- rbind(eq.const.mat, ineq.const.mat)
  anew.dir <- c(rep("=", meq), rep(">=", mineq))
  anew.rhs <- c(eq.const.rhs, ineq.const.rhs)
  
  rst <- list(mat = anew.mat, dir = anew.dir, rhs = anew.rhs, meq = meq )
  return(rst)
}


create.new.constraints <- function
(
  n,                # n variables
  const.mat = NULL, # matrix
  const.dir = NULL, # =, <=, >=
  const.rhs = NULL, # b
  lb = 0,
  lb.vec = NULL,
  ub = +Inf,
  ub.vec = NULL,
  binary.vec  = NULL, # indices of which var is binary
  integer.vec = NULL  # indices of which var is integer
)
{
  meq <- 0
  if ( is.null(const.mat) || is.null(const.rhs) ) {
    const.mat <- matrix(0, 0, n, byrow = TRUE)
    const.dir <- c()
    const.rhs <- c()
    meq       <- 0
  } else {
    if ( is.null(dim(const.mat)) ) dim(const.mat) = c(1, length(const.mat))
   
    # re-organize the constraints
    const.reorg <- p.reorg.constraints(const.mat, const.dir, const.rhs)
    const.mat <- const.reorg$mat
    const.dir <- const.reorg$dir
    const.rhs <- const.reorg$rhs
    meq       <- const.reorg$meq
  }
  if (isnone(lb)) {
    lb <- rep(NA, n)
  } else if (length(lb) == 1) {
    lb <- rep(lb[1], n)
  }
  if (isnone(ub)) {
    ub <- rep(+Inf, n)
  } else if (length(ub) == 1) {
    ub <- rep(ub[1], n)
  }
  
  return( list(n = n, mat = const.mat, dir = const.dir, rhs = const.rhs, lb = lb, ub = ub, meq = meq, binary.vec = binary.vec, integer.vec = integer.vec, lb.vec = lb.vec, ub.vec = ub.vec ) )
}

####
add.constraints <- function
(
  const.mat,
  const.dir,
  const.rhs,
  constraints
)
{
  # add constraints to existing constraints
  if(is.null(dim(const.mat))) const.mat <- p.force.be.matrix(const.mat)
  if(length(const.dir) == 1) const.dir <- rep(const.dir, nrow(const.mat))
  if(length(const.rhs) == 1) const.rhs <- rep(const.rhs, nrow(const.mat))
  
  if(is.null(constraints)) constraints <- create.new.constraints(n = ncol(const.mat))
  
  constraints$mat <- rbind( const.mat, constraints$mat )
  constraints$dir <- c( const.dir, constraints$dir )
  constraints$rhs <- c( const.rhs, constraints$rhs )
  
  newConst <- p.reorg.constraints(constraints$mat, constraints$dir, constraints$rhs)
  constraints[c("mat", "dir", "rhs", "meq")] <- newConst
  
  return(constraints)
}

####
add.variables <- function
(
  n,
  constraints,
  lb = NA,
  ub = NA,
  lb.vec = NULL,
  ub.vec = NULL,
  binary.vec = NULL,
  integer.vec = NULL
)
{
  # add n variables to constraints already exists
  constraints$mat <- cbind( constraints$mat, matrix(0, nrow(constraints$mat), n, byrow=TRUE) )
  if (isnone(lb))  { lb <- rep(-Inf, n) } else {
  if (length(lb) != n) lb <- rep(lb[1], n) }
  
  if (isnone(ub)) { ub <- rep(+Inf, n) } else {
  if (length(ub) != n) ub = rep(ub[1], n) }
  
  if (!isnone(lb.vec)) constraints$lb.vec <- c(constraints$lb.vec, lb.vec)
  if (!isnone(ub.vec)) constraints$ub.vec <- c(constraints$ub.vec, ub.vec)
  if (!isnone(binary.vec)) constraints$binary.vec <- c(constraints$binary.vec, binary.vec)
  if (!isnone(integer.vec)) constraints$integer.vec <- c(constraints$integer.vec, integer.vec)
  
  constraints$lb <- c(constraints$lb, lb)
  constraints$ub <- c(constraints$ub, ub)
  constraints$n <- constraints$n + n
  return( constraints )
}

####
delete.constraints <- function
(
  delete.index,
  constraints
)
{
  constraints$mat = constraints$mat[-delete.index, , drop=F]
  constraints$rhs = constraints$rhs[-delete.index]
  constraints$meq = constraints$meq - length(intersect((1:constraints$meq), delete.index))
  return( constraints )
}
####
#### END constraint utils

######## linear programming ######
optimize.LP <- function
(
  direction,           # "min" or "max"
  objective.vec,       # objective function
  const.mat,           # constraint matrix, each row is a constraint
  const.dir,           # direction, "=", ">=" "<=" etc
  const.rhs,           # rhs
  binary.vec  = NA,    # index of which vars are binary
  integer.vec = NA,    # index of which vars are integers
  lb     = 0,          # lower bound
  lb.vec = NA,         # what vars to set lower bound
  ub     = +Inf,       # upper bound
  ub.vec = NA          # what vars to set upper bound
)
{
  # linear programming tool with bounds and binary
  # a wrapper on lpSolveAPI, so requires package lpSolveAPI
  
  n <- length(objective.vec)
  iDim <- dim(const.mat) 
  m <- iDim[1]
  # n variables, m rows of constraints
  
  lprec <- make.lp(m, n)
  
  lp.control(lprec, sense = direction)
  # set constraints
  for (i in 1:ncol(const.mat)) {
    set.column(lprec, i, const.mat[, i])
  }
  
  # set constraint type
  set.constr.type(lprec, const.dir)
  
  # set rhs
  set.rhs(lprec, const.rhs)
  
  # set binary
  if(!isnone(binary.vec)) {
    for(i in binary.vec) {
      set.type(lprec, i, "binary")
    }
  }
  
  # set integer
  if(!isnone(integer.vec)) {
    for(i in integer.vec) {
      set.type(lprec, i, "integer")
    }
  }
  
  # set bounds  
  if(!isnone(lb.vec)) {
    if(length(lb) == 1) lb <- rep(lb, length(lb.vec))    
  } else {
    if(isnone(lb))
    {      
      lb <- NULL
      lb.vec <- NULL
    } else if(length(lb) == 1) {
      lb <- rep(lb[1], n)      
      lb.vec <- seq(1,n)
    } else if(isnone(lb.vec)) {
      lb.vec <- seq(1,n)
    } 
  }
  
  if(!isnone(ub.vec)) {
    if(length(ub) == 1) ub <- rep(ub, length(ub.vec))    
  } else {
    if(isnone(ub))
    {      
      ub <- NULL
      ub.vec <- NULL
    } else if(length(ub) == 1) {
      ub <- rep(ub[1], n)      
      ub.vec <- seq(1,n)
    } else if(isnone(ub.vec)) {
      ub.vec <- seq(1,n)
    }
  }
  
  set.bounds(lprec, lower = lb, upper = ub, columns = lb.vec)
  
  # set objective function here
  # do this in the last step b/c somehow if we do it at the first step then obj func gets 
  # messed up due to maybe a bug somewhere
  set.objfn(lprec, objective.vec)
  
  # ready to optimize
  print(lprec)
  
  status <- solve(lprec)
  if(status==0) {
    cat("**LP Optimizing Successful.\n")
  } else {
    cat("LP Optimizing Failed.\n")
    rm(lprec)
    return(NULL)
  }
  
  # now get results
  rst     <- list()
  rst$Obj <- get.objective(lprec)
  rst$Sol <- get.variables(lprec)
  rst$Con <- get.constraints(lprec)
  
  # free memory
  rm(lprec)
  
  return(rst)
} #END optimize.LP


optimize.LP.wrap <- function
(
  direction,          # "min" or "max"
  objective.vec,      # objective function
  constraints         # constraint list  
) {
  sol <- optimize.LP(  
                     direction,            
                     objective.vec,        
                     constraints$mat,     
                     constraints$dir,     
                     constraints$rhs,     
                     constraints$binary.vec,
                     constraints$integer.vec,
                     constraints$lb,
                     constraints$lb.vec,  
                     constraints$ub,     
                     constraints$ub.vec )
}

#### max/min return portfolio
lp.obj.portfolio <- function
(
  inp,
  constraints,
  direction = 'min'
)
{
  x     <- NA 
  f.obj <- c(inp$expected.return)
  sol   <- try(optimize.LP.wrap(direction, f.obj, constraints), FALSE)
  if(!inherits(sol, 'try-error')) {
    x <- sol$Sol
  }
  return( x )
}

#### max return portfolio
max.return.portfolio <- function(
  inp,
  constraints
  ) {
  return( lp.obj.portfolio(inp, constraints, 'max') )
}

### portfolio utils
portfolio.weighted.return <- function
(
  weight,
  inp
)
{
  if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
  weight <- weight[, 1:inp$n, drop=F]
  portfolio.return <- weight %*% inp$expected.return
  return( c(portfolio.return) )
}

#### portfolio risk
portfolio.std.risk <- function(
  weight,
  inp
){
  if(is.null(dim(weight))) dim(weight) <- c(1, length(weight))
  weight <- weight[, 1:inp$n, drop=F]
  cov    <- inp$cov[1:inp$n, 1:inp$n]
  return( apply(weight, 1, function(x) sqrt(t(x) %*% cov %*% x)) )
}


#### generat efficient portfolio frontier
# inp is a list with the following structure
#   : n
#   : symbols
#   : expected.return
#   : cov
#   : hist.returns

efficient.port.gen <- function( 
  
  inp, 
  constraints,
  min.risk.fn,  
  name = 'Risk',
  npoints = 25
  )
{
  # first find max return portfolio
   
  load.packages('quadprog,corpcor,lpSolve,kernlab')
  if( is.null(constraints) ) {
    constraints <- create.new.constraints( inp$n )
  }
  
  inp$risk <- ifelse(inp$risk == 0, 0.000001, inp$risk)
  if( is.null(inp$cov) ) inp$cov <- inp$corr * (inp$risk %*% t(inp$risk))
  inp$cov.temp <- inp$cov
  n0 <- inp$n
  n  <- ncol(constraints$mat)
  if( n != ncol(inp$cov.temp) ) {
    temp <- matrix(0, n, n)
    temp[1:n0, 1:n0] <- inp$cov.temp[1:n0, 1:n0]
    inp$cov.temp <- temp
  }
  
  if(!is.positive.definite(inp$cov.temp, method = 'chol')) {
    inp$cov.temp <- make.positive.definite(inp$cov.temp, 0.000000001)
  }
  
  if(npoints<2) npoints <- 2

  # results saved in out
  out <- list(weight = matrix(NA, npoints, ncol(constraints$mat)))
  
  colnames(out$weight)          <- rep('', ncol(out$weight))
  colnames(out$weight)[1:inp$n] <- inp$symbols
  
  # get portfolio with max return
  out$weight[npoints, ] <- max.return.portfolio(inp, constraints)
  # get portfolio with min risk
  solMinRisk <- match.fun(min.risk.fn)(inp, constraints)
  out$weight[1, ]       <- solMinRisk$Sol
  
  #constraints$x0 = out$weight[1, ]
  if(npoints > 2) {
    out$return  <- portfolio.weighted.return(out$weight, inp)
    target      <- seq(out$return[1], out$return[npoints], length.out = npoints)
    #base.constr <- add.constraints(c(inp$expected.return, rep(0, ncol(constraints$mat) - inp$n)),
    #                              '', target[1], constraints)
    for(i in 2:(npoints - 1) ) {
      iconstr <- add.constraints(c(inp$expected.return, rep(0, ncol(constraints$mat) - inp$n)),
                                     '=', target[i], constraints)
      #constraints$b[ length(constraints$b) ] = target[i]
      isolMinRisk <- match.fun(min.risk.fn)(inp, iconstr)
      out$weight[i, ] <- isolMinRisk$Sol
      #constraints$x0 = out$weight[i, ]
    }
         
  }
  
  rm.index <- is.na(apply(out$weight, 1, sum))
  if(any(rm.index)) out$weight <- out$weight[!rm.index, ]
  out$return <- portfolio.weighted.return(out$weight, inp)
  out$risk   <- portfolio.std.risk(out$weight, inp)
  out$name   <- name
  return(out)
} #### END efficient.port.gen


###################################################################################
######################## some plotting tools for portfolio ########################
###################################################################################
# tplot.legend <- function
# (
#   labels,
#   fill = NULL,
#   lastobs = NULL,
#   x = 'topleft',
#   merge = F,
#   bty = 'n' 
#   ...
# )
# {
#   if( !is.null(fill) ) fill <- splstr( as.character(fill) )
#   labels <- splstr( as.character(labels) )
#    
#   legend(x, legend = labels, fill = fill, merge = merge, bty = bty, ...)
# }
# 
#### given limits, do a rescaling to make limits wider and make plot nicer
p.rescale.limits <- function( limits ) {
  scaleH <- 1.2
  scaleL <- 0.8
  ilim <- range(limits)
  if( length(ilim) == 1 ) ilim <- rep(ilim[1], 2)
  lowerLim  <- ilim[1]
  higherLim <- ilim[2]
  
  lowerLim <- lowerLim * ifelse( lowerLim < 0, scaleH, scaleL )  
  higherLim <- higherLim * ifelse( higherLim < 0, scaleL, scaleH )
  
  return( c(lowerLim, higherLim) )
}

#### efficient.port.plot
efficient.port.plot <- function(
  
  inp,                          # input data includes symbol,returns
  eff.portfolios,               # a list of different efficient portfolios
                                # each item must be a list with $weight $return 
  portfolio.risk.fn = portfolio.std.risk # the function to calculate the risk defined here    
  )
{
  # plot the efficient frontier
  
  
  scaleReturn <- 100
  scaleRisk   <- 100
  
  n <- inp$n
  x <- match.fun(portfolio.risk.fn)(diag(n), inp)
  y <- inp$expected.return
  
  # xlim gives the range of risk of all portfolios including 0
  xlim <- range(c(0, x,
                 max( sapply(eff.portfolios, function(x) max(match.fun(portfolio.risk.fn)(x$weight,inp))) ) ), na.rm = T)
  # ylim gives the range of return of all porfolios including 0
  ylim <- range(c(0, y,
                 min( sapply(eff.portfolios, function(x) min(portfolio.weighted.return(x$weight,inp))) ),
                 max( sapply(eff.portfolios, function(x) max(portfolio.weighted.return(x$weight,inp))) ) ), na.rm = T)
   
  xlim <- scaleRisk * p.rescale.limits(xlim)
  ylim <- scaleReturn * p.rescale.limits(ylim)
   
  y <- c()
  x <- c()
  name <- c()
  
  for(i in length(eff.portfolios):1) {
    ef <- eff.portfolios[[ i ]]
    ix  <- scaleRisk   * match.fun(portfolio.risk.fn)(ef$weight, inp)
    iy  <- scaleReturn * ef$return
    iname <- rep(ef$name, length(ix))
    x <- c(x, ix)
    y <- c(y, iy)
    name <- c(name, iname)
  }
  dataToPlot <- data.frame( x, y, name )
  assetPoints <- list(x=inp$risk * scaleRisk, y=inp$expected.return * scaleReturn, symbol=inp$symbols)
   
  xyplot(y~x, dataToPlot, groups=name, type="l", lwd=2, 
         par.settings = list(axis.line = list(lwd = 2)), # set axis line width
         factor = 5, 
         scales = list(tck=c(1,0), x=list(font=2,cex=1.1,limits=xlim), y=list(font=2,cex=1.1,limits=ylim)), # axis font size and bold face
         auto.key = list(points = F, lines = F, rectangles = T, columns=1 , space="inside",  x = .75, y=.9, corner = c(0,1), border = FALSE),  
         xlab=list(label="Risk",fontsize=14, font=2),
         ylab =list(label= "Return(%)",fontsize=14, font=2), 
         panel = function(...) 
           { panel.grid(); 
             panel.xyplot(...); 
             panel.points(x=assetPoints$x, y=assetPoints$y, type="p", pch=21, col="navy",fill="navy"); 
             panel.text(x=assetPoints$x, y=assetPoints$y, labels=assetPoints$symbol, adj=c(1.25,1.25), col="red",cex=0.9,font=3)
             } 
         )
} ## END eff.portfolio.plot

####
#plot.transitopn.map <- function(x,y,xlab = 'Risk',name = '',type=c('s','l')) {
##  plot.transition.map(x,y,xlab,name,type)
#}

#### BEGIN plot transition map
plot.transition.map <- function
(
  weight,
  risk,
  xlab = 'Risk',
  name = '',
  type=c('s','l'),
  col = NA
)
{
  if( is.list(weight) ) {
    name <- weight$name
    risk    <- 100 * weight$risk
    weight    <- weight$weight
  }
  weight[is.na(weight)] <- 0
  par(mar = c(4,3,2,1), cex = 0.8)
  plota.stacked(risk, weight, xlab = xlab, main = paste('Transition Map for', name),
                type=type[1], col=ifna(col, plota.colors(ncol(y))) )
}