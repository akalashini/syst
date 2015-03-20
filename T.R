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
  if(isnone(cond))
    return(falsepart)
  else
    return(truepart)
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
    lb <- rep(-Inf, n) 
  } else if (length(lb) == 1) {
    lb <- rep(lb[1], n) 
  }
  if(length(lb) == n)
    lb.vec <- seq(1, n)
  
  if (isnone(ub)) {
    ub <- rep(+Inf, n) 
  } else if (length(ub) == 1) {
    ub <- rep(ub[1], n) 
  }
  if(length(ub) == n)
    ub.vec <- seq(1, n)
  
  if( isnone(lb.vec) ) {
    lb.vec <- seq(1, n)
  }
   
  if( isnone(ub.vec) ) {
    ub.vec <- seq(1, n)
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
  lb = 0,
  ub = +Inf,
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
  if (length(ub) != n) ub <- rep(ub[1], n) }
  
  if (!isnone(lb.vec)) 
    constraints$lb.vec <- c(constraints$lb.vec, lb.vec+constraints$n)
  else {
    if(!isnone(constraints$lb.vec))
      constraints$lb.vec <- c(constraints$lb.vec, seq(constraints$n + 1, constraints$n + n) )
    else
    {
      constraints$lb.vec <- seq(1, constraints$n + n)
    }
  }
  
  if (!isnone(ub.vec)) 
    constraints$ub.vec <- c(constraints$ub.vec, ub.vec+constraints$n)
  else {
    if(!isnone(constraints$ub.vec))
      constraints$ub.vec <- c(constraints$ub.vec, seq(constraints$n + 1, constraints$n + n) )
    else
    {
      constraints$ub.vec <- seq(1, constraints$n + n)
    }
  }
  
  if (!isnone(binary.vec)) constraints$binary.vec <- c(constraints$binary.vec, binary.vec+constraints$n)
  if (!isnone(integer.vec)) constraints$integer.vec <- c(constraints$integer.vec, integer.vec+constraints$n)
  
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

## -----

## reshape the input var into x = Long - Short
## 
p.represent.var.long.short <- function( inp )
{
  inp$symbols <- c(inp$symbols, inp$symbols)
  inp$n <- 2*inp$n
  inp$hist.returns <- cbind(inp$hist.returns, -inp$hist.returns)
  inp$expected.return <- c(inp$expected.return, -inp$expected.return)
  inp$risk <- c(inp$risk, inp$risk)
  inp$corr <- cbind( rbind(inp$corr, -inp$corr), rbind(-inp$corr, inp$corr) )
  inp$cov  <- cbind( rbind(inp$cov, -inp$cov), rbind(-inp$cov, inp$cov) )
  
  return(inp)
}

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
  lapply(c('lpSolve','lpSolveAPI','logging'), require, character.only=T)
  
  n0 <- length(objective.vec)
  iDim <- dim(const.mat) 
  m <- iDim[1]
  n <- iDim[2]
  # n variables, m rows of constraints
  if(n0 < n)
  {
    # if
    objective.vec <- c( objective.vec, rep(0, n-n0) )
  }
  
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
  #logdebug(lprec) # looks like this does not work
  #print(lprec)
  
  status <- solve(lprec)
  if(status==0) {
    logdebug("LP Optimizing Successful.\n")
  } else {
    logwarn("**LP Optimizing Failed.\n")
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
portfolio.risk.std <- function(
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
  out$risk   <- portfolio.risk.std(out$weight, inp)
  out$name   <- name
  return(out)
} #### END efficient.port.gen


###################################################################################
######################## some plotting tools for portfolio ########################
###################################################################################

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
  portfolio.risk.fn = portfolio.risk.std, # the function to calculate the risk defined here    
  title        = "",
  asset.points = TRUE           # whether plot the scattered asset risk-return points on the same plot
  )
{
  require(lattice)
  # plot the efficient frontier
  if(isnone(title) | title == "")
    title <- as.character( substitute(portfolio.risk.fn) )
  
  scaleReturn <- 100
  scaleRisk   <- 100
  
  n <- inp$n
  x <- match.fun(portfolio.risk.fn)(diag(n), inp)
  y <- inp$expected.return
  assetPoints <- list(x=x * scaleRisk, y=y * scaleReturn, symbol=inp$symbols)
  
  # xlim gives the range of risk of all portfolios including 0
  xlim <- range(c( min( sapply(eff.portfolios, function(x) min(match.fun(portfolio.risk.fn)(x$weight,inp))) ),
                   max( sapply(eff.portfolios, function(x) max(match.fun(portfolio.risk.fn)(x$weight,inp))) ) ), na.rm = T)
  # ylim gives the range of return of all porfolios including 0
  ylim <- range(c( min( sapply(eff.portfolios, function(x) min(portfolio.weighted.return(x$weight,inp))) ),
                   max( sapply(eff.portfolios, function(x) max(portfolio.weighted.return(x$weight,inp))) ) ), na.rm = T)
  if(asset.points) {
    xlim <- range( c(xlim, x) )
    ylim <- range( c(ylim, y) )
  }
    
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
   
  xyplot(y~x, dataToPlot, groups=name, type="l", lwd=2, main=title,
         par.settings = list(axis.line = list(lwd = 2)), # set axis line width
         factor = 5, 
         scales = list(tck=c(1,0), x=list(font=2,cex=1.1,limits=xlim), y=list(font=2,cex=1.1,limits=ylim)), # axis font size and bold face
         auto.key = list(points = F, lines = T, rectangles = F, columns=1 , space="inside",  x = .6, y=.4, corner = c(0,1), border = FALSE),  
         xlab=list(label="Risk",fontsize=14, font=2),
         ylab =list(label= "Return(%)",fontsize=14, font=2), 
         panel = function(...) 
           { panel.grid(); 
             panel.xyplot(...); 
             if(asset.points ) {
               panel.points(x=assetPoints$x, y=assetPoints$y, type="p", pch=21, col="navy",fill="navy"); 
               panel.text(x=assetPoints$x, y=assetPoints$y, labels=assetPoints$symbol, adj=c(1.25,1.25), col="red",cex=0.9,font=3)
             }
             } 
         )
} ## END eff.portfolio.plot

#### BEGIN utils for transition map plotting
## some helper functions
p.tplot.format <- function(
  temp,
  nround = 2,
  sprefix = '',
  eprefix = ''
)
{
  return( paste(sprefix,  format(round(as.numeric(temp), nround), big.mark = ',', scientific=FALSE),
                eprefix ,sep='') )
}

## private function to generate a series of discernable colors
#  neighbors do not have the same color
p.tplot.colors <- function(N) {
  col  <- rev(c('yellow','cyan','magenta','red','gray','green','blue'))
  temp <- list()
  for(j in 1:length(col)) {
    temp[[j]] <- colors()[grep(col[j],colors())]
    temp[[j]] <- temp[[j]][grep('^[^0-9]*$',temp[[j]])]
    temp[[j]] <- temp[[j]][order(nchar(temp[[j]]))]
    index <- which( colSums(col2rgb(temp[[j]])) < 100 )
    if( length(index) > 0 ) temp[[j]] <- temp[[j]][-index]
    index <- which( colSums(255 - col2rgb(temp[[j]])) < 100 )
    if( length(index) > 0 ) temp[[j]] <- temp[[j]][-index]
  }
  index <- 1
  col <- rep('', N)
  for(i in 1:10) {
    for(j in 1:length(temp)) {
      if(length(temp[[j]]) >= i) {
        col[index] <- temp[[j]][i]
        index <- index + 1
        if(index > N) break
      }
    }
    if(index > N) break
  }
  return(col)
}

p.tplot.legend <- function
(
  labels,
  fill = NULL,
  lastobs = NULL,
  x = 'topleft',
  merge = F,
  bty = 'n',
  yformat = p.tplot.format,
  ...
)
{
  if( !is.null(fill) ) fill <- splstr( as.character(fill) )
  labels <- splstr( as.character(labels) )
  if( !is.null(lastobs) ) {
    if( is.list(lastobs) ) {
      labels1 <- sapply(lastobs, function(x) unclass(last(x))[1])
    } else {
      labels1 <- unclass(last(lastobs))[1];
    }
    labels <- paste(labels, match.fun(yformat)( labels1 ))
  }
  legend(x, legend = labels, fill = fill, merge = merge, bty = bty, ...)
}

## plot stacked graph
p.tplot.stacked <- function
(
  x,
  y,
  xlab='',
  col = p.tplot.colors(ncol(y)),
  type=c('l','s'),
  ...
)
{
  y  <- 100 * y
  x  <- 100 * x
  y1 <- list()
  y1$positive <- y
  y1$positive[ y1$positive < 0 ] <- 0
  y1$negative <- y
  y1$negative[ y1$negative > 0 ] <- 0
  ylim <- c(min(rowSums(y1$negative, na.rm = T)), max(1, rowSums(y1$positive, na.rm = T)))
  if( class(x)[1] != 'Date' & class(x)[1] != 'POSIXct') {
    plot(x, rep(0, length(x)), ylim = ylim, t = 'n', xlab = '', ylab = '', cex = par('cex'), ...)
    grid()
  } else {
    #plota(make.xts(y[,1], x), ylim = ylim, cex = par('cex'), LeftMargin = 4, ...)
    #axis(2, las = 1)
    #x = unclass(as.POSIXct(x))
  }
  mtext('Allocation %', side = 2,line = 2, cex = par('cex'))
  mtext(xlab,           side = 1,line = 2, cex = par('cex')) 
  if( type[1] == 'l' ) {
    prep.x = c(x[1], x, x[length(x)])
    for( y in y1 ) {
      for (i in ncol(y) : 1) {
        prep.y = c(0, rowSums(y[, 1 : i, drop = FALSE]), 0)
        polygon(prep.x, prep.y, col = col[i], border = NA, angle = 90 )
      }
    }
  } else {
    dx = mean(diff(x))
    prep.x = c(rep(x,each=2), x[length(x)] + dx, x[length(x)] + dx)
    for( y in y1 ) {
      for (i in ncol(y) : 1) {
        prep.y = c(0, rep(rowSums(y[, 1 : i, drop = FALSE]),each=2), 0)
        polygon(prep.x, prep.y, col = col[i], border = NA, angle = 90)
      }
    }
  }
  p.tplot.legend(colnames(y), col, cex = par('cex')) 
}

#### BEGIN plot transition map
plot.transition.map <- function
(
  weight,    # a matrix, each column is a asset, 
  risk,      # a vector
  xlab = 'Risk',
  name = '',
  type = "l",
  col  = NA
)
{
  if( is.list(weight) ) {
    name   <- weight$name
    risk   <- weight$risk
    weight <- weight$weight
  }
  weight[is.na(weight)] <- 0
  par(mar = c(4,3,2,1), cex = 0.8)
  if(isnone(col)) col <- p.tplot.colors(ncol(weight))
  idxOrder <- order(risk)
  risk <- risk[idxOrder]
  weight <- weight[idxOrder, ]
  p.tplot.stacked(risk, weight, xlab = xlab, main = paste('Transition Map for', name),
                type=type[1], col=col )
}

#### END of transition map plotting


###############################################################################
######## QP (Quadratic Programming) 
###############################################################################
optimize.QP <- function
( 
  Hmat,                # objective function H
  fvec,                # f vec
  const.mat,           # constraint matrix, each row is a constraint
  const.dir,           # direction, "=", ">=" "<=" etc
  const.rhs,           # rhs
  binary.vec  = NA,    # index of which vars are binary
  lb     = -Inf,       # lower bound 
  ub     = +Inf,       # upper bound   
  factorized=FALSE
)
{
  require(quadprog)
  ## input conventions please refer to Mathworks:
  ## min { 1/2 t(x) * H * x + t(f)*x }
  ## constrainted by:
  ## Ax >,<,= b # this does not matter here since we will reorganize to be = or >= 
  ## these input will be revised to conform to the input requirement of quadprog package
  
  # reorganize the constraints so that the first #meq have "=" constraints
  # and the rest are ">=" constraints
  constr <- p.reorg.constraints( const.mat, const.dir, const.rhs )
  meq    <- constr$meq
  Amat   <- t(constr$mat)
  bvec   <- constr$rhs
  
  # deal with lower and upper bounds
  if(!isnone(fvec)) n <- length(fvec)
  else n <- nrow( Hmat)  
  if( length(lb) == 1 ) lb <- rep(lb, n)
  if( length(ub) == 1 ) ub <- rep(ub, n)
  
  lb[is.na(lb) | is.null(lb) | is.nan(lb) ] <- -Inf
  ub[is.na(ub) | is.null(ub) | is.nan(ub) ] <- +Inf
   
  index <- which( ub < +Inf )
  if( length(index) > 0 ) {
    bvec <- c(bvec, -ub[index])
    Amat <- cbind(Amat, -diag(n)[, index])
  }
  
  index <- which( lb > -Inf )
  if( length(index) > 0 ) {
    bvec <- c(bvec, lb[index])
    Amat <- cbind(Amat, diag(n)[, index])
  }
  
  rst <- list()
  
  if(isnone(binary.vec)) {
    #qp.data.final = solve.QP.remove.equality.constraints(Dmat, dvec, Amat, bvec, meq)
    Dmat <- Hmat
    dvec <- -fvec
    #dvec = qp.data.final$dvec
    #Amat = qp.data.final$Amat
    #bvec = qp.data.final$bvec
    #meq = qp.data.final$meq
    sol = try(solve.QP(Dmat, dvec, Amat, bvec, meq, factorized),TRUE)
    if(inherits(sol, 'try-error')) {
      ok = F
      sol = list()
    } else {
      tol = 1e-3
      ok = T
      check = sol$solution %*% Amat - bvec
      if(meq > 0) ok = ok & all(abs(check[1:meq]) <= tol)
      ok = ok & all(check[-c(1:meq)] > -tol)
    }
#     if(!ok) {
#       require(kernlab)
#       index.constant.variables = which(!is.na(qp.data.final$solution))
#       if( len(index.constant.variables) > 0 ) {
#         Amat1 = Amat[,1:ncol(Amat1)]
#         bvec1 = bvec[1:ncol(Amat1)]
#         lb = lb[-index.constant.variables]
#         ub = ub[-index.constant.variables]
#       }
#       sv = ipop(c = matrix(-dvec), H = Dmat, A = t(Amat1),
#                 b = bvec1, l = ifna(lb,-100), u = ifna(ub,100),
#                 r = c(rep(0,meq), rep(100, len(bvec1) - meq))
#       )
#       sol$solution = primal(sv)
#     }
#    x = qp.data.final$solution
#    x[qp.data.final$var.index] = sol$solution
    rst$Sol <- sol$solution
  } else {
#     qp_data = qp_new(binary.vec, Dmat = Dmat, dvec = dvec,
#                      Amat=Amat, bvec=bvec, meq=meq, factorized=factorized)
#     sol = binary_branch_bound(binary.vec, qp_data, qp_solve,
#                               control = bbb_control(silent=T, branchvar='max', searchdir='best' ))
#     qp_delete(qp_data)
#     sol$value = sol$fmin
#     sol$solution = sol$xmin
  }
  return(rst)
}



optimize.QP.wrap <- function
(
  direction,          # "min" or "max"
  objective.mat,      # objective function: mat part ( 2nd order part )
  objective.vec,      # objective function: vec part ( 1st order part )
  constraints         # constraint list  
) {
  sol <- optimize.QP(  
    objective.mat,
    objective.vec,
    constraints$mat,
    constraints$dir,
    constraints$rhs,
    constraints$binary.vec,
    constraints$lb,
    constraints$ub,
    factorized=FALSE )
  return(sol)
}

#### END Quadratic Programming basic interface
