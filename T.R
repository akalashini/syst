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
  } else {
    if ( is.null(dim(const.mat)) ) dim(const.mat) = c(1, length(const.mat))
   
    # re-organize the constraints
    const.reorg <- p.reorg.constraints(const.mat, const.dir, const.rhs)
    const.mat <- const.reorg$mat
    const.dir <- const.reorg$dir
    const.rhs <- const.reorg$rhs
    meq       <- const.reorg$meq
  }
  if (isnone(lb))   lb <- rep(NA, n)
  if (length(lb) == 1) lb <- rep(lb[1], n)
  if (isnone(ub))   ub <- rep(+Inf, n)
  if (length(ub) == 1) ub <- rep(ub[1], n)
  
  return( list(n = n, mat = const.mat, dir = const.dir, rhs = const.rhs, lb = lb, ub = ub, meq = meq, binary.vec = binary.vec, integer.vec = integer.vec) )
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
  
  if(is.null(constraints)) constraints <- create.new.constraints(n = ncol(const.mat))
  
  if(is.null(dim(const.mat))) const.mat <- p.force.be.matrix(const.mat)
  if(length(const.dir) == 1) const.dir <- rep(const.dir, nrow(const.mat))
  if(length(const.rhs) == 1) const.rhs <- rep(const.rhs, nrow(const.mat))
 
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
  ub = NA
)
{
  # add n variables to constraints already exists
  constraints$mat <- cbind( constraints$mat, matrix(0, nrow(constraints$mat), n, byrow=TRUE) )
  if (isnone(lb)) lb <- rep(NA, n)
  if (length(lb) != n) lb <- rep(lb[1], n)
  if (isnone(ub)) ub <- rep(NA, n)
  if (length(ub) != n) ub = rep(ub[1], n)
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
  set.objfn(lprec, objective.vec)
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
    if(length(lb) == 1) lb = rep(lb, length(lb.vec))
    set.bounds(lprec, lower = lb, columns = lb.vec)
  }
  
  if(!isnone(ub.vec)) {
    if(length(ub) == 1) ub = rep(ub, length(ub.vec))
    set.bounds(lprec, lower = ub, columns = ub.vec)
  }
 
  # ready to optimize
  print(lprec)
  
  status <- solve(lprec)
  if(status==0) {
    cat("LP Optimizing Successful.\n")
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
                     constraint$mat,     
                     constraint$dir,     
                     constraint$rhs,     
                     constraint$binary.vec,
                     constraint$integer.vec,
                     constraint$lb,
                     constraint$lb.vec,  
                     constraint$ub,     
                     constraint$ub.vec )
}

#### max/min return portfolio
lp.obj.portfolio <- function
(
  inp,
  constraints,
  direction = 'min'
)
{
  x <- NA 
  f.obj <- c(inp$expected.return)
  sol = try(optimize.LP(direction, f.obj,
                            constraints$mat,
                            constraints$dir,
                            constraints$rhs, 
                            lb = constraints$lb, 
                            ub = constraints$ub,
                            binary.vec  = constraints$binary.vec,
                            integer.vec = constraints$integer.vec), TRUE)
  if(!inherits(sol, 'try-error')) {
    x = sol$Sol
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
  return( portfolio.return )
}

#### portfolio risk
portfolio.std.risk <- function(
  weight,
  inp
){
  if(is.null(dim(weight))) dim(weight) = c(1, length(weight))
  weight = weight[, 1:inp$n, drop=F]
  cov = inp$cov[1:inp$n, 1:inp$n]
  return( apply(weight, 1, function(x) sqrt(t(x) %*% cov %*% x)) )
}


#### generat efficient portfolio frontier
# inp is a list with the following structure
#   : n
#   : symbols
#   : expected.returns
#   : risk
#   : 
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
  out$weight[1, ]       <- match.fun(min.risk.fn)(inp, constraints)
  
  #constraints$x0 = out$weight[1, ]
  if(npoints > 2) {
    out$return  <- portfolio.weighted.return(out$weight, ia)
    target      <- seq(out$return[1], out$return[npoints], length.out = npoints)
    #base.constr <- add.constraints(c(inp$expected.return, rep(0, ncol(constraints$mat) - inp$n)),
    #                              '', target[1], constraints)
    for(i in 2:(npoints - 1) ) {
      iconstr <- add.constraints(c(inp$expected.return, rep(0, ncol(constraints$mat) - inp$n)),
                                     '=', target[i], constraints)
      #constraints$b[ length(constraints$b) ] = target[i]
      out$weight[i, ] = match.fun(min.risk.fn)(inp, iconstr)
      #constraints$x0 = out$weight[i, ]
    }
         
  }
  
  rm.index <- is.na(apply(out$weight, 1, sum))
  if(any(rm.index)) out$weight <- out$weight[!rm.index, ]
  out$return <- portfolio.weighted.return(out$weight, inp)
  out$risk   <- portfolio.std.risk(out$weight, inp)
  out$name   <- name
  return(out)
}
