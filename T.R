# private tool for asset allocation research
# by Jian Tang 2015
# all rights reserved
# packages needed to use this module:
# lpSolveAPI 

##################################################################################
# some convenient functions
##################################################################################

splstr <- function(s, delim = ',')
{
  # split a string and return a vector
  return(unlist(strsplit(s,delim)))
}

isnone <- function(var) {
  # if var is a non-valid object then return true, such as NA,NULL,FALSE
  if (len(var) == 0)
    return(TRUE)
  if (len(var) == 1) {
    if(is.na(var) || is.null(var) || var == FALSE || var == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

iif <- function (cond, truepart, falsepart) {
  # if cond is TRUE then return true part else return falsepart
  return(ifelse(isnone(cond), falsepart, truepart))
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

####
p.reorg.constraints <- function 
(
  const.mat,
  const.dir,
  const.rhs
)
{
  # let the first constraints to be "=", the rest to be ">=", to comply wiht QP package
  # TESTED
  
  if (is.null(dim(const.mat))) dim(const.mat) <- c(1, len(const.mat))
  
  # get constraints with "=" first
  meq   <- sum(const.dir == "=")
  mineq <- len(const.dir) - meq
  
  eq.const.mat <- const.mat[const.dir == "=", ]
  eq.const.rhs <- const.rhs[const.dir == "="]
  
  ineq.const.mat <- const.mat[!(const.dir == "="), ]
  ineq.const.rhs <- const.rhs[!(const.dir == "=")]
  
  # for QP, need to convert constraint to >= mode
  ineq.const.mat[dir == "<=", ] <- - ineq.const.mat[dir == "<=", ]
  ineq.const.rhs[dir == "<="] <- - ineq.const.rhs[dir == "<="]
  
  anew.mat <- rbind(eq.const.mat, ineq.const.mat)
  anew.dir <- c(rep("=", meq), rep(">=", mineq))
  anew.rhs <- c(eq.const.rhs, ineq.const.rhs)
  
  rst <- list(mat = anew.mat, dir = anew.dir, rhs = anew.rhs, meq = meq )
  return(rst)
}


# create.new.constraints <- function
# (
#   n,              # n variables
#   const.mat   = NULL,
#   const.dir   = NULL,
#   const.rhs   = NULL,
#   lb = NA,
#   ub = NA
# )
# {
#   meq <- 0
#   if ( isnone(const.mat) || isnone(const.rhs) ) {
#     const.mat <- matrix(0, 0, n)
#     const.rhs <- c()
#   } else {
#     if ( is.null(dim(const.mat)) ) dim(const.mat) = c(1, len(const.mat))
#    
#     # get constraints with "=" first
#     
#     # for QP, need to convert constraint to >= mode
#     const.mat[dir == "<=",] <- - const.mat[dir == "<=",]
#     const.rhs[dir == "<=",] <- - const.rhs[dir == "<=",]
#   }
#   if ( is.null(lb) || is.na(lb) ) lb = rep(NA, n)
#   if ( len(lb) != n ) lb = rep(lb[1], n)
#   if ( is.null(ub) || is.na(ub) ) ub = rep(NA, n)
#   if ( len(ub) != n ) ub = rep(ub[1], n)
#   return( list(n = n, A = A, b = b, meq = meq, lb = lb, ub = ub) )
# }
# add.constraints <- function
# (
#   A,
#   b,
#   type = c('=', '>=', '<='),
#   constraints
# )
# {
#   if(is.null(constraints)) constraints = new.constraints(n = nrow(A))
#   if(is.null(dim(A))) A = matrix(A)
#   if(len(b) == 1) b = rep(b, ncol(A))
#   if ( type[1] == '=' ) {
#     constraints$A = cbind( A, constraints$A )
#     constraints$b = c( b, constraints$b )
#     constraints$meq = constraints$meq + len(b)
#   }
#   if ( type[1] == '>=' ) {
#     constraints$A = cbind( constraints$A, A )
#     constraints$b = c( constraints$b, b )
#   }
#   if ( type[1] == '<=' ) {
#     constraints$A = cbind( constraints$A, -A )
#     constraints$b = c( constraints$b, -b )
#   }
#   return( constraints )
# }
# add.variables <- function
# (
#   n,
#   constraints,
#   lb = NA,
#   ub = NA
# )
# {
#   constraints$A = rbind( constraints$A, matrix(0, n, len(constraints$b)) )
#   if ( is.null(lb) || is.na(lb) ) lb = rep(NA, n)
#   if ( len(lb) != n ) lb = rep(lb[1], n)
#   if ( is.null(ub) || is.na(ub) ) ub = rep(NA, n)
#   if ( len(ub) != n ) ub = rep(ub[1], n)
#   constraints$lb = c(constraints$lb, lb)
#   constraints$ub = c(constraints$ub, ub)
#   constraints$n = constraints$n + n
#   return( constraints )
# }
# delete.constraints <- function
# (
#   delete.index,
#   constraints
# )
# {
#   constraints$A = constraints$A[, -delete.index, drop=F]
#   constraints$b = constraints$b[ -delete.index]
#   constraints$meq = constraints$meq - len(intersect((1:constraints$meq), delete.index))
#   return( constraints )
# }
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
  
  n <- len(objective.vec)
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
    if(len(lb) == 1) lb = rep(lb, len(lb.vec))
    set.bounds(lprec, lower = lb, columns = lb.vec)
  }
  
  if(!isnone(ub.vec)) {
    if(len(ub) == 1) ub = rep(ub, len(ub.vec))
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
} #END solve.LP


#### max/min return portfolio
lp.obj.portfolio <- function
(
  inp,
  constraints,
  f.obj = c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)),
  direction = 'min'
)
{
  x = NA
  binary.vec = 0
  if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
  sol = try(solve.LP.bounds(direction, f.obj,
                            t(constraints$A),
                            c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq)),
                            constraints$b, lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec), TRUE)
  if(!inherits(sol, 'try-error')) {
    x = sol$solution
  }
  return( x )
}


#### generat efficient portfolio frontier
# efficient.port.gen <- function( 
#   inp, 
#   min.risk.fn,  
#   name = 'Risk',
#   npoints = 25
#   )
# {
#   # first find max return portfolio
#   
#   const.mat,           # constraint matrix, each row is a constraint
#   const.dir,           # direction, "=", ">=" "<=" etc
#   const.rhs,           # rhs
#   binary.vec  = NA,    # index of which vars are binary
#   integer.vec = NA,    # index of which vars are integers
#   lb     = 0,          # lower bound
#   lb.vec = NA,         # what vars to set lower bound
#   ub     = +Inf,       # upper bound
#   ub.vec = NA    
#   
# }
# load.packages('quadprog,corpcor,lpSolve,kernlab')
# if( is.null(constraints) ) {
#   constraints = new.constraints(rep(0, ia$n), 0, type = '>=')
# }
# ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
# if( is.null(ia$cov) ) ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
# ia$cov.temp = ia$cov
# n0 = ia$n
# n = nrow(constraints$A)
# if( n != nrow(ia$cov.temp) ) {
#   temp =  matrix(0, n, n)
#   temp[1:n0, 1:n0] = ia$cov.temp[1:n0, 1:n0]
#   ia$cov.temp = temp
# }
# if(!is.positive.definite(ia$cov.temp, method = 'chol')) {
#   ia$cov.temp <- make.positive.definite(ia$cov.temp, 0.000000001)
# }
# if(nportfolios<2) nportfolios = 2
# out = list(weight = matrix(NA, nportfolios, nrow(constraints$A)))
# colnames(out$weight) = rep('', ncol(out$weight))
# colnames(out$weight)[1:ia$n] = ia$symbols
# out$weight[nportfolios, ] = max.return.portfolio(ia, constraints)
# out$weight[1, ] = match.fun(min.risk.fn)(ia, constraints)
# constraints$x0 = out$weight[1, ]
# if(nportfolios > 2) {
#   out$return = portfolio.return(out$weight, ia)
#   target = seq(out$return[1], out$return[nportfolios], length.out = nportfolios)
#   constraints = add.constraints(c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)),
#                                 target[1], type = '>=', constraints)
#   for(i in 2:(nportfolios - 1) ) {
#     constraints$b[ len(constraints$b) ] = target[i]
#     out$weight[i, ] = match.fun(min.risk.fn)(ia, constraints)
#     constraints$x0 = out$weight[i, ]
#   }
#   if( equally.spaced.risk ) {
#     out$risk = portfolio.risk(out$weight, ia)
#     temp = diff(out$risk)
#     index = which(temp >= median(temp) + mad(temp))
#     if( len(index) > 0 ) {
#       index = min(index)
#       proper.spacing = ceiling((out$risk[nportfolios] - out$risk[index])/temp[(index-1)])-1
#       nportfolios1 = proper.spacing + 2
#       if(nportfolios1 > 2) {
#         out$return = portfolio.return(out$weight, ia)
#         out$risk = portfolio.risk(out$weight, ia)
#         temp = spline(out$risk, out$return, n = nportfolios, method = 'natural')
#         target = temp$y[ which(temp$y > out$return[index] & temp$y < out$return[nportfolios] &
#                                  temp$x > out$risk[index] & temp$x < out$risk[nportfolios])]
#         target = c(out$return[index], target, out$return[nportfolios])
#         nportfolios1 = len(target)
#         out1 = list(weight = matrix(NA, nportfolios1, nrow(constraints$A)))
#         out1$weight[1, ] = out$weight[index, ]
#         out1$weight[nportfolios1, ] = out$weight[nportfolios, ]
#         constraints$x0 = out1$weight[1, ]
#         for(i in 2:(nportfolios1 - 1) ) {
#           constraints$b[ len(constraints$b) ] = target[i]
#           out1$weight[i, ] = match.fun(min.risk.fn)(ia, constraints)
#           constraints$x0 = out1$weight[i, ]
#         }
#         out$weight = rbind(out$weight[-c(index:nportfolios),], out1$weight)
#       }
#     }
#   }
# }
# rm.index = is.na(rowSums(out$weight))
# if(any(rm.index)) out$weight = out$weight[!rm.index,]
# out$return = portfolio.return(out$weight, ia)
# out$risk = portfolio.risk(out$weight, ia)
# out$name = name
# return(out)
# }