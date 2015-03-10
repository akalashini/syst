############################
## Unit test code for T.R ##
############################
# checkEquals: Are two objects equal, including named attributes?
# checkEqualsNumeric: Are two numeric values equal?
# checkIdentical: Are two objects exactly the same?
# checkTrue: Does an expression evaluate to TRUE?
# checkException: Does an expression raise an error?

# Using expect_that()
# 
# In general, you can ask expect_that() to test the following conditions:
#   
# is_true: Does the expression evaluate to TRUE?
# is_false: Does the expression evaluate to FALSE?
# is_a: Did the object inherit from a specified class?
# equals: Is the expression equal within numerical tolerance to your expected value?
# is_equivalent_to: Is the object equal up to attributes to your expected value?
# is_identical_to: Is the object exactly equal to your expected value?
# matches: Does a string match the specified regular expression?
# prints_text: Does the text that’s printed match the specified regular expression?
# throws_error: Does the expression raise an error?
# takes_less_than: Does the expression take less than a specified number of seconds to run?

test_splstr <- function()
{
  checkEquals( splstr('AAA,BBB,CCC'), c('AAA','BBB','CCC'))
}

test_isnone <- function()
{
  checkTrue(isnone(NA))
  checkTrue(isnone(NULL))
  checkTrue(isnone(NaN))
}

test_p.force.be.matrix <- function()
{
  vec2Mat_row <- p.force.be.matrix( c(1,2,3), TRUE)
  vec2Mat_col <- p.force.be.matrix( c(1,2,3), FALSE)
  checkIdentical( vec2Mat_row, matrix(c(1,2,3), 1,3, byrow = TRUE))
  checkIdentical( vec2Mat_col, matrix(c(1,2,3), 3,1, byrow = FALSE))
}

test_p.reorg.constraints <- function()
{
  mat <- matrix( c(1,1,1,2,2,2,3,3,3,4,4,4), 4,3, byrow=TRUE)  
  rhs <- c(1,2,3,4)  
  dir <- c("<=","=",">=","<=")
  rst <- p.reorg.constraints(mat, dir, rhs)
  
  target <- list()
  target$mat <- matrix( c(2,2,2,-1,-1,-1,3,3,3,-4,-4,-4), 4,3, byrow=TRUE)
  target$dir <- c("=", ">=", ">=", ">=")
  target$rhs <- c(2, -1, 3, -4)
  target$meq <- 1
  checkEquals( target, rst )
}

test_add.constraints <- function()
{
  mat <- matrix( c(1,1,1,2,2,2,3,3,3,4,4,4), 4,3, byrow=TRUE)  
  rhs <- c(1,2,3,4)  
  dir <- c("<=","=",">=","<=")
  constr <- create.new.constraints( 3, mat, dir, rhs )
  
  mat1 <- matrix(c(5,5,5,6,6,6), 2,3, byrow=TRUE)
  dir1 <- c("<=","=")
  rhs1 <- c(5,6)
  
  const1 <- add.constraints( mat1, dir1, rhs1, constr )
  
  target <- list()
  target$mat <- matrix( c(6,6,6,2,2,2,-5,-5,-5,-1,-1,-1,3,3,3,-4,-4,-4), 6,3, byrow=TRUE)
  target$dir <- c("=", "=", ">=", ">=", ">=", ">=")
  target$rhs <- c(6, 2, -5, -1, 3, -4)
  target$meq <- 2
  checkEquals( target, const1[c("mat","dir","rhs","meq")] )
  
  const2  <- add.variables(2, const1, lb=c(101,102), ub=c(1001,1002))
  target2 <- list()
  target2$n   <- 5
  target2$mat <- matrix( c(6,6,6,0,0,2,2,2,0,0,-5,-5,-5,0,0,-1,-1,-1,0,0,3,3,3,0,0,-4,-4,-4,0,0), 6,5, byrow=TRUE)
  target2$dir <- c("=", "=", ">=", ">=", ">=", ">=")
  target2$rhs <- c(6, 2, -5, -1, 3, -4)
  target2$meq <- 2
  target2$lb  <- c(0,0,0,101,102)
  target2$ub  <- c(Inf,Inf,Inf,1001,1002)
  checkEquals( target2, const2[c("n","mat","dir","rhs","meq","lb","ub")] )
  
}

test_max.return.portfolio <- function()
{
  load("./data//asset_return_risk.RData")
  constr <- create.new.constraints( 8, rep(1,8), "=", 1., lb=0, ub=1)
  wt <- max.return.portfolio( data, constr )
  checkIdentical( wt, c(0,0,0,0,0,0,0,1))
}

