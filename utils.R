###############################################################################
######## UTIL FUNCTIONS
###############################################################################
repmat <- function
(
  v,
  n,
  m
)
{
  # repeat a matrix by n X m 
  if( is.null(dim(v)) ) dim(v) <- c(1, length(v))
  kronecker( matrix(1, n, m), v )
}

