# case study of linear-programming
# maximize :
# x1 + 9 * x2 + x3, subject to 
# x1 + 2 * x2 + 3 * x3 <= 9
# 3 * x1 + 2 * x2 + 2 * x3 <= 15
library('lpSolve')
f.obj <- c(1, 9, 1)
f.con <- matrix(c(1, 2, 3, 3, 2, 2), nrow = 2, byrow = TRUE)
f.dir <- c('<=', '<=')
f.rhs <- c(9, 15)
sol <- lp('max', f.obj, f.con, f.dir, f.rhs )