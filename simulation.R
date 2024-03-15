n = 1000
mat = t(matrix(rnorm(n*3, mean=c(1, 0, 0), sd=c(0, 1, 1)), 3, n))
beta = c(1, 2, 3) 
epsilon = rnorm(n, mean=0, sd=1)
y = as.vector(mat%*%beta + epsilon)
# true model yi = 1 + 2X1 + 3X2 + N(0, i^2)

lm(y ~ mat[,2])$coef
lm(y ~ mat[,2] + mat[,3])$coef
