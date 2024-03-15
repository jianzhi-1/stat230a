n = 1000
mat = t(matrix(rnorm(n*3, mean=c(1, 0, 0), sd=c(0, 1, 1)), 3, n))
beta = c(1, 2, 3) 
epsilon = rnorm(n, mean=0, sd=1)
y = as.vector(mat%*%beta + epsilon)
# true model yi = 1 + 2X1 + 3X2 + N(0, i^2)

lm(y ~ mat[,2])$coef
lm(y ~ mat[,2] + mat[,3])$coef

# ----------

set.seed(42)
n = 10
mat = t(matrix(rnorm(n*3, mean=c(1, 0, 0), sd=c(0, 1, 1)), 3, n))
beta = c(1, 2, 3) 
sigma = (1:n)^5
epsilon = rnorm(n, mean=0, sd=sigma)
y = as.vector(mat%*%beta + epsilon)
# true model yi = 1 + 2X1 + 3X2 + N(0, i^2)

# var[beta hat]
var.beta.hat = sum((lm(mat[,2] ~ mat[,3])$res^2)*(sigma^2))/(sum(lm(mat[,2] ~ mat[,3])$res^2)^2)

# var[beta tilde]
var.beta.tilde = sum((lm(mat[,2] ~ 1)$res^2)*(sigma^2))/(sum(lm(mat[,2] ~ 1)$res^2)^2)

c(var.beta.hat, var.beta.tilde)
