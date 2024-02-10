# Linear regression (with intercept)
galton_fit <- lm(childHeight ~ midparentHeight, data=GaltonFamilies)
summary(galton_fit)
round(summary(galton_fit)$coef,3)

# Linear regression (no intercept)
galton_fit <- lm(childHeight ~ 0 + midparentHeight, data=GaltonFamilies)
summary(galton_fit)

# Hand generated R^2
n <- nrow(GaltonFamilies)
p <- 2
X <- GaltonFamilies$midparentHeight
X <- cbind(rep(1, n), GaltonFamilies$midparentHeight)
y <- GaltonFamilies$childHeight
betahat <- solve(t(X)%*%X)%*%t(X)%*%y
betahat # estimate

residual <- y - X%*%betahat
summary(residual) # gives quantiles of residuals

sigmahat <- norm(residual, type="2")/sqrt(n-p)
sigmahat # residual standard error

for(i in 1:p){
  print(sigmahat*sqrt(solve(t(X)%*%X)[i, i])) # standard error
}

for(i in 1:p){
  print(betahat[i]/(sigmahat*sqrt(solve(t(X)%*%X)[i, i]))) # t value, with H0: betai = 0
}

for(i in 1:p){
  print(2*pt(betahat[i]/(sigmahat*sqrt(solve(t(X)%*%X)[i, i])), n-p, lower.tail = FALSE)) # Pr[>|t|] = 0
}
