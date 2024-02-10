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

ybar <- mean(y)
yhat <- X%*%betahat
R2 <- sum((yhat - ybar)^2)/sum((y - ybar)^2)
R2 # multiple R-squared
adjR2 <- (1 - (1 - R2)*(n-1)/(n-p))
adjR2 # adjusted R-squared

C <- t(rbind(rep(0, p - 1), diag(p - 1)))
Cb <- C%*%betahat
Fstat <- Cb%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%t(Cb)/(sigmahat^2)
Fstat # F-statistic
pf(Fstat, p-1, n-p, lower.tail=F) # P[F > f]