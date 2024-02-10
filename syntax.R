Simu1 <- replicate(5000, {
  y = xbeta + rnorm (n)
  ols.fit = lm(y ~ x)
  c( summary ( ols.fit )$coef[2, 1:2],sqrt (hccm (ols.fit )[2, 2 ]))
})
