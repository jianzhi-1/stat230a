X <- matrix(rnorm(7*3), 7, 3)
qrX <- qr(X)
qr.Q(qrX)
qr.R(qrX)

Y <- rnorm (7)
lmfit <- lm(Y ~ 0 + X, qr=T)
qr.Q(lmfit$qr)
qr.R(lmfit$qr)
