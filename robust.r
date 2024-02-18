lm7 <- lm(sum ~ un_per_l*election_av  + gdp_r_wk +gg_oecd_l + libor_l + mg_l + factor(country) + factor(year), data=df)
hats <- as.data.frame(hatvalues(lm7)) # if hat = 1, then watch out

rownames(df) <- 1:nrow(df) # reindex rows
df <- df[-c(86, 118, 142),] # remove problematic rows

lmm7 = sqrt(diag(hccm(lm7, type="hc0")))
