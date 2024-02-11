lalonde_full <- lm(re78 ~ ., data=lalonde)
lalonde_treat <- lm(re78 ~ treat, data=lalonde)
lalonde_treat_e_b <- lm(re78 ~ treat + educ + black, data=lalonde)

# ANOVA compares models against each other in sequential order
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/anova
anova(lalonde_treat, lalonde_full, lalonde_treat_e_b)
anova(lalonde_treat, lalonde_treat_e_b, lalonde_full)
anova(lalonde_treat, lalonde_treat_e_b)
