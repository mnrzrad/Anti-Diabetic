
# Bayesian models ---------------------------------------------------------

# Horseshoe
model_hs  <- bayesreg(target~.,
                   data = df_standard,
                   model = "gaussian",
                   prior = "hs",
                   burnin = 10000,
                   thin = 5,
                   n.samples = 20000)

#HorseShoe+
model_hsplus <- bayesreg(target~.,
                   data = df_standard,
                   model = "gaussian",
                   prior = "hs+",
                   burnin = 10000,
                   thin = 5,
                   n.samples = 20000)

# Lasso
model_lasso <- bayesreg(target~.,
                   data = df_standard,
                   model = "gaussian",
                   prior = "lasso",
                   burnin = 10000,
                   thin = 5,
                   n.samples = 20000)

# Outputs -----------
# density plot of the posteriors   --------
plot(density(df_standard$target),col = "black", ylim = c(0,0.5))
lines(density(model_hs$yhat),col = "red")
lines(density(model_hsplus$yhat),col="blue")
lines(density(model_lasso$yhat),col="green")

# model summary ------------
summary_model_hs     <- summary(model_hs, CI = 0.99)
summary_model_hsplus <- summary(model_hsplus, CI = 0.99)
summary_model_lasso  <- summary(model_lasso, CI = 0.99)

# selecting best model
# waic
cat('WAIC of hs: ', summary_model_hs$waic, '\n')
cat('WAIC of hsplus: ', summary_model_hsplus$waic, '\n')
cat('WAIC of lasso: ', summary_model_lasso$waic, '\n')


# r2
cat('R2 of hs: ',summary_model_hs$r2, '\n')
cat('R2 of hsplus: ', summary_model_hsplus$r2, '\n')
cat('R2 of lasso: ', summary_model_lasso$r2, '\n')


# distance of confidence interval ----------
# number of variables that has zero in their credible interval
sum(apply(summary_model_lasso$CI.coef,1,prod)<0)