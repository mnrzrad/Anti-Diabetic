# variable selection using Linear regression

df_top_vars <- data.frame(target = df_standard$target,
                   GATS6m  = df_standard$GATS6m,
                   ITH = df_standard$ITH,
                   F06.C.O. = df_standard$F06.C.O.,
                   EEig11d =df_standard$EEig11d,
                   RDF080m = df_standard$RDF080m,
                   T.N..S. = df_standard$T.N..S.,
                   RDF080p = df_standard$RDF080p,
                   GATS3m = df_standard$GATS3m,
                   G.N..S. = df_standard$G.N..S.,
                   RDF080v  = df_standard$RDF080v
)
final_model <- lm(target~.-1,data = df_top_vars) 
summary(final_model)

# null model
final_null_model <- lm(target~-1,data  = df_top_vars)


# stepwise ---------
# bothside
step(final_null_model,scope = list(lower= final_null_model,upper = final_model), 
     direction = "both",k = log(nrow(df_top_vars)),trace = TRUE)

# forward
step(final_null_model,scope = list(lower= final_null_model,upper = final_model),
     direction = "forward",k = log(nrow(df_top_vars)))

# backward
s_final_model <- step(final_model,direction = "backward",
                      k = log(nrow(df_top_vars)))
# all the models converged to the same model.
# so:
s_final_model |> summary()

df_final_vars <- data.frame(target = df_standard$target,
                   GATS6m  = df_standard$GATS6m,
                   ITH = df_standard$ITH,
                   F06.C.O. = df_standard$F06.C.O.,
                   #EEig11d =df_standard$EEig11d,
                   RDF080m = df_standard$RDF080m,
                   # T.N..S. = df_standard$T.N..S.,
                   RDF080p = df_standard$RDF080p,
                   #GATS3m = df_standard$GATS3m,
                   G.N..S. = df_standard$G.N..S.
                   # RDF080v  = df_standard$RDF080v
)
final_model_parametric <- lm(target~.-1,data = df_final_vars) 
summary(final_model_parametric)

par(mfrow = c(1,1))
plot(final_model_parametric,which = 1)
plot(final_model_parametric,which = 2)
plot(final_model_parametric,which = 3)
plot(final_model_parametric,which = 4)

shapiro.test(residuals(final_model_parametric))

lmtest::dwtest(final_model_parametric)
