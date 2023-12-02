# selecting the top important variables
library(ggplot2)
library(dplyr)

# saving the best bayesian model output
dif_ci <- summary_model_lasso$CI.coef |> 
  apply(1,function(x){x[2]- x[1]})

df_ci <- data.frame(name = rownames(summary_model_lasso$mu.coef),
                    coef = summary_model_lasso$mu.coef,
                    rank = summary_model_lasso$rank,
                    ci1 = summary_model_lasso$CI.coef[,1],
                    ci2 = summary_model_lasso$CI.coef[,2],
                    dif_ci= dif_ci)
# summary and sorting the output of selected bayesian model---
head(df_ci)
df2_ci <- df_ci[order(df_ci$rank),]
head(df2_ci)
sum(df2_ci$dif_ci>1.60e-5)


df3_ci <- df2_ci
# sorting varaibels to plot ---------
df3_ci$rank2 <- factor(df3_ci$name,
                       labels = df3_ci$name,
                       levels = df3_ci$name[order(df3_ci$rank)]
)
# head(df3_ci)

df3_ci$rank3 <- factor(df3_ci$name,
                       labels = df3_ci$name,
                       levels = df3_ci$name[order(order(df3_ci$dif_ci))]
)

# plotting the top largest credible intervals --------
ggplot(df3_ci[1:20,])+
  geom_segment(aes(x = ci1, 
                   xend = ci2, 
                   y = reorder(rank2, desc(rank2)),
                   yend = reorder(rank2, desc(rank2))))+
  geom_point(aes(x = ci1,y =reorder(rank2, desc(rank2))),size = 1,color = "blue") +
  geom_point(aes(x = ci2,y = reorder(rank2, desc(rank2))),color = "red",size= 1)+
  theme_classic()


# top 10 variable Coefficients(mean of posterior distribution) -------
library(ggplot2)
ggplot(df3_ci[1:10,])+
  geom_segment(aes(x = 0,
                   xend = coef,
                   y = reorder(rank2, desc(rank2)),
                   yend = reorder(rank2, desc(rank2))))+
  geom_point(aes(x = coef,y =reorder(rank2, desc(rank2))),size = 1,color = "blue") +
  geom_text(aes(x = -0.07,y = reorder(rank2,desc(rank2)),label = round(coef,4)),size = 2)+
  ylab("Selected Variables")+
  theme_classic()

## summary of the coefficients of the best bayesian model
summary_model_lasso$mu.coef[order(abs(summary_model_lasso$mu.coef),decreasing = TRUE),] |> 
  abs() |> 
  round(2) |>  
  table()


# choosing the best variables------------
final_data <- data.frame(val = df3_ci$coef[order(abs(df3_ci$coef),decreasing = TRUE)],
                         n = df3_ci$name[order(abs(df3_ci$coef),decreasing = TRUE)]
)
# using plot
val <- final_data$val
names(val) <- final_data$n
barplot(val)
abline(h = c(-0.02,0.02),col = "red")

val <- final_data$val

names(val) <- final_data$n
barplot(abs(val),main= "abs value of mean of posteriors")
abline(h = c(0.02),col = "red")

# using frequency
val <- round(val,2)
c(val[val < -0.02],val[val > 0.02])
c(val[val < -0.02],val[val > 0.02]) |> length()