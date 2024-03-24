library(bayesreg)
library(data.table)

# loading data ------------------------------------------------------------


df <- fread("data_ch.csv")
df |> 
  dim()


# preprocessing -----------------------------------------------------------
df <- as.data.frame(df) 

df |> 
  dim()

# number of NA
df |> 
  sapply(function(x){is.na(x)}) |>
  sum()


# removing variables with variance less than 0.05
numerical_columns <- df[3:3226]
variances <- sapply(df[3:3226], var)
selected_numerical_columns <- numerical_columns[variances > 0.05]

first_two_columns <- df[1:2]
df <- cbind(first_two_columns, selected_numerical_columns)
# add dimension
dim(df) 


# preprocessing using table of frequency
tbl_uniques <- df |> 
  sapply(function(x){length(unique(x))})
table(tbl_uniques)

df <- df[tbl_uniques>2]
# removing No. and MolID
df <- df[,-c(1,2)]
#heatmap(cor(df))
n <- names(df)

# assigning target value
df$target <-  c(91.3,106.5,92.8,94.2,103.4,91.3,102.5,101.6,91.1,103.2,108.8,105.8,
                104.2,107.4,104.3,97.7,105.3,105.8,94.6,98.9,100.3,95.6,103.2,96.4,
                97.4,103.3,100.5)

# standardizing
df_standard <- lapply(df,function(x){
  (x - mean(x))/sd(x)
}
) |>
  as.data.frame()
