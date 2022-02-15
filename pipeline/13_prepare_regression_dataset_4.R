library(foreign)

# dataset
load("data/rdata/reg_data_inclMonth_4.RData")

source("pipeline/data_preparation_func.R")
df <- prepare_df(df)

save(df, file = "data/rdata/final_regression_df.RData")
foreign::write.dta(df, file = "data/datasets_final/final_regression_df.dta")

