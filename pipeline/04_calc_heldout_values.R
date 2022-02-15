
library(stm)

load("data/dataset_final1.RData")

heldout <- make.heldout(data$documents,
  data$vocab,
  N = floor(length(data$documents) * 0.2),
  proportion = 0.5,
  seed = 1234
)

all_models <- list.files("data/heldout_models", pattern = "*.RData")


exp_heldout <- vector(mode = "numeric", length = length(all_models))
residuals <- vector(mode = "numeric", length = length(all_models))
k_values <- vector(mode = "numeric", length(all_models))


for (i in 1:length(all_models)) {
  modelname <- all_models[i]

  load(paste0("data/heldout_models/", modelname))

  result <- regmatches(modelname, regexec("model_k_([0-9]+).RData", modelname))
  k <- as.integer(result[[1]][2])
  gc()

  k_values[i] <- k
  exp_heldout[i] <- eval.heldout(model, heldout$missing)$expected.heldout
  residuals[i] <- checkResiduals(model, heldout$documents)$dispersion
}

df <- data.frame(k_values, exp_heldout, residuals)

save(df, file = "result_df.RData")
