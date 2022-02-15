rm(list = ls())

library(xlsx)
library(stm)
library(optparse)

args <- commandArgs(trailingOnly = TRUE)

parser <- OptionParser()
parser <- add_option(parser, c("--ntopics"), type = "integer")
args <- parse_args(parser, args = args)
ntopics <- args$ntopics

load("data/rdata/processed.RData")

control_parameters <- list(
  maxV = floor(length(data$vocab) * 0.1), # number of words used for spectral initialization
  tSNE_init.dims = 100, # dimensions for tSNE approach, when K=0
  gamma.enet = 0 # prevalence penalty parameter 0=L2-norm, 1=L1-norm, inbetween = a mixture called elastic net
)

model <- stm::stm(
  documents = data$documents, # docs from data creation
  vocab = data$vocab, # vocab from data creation
  data = data$meta, # dataframe with all the metadata
  init.type = "Spectral", # Spectral in general best (gives deterministic results and the Lee/Mimno t-SNE estimation if K=0). Random and LDA are alternatives.
  K = ntopics, # Number of topics
  prevalence = ~ s(week, 780) + zeitung, # topic prevalence formula
  content = NULL, # topic content covariate
  interactions = FALSE, # allow interactions between content covariates and latent topics
  max.em.its = 250, # max iterations
  emtol = 1e-5, # convergence criterion von E-M-Algorithm (default = 1e-05)
  gamma.prior = "L1", # estimation method for topic prevalence can be L1 or Pooled -> switch to L1 if slow.
  sigma.prior = 0.1, # regularization value betw. 0 and 1 for the half-cauchy Pooled gamma.prior
  reportevery = 10, # topic short info during estimation after every n iterations.
  verbose = TRUE,
  control = control_parameters # slot for additional parameters (see above)
)

save(model, file = paste0("models/stmobjects/model_k_", model$setting$dim$K, ".modelR"))


topics <- labelTopics(model, n = 5)

df <- data.frame()

for (index in 1:length(topics$topicnums)) {
  prob_str <- paste(topics$prob[index, ], collapse = ", ")
  frex_str <- paste(topics$frex[index, ], collapse = ", ")
  lift_str <- paste(topics$lift[index, ], collapse = ", ")
  score_str <- paste(topics$score[index, ], collapse = ", ")

  df <- rbind(df, c(prob_str, frex_str, lift_str, score_str), stringsAsFactors = FALSE)
}
colnames(df) <- c("Prob", "frex", "lift", "score")

write.xlsx(df, paste0("models/topiclists/output_k_", model$setting$dim$K, ".xlsx"))
