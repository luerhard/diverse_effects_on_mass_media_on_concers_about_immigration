rm(list=ls())

library(stm)
library(optparse)

args = commandArgs(trailingOnly=TRUE)

parser = OptionParser()
parser = add_option(parser, c("--ntopics"), type="integer")
args = parse_args(parser, args=args)
ntopics=args$ntopics

#To reproduce heldout dataset
# load("/home/lukas/git/masterarbeit/data/rdata/processed.RData")
# heldout = make.heldout(data$documents,
#                        data$vocab,
#                        N=floor(length(data$documents) * 0.1),
#                        proportion = 0.4,
#                        seed = 42
# )
# save(heldout, file="/home/lukas/git/masterarbeit/data/rdata/heldoutdata.RData")

load("data/rdata/heldoutdata.RData")
load("data/rdata/processed.RData")

control_parameters = list(
  maxV = floor(length(heldout$vocab) * 0.1),  # number of words used for spectral initialization
  tSNE_init.dims = 100,                     # dimensions for tSNE approach, when K=0
  gamma.enet = 0                            # prevalence penalty parameter 0=L2-norm, 1=L1-norm, inbetween = a mixture called elastic net
)

model = stm::stm(
  documents = heldout$documents,               # docs from data creation
  vocab = heldout$vocab,                       # vocab from data creation
  data = data$meta,                         # dataframe with all the metadata
  init.type = "Spectral",                   # Spectral in general best (gives deterministic results and the Lee/Mimno t-SNE estimation if K=0). Random and LDA are alternatives.
  K = ntopics,                              # Number of topics
  prevalence = ~ s(week, 780) + zeitung,    # topic prevalence formula
  content = NULL      ,                     # topic content covariate
  interactions = FALSE,                     # allow interactions between content covariates and latent topics
  max.em.its = 250,                         # max iterations
  emtol = 1e-5,                             # convergence criterion von E-M-Algorithm (default = 1e-05)
  gamma.prior = "L1",                       # estimation method for topic prevalence can be L1 or Pooled -> switch to L1 if slow.
  sigma.prior = 0.1,                        # regularization value betw. 0 and 1 for the half-cauchy Pooled gamma.prior
  reportevery = 10,                         # topic short info during estimation after every n iterations.
  verbose = TRUE,
  control = control_parameters              # slot for additional parameters (see above)
)

save(model, file=paste0("models/heldoutobjects/model_k_", model$setting$dim$K, ".modelR"))


