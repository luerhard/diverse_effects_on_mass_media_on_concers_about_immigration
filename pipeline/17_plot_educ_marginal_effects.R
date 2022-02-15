rm(list=ls())


setwd('~/git/migration_news')

library(foreign)
library(ggplot2)
library(dplyr)
library(dotwhisker)

make.brackets <- function(mod) {
  mod$positiv <- ifelse(mod$estimate > 0, 1, 0)
  tmp <- data.table::as.data.table(mod[, c("term", "positiv")])
  get.bracks <- tmp[, .SD[c(1, .N)], by = positiv]
  get.bracks$term <- as.character(get.bracks$term)
  brackets <- list()
  for (i in get.bracks$positiv) {
    if (i == 0) {
      clust.name <- "Decreasing concerns"
    }
    if (i == 1) {
      clust.name <- "Increasing concerns"
    }
    temp.l <-
      c(clust.name, unlist(get.bracks[positiv == i, "term"]))
    brackets[[i + 1]] <- temp.l
  }
  return(brackets)
}

## stata output for margins
df <- foreign::read.dta(file = 'predict_educ_R.dta')

df <- df[, c('_deriv','_at', '_margin', '_se_margin', '_pvalue')]

names(df) <-  c("topicV", "model", "estimate", "std.error", "p.value")

# topic labels
labels <- read.csv("data/lists/Topics_Label_69.csv")
labels$topicV <- paste0("V", as.character(labels$Topics))
labels$term <- paste(labels$topicV, labels$Eng.Label, sep = ".")

mod <- merge(df, labels[, c('topicV', 'term')],
              by.x = 'topicV',
              by.y = 'topicV')

mod$model <- ifelse(mod$model == 1, 'Lower.Education', 'Higher.Education')

mod <- mod[order(-mod$estimate),]

dwplot(mod,
       conf.level = .83,
       dot_args = list(aes(colour = model, shape = model)),
       whisker_args = list(aes(colour = model))) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  labs(x = 'Average Marginal Effects', y = '') +
  theme_minimal() +
  # theme(legend.position = 'None') +
  scale_color_grey(start = 0.2, end = 0.7)


# brackets <- make.brackets(mod)

# Create list of brackets (label, topmost included predictor, bottommost included predictor)
brackets <- list(c("Increasing Concerns", "V54.Integration: Domestic Violence", "V24.Romanies"), 
                       c("Decreasing Concerns", "V3.Family", "V18.Studies"))


pdf('vis/regression/Marginal_Education.pdf', onefile=FALSE)
{dwplot(mod,
       conf.level = .83,
       dot_args = list(aes(colour = model, shape = model)),
       whisker_args = list(aes(colour = model))) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  labs(x = 'Average Marginal Effects', y = '') +
  theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
  scale_color_grey(start = 0.2, end = 0.7)} %>% 
  add_brackets(brackets)
dev.off()
