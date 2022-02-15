rm(list = ls())
# _v5: after revision, only change are nicer labels; dropped some outcommented preprocessing steps of _v4
# setwd to toplevel git directory

options("scipen" = 50, "digits" = 3)

library(foreign)
library(dotwhisker)
library(lfe)
library(dplyr)

# DEFINE CONSTANTS

# weeks <- c(2, 4:8) # c(2,4,8)
week <- 4

# LOAD DATA

# dataset
load("data/final_datasets/final_regression_df.RData")


topics <- grep("^V", names(df), value = TRUE)

# topic labels
labels <- read.csv("data/lists/Topics_Label_69.csv")
labels$topicV <- paste0("V", as.character(labels$Topics))
labels$name <- paste(labels$topicV, labels$Eng.Label, sep = ".")


# FUNCTIONS
caterpillar <- function(model, for.topics, sign.level, week) {
  # new labels for some effects, after R&R
  mod <- data.frame(summary(model, robust = T)$coefficients)
  mod$term <- row.names(mod)
  colnames(mod) <-
    c("estimate", "std.error", "statistic", "p.value", "term")

  if (for.topics == T) {
    # plot only TOPICS
    mod <- mod[mod$term %in% topics, ]
    # attach labels
    mod <-
      merge(mod, labels[, c("topicV", "name")], by.x = "term", by.y = "topicV")
    # labels as terms
    mod$term <- mod$name
  }
  if (for.topics == F) {
    # plot NOT only topics
    mod <-
      mod[!(mod$term %in% topics) &
        !(mod$term %in% paste("pmonin", unique(df$pmonin), sep = "")), ]
  }
  # attach weeks [only plotted if diff values]
  # mod$model <- paste('Week', week, sep = '.')

  # filter only significant topics
  # mod$p.value <- p.adjust(mod$p.value, 'bonferroni') #bonferroni - most conservative correction - changes stuff, holm doesnt
  mod <- mod[mod$p.value < sign.level, ]
  # order by est
  mod <- mod[order(-mod$estimate), ]
  return(mod)
}

# brackets: all < or > 0
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


covariates <- c(
  "party",
  "political.Interest",
  "income.Satisfact",
  "worried.About.Econ",
  "n.Articles",
  "pmonin"
)

# formula: non-topic vars
model <- as.formula(paste(
  "AV.bin ~ ",
  paste(covariates, collapse = " + "),
  "| fac.pid",
  collapse = ""
))


# formula: with topics
model.topics <- as.formula(paste(
  "AV.bin~",
  paste(covariates, collapse = " + "),
  " + ",
  paste(topics, collapse = " + "),
  "| fac.pid",
  collapse = ""
))

## run models
lfe.bin <- felm(model,
  data = df
)


lfe.topics <- felm(model.topics,
  data = df
)


## make PLOTs

mod <-
  caterpillar(
    lfe.topics,
    for.topics = T,
    sign.level = 0.01,
    week = week
  )
brackets <- make.brackets(mod)

pdf(paste("vis/Caterpillar_Week_", week, ".pdf", sep = ""))
{
  dwplot(
    mod,
    conf.level = .83,
    dot_args = list(size = 2, color = "grey20"),
    # , shape = 17
    whisker_args = list(color = "grey60")
  ) +
    geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = 2
    ) +
    labs(x = "Coefficients", y = "") +
    theme_minimal() +
    theme(legend.position = "None")
} %>%
  add_brackets(brackets)
dev.off()

## select only few highest/lowest topics
mod.select <- rbind(
  head(mod, 5),
  tail(mod, 5)
)
brackets.select <- make.brackets(mod.select)

pdf(paste(
  "vis/Caterpillar_Week_Selected",
  week,
  ".pdf",
  sep = ""
))
{
  dwplot(
    mod.select,
    conf.level = .83,
    dot_args = list(size = 2, color = "grey20"),
    # , shape = 17
    whisker_args = list(color = "grey60")
  ) +
    geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = 2
    ) +
    labs(x = "Coefficients", y = "") +
    theme_minimal() +
    theme(legend.position = "None")
} %>%
  add_brackets(brackets.select)
dev.off()


## only STRUCTURAL effects
mod <-
  caterpillar(
    lfe.topics,
    for.topics = F,
    sign.level = 0.01,
    week = week
  )
brackets <- make.brackets(mod)


pdf(paste(
  "vis/Caterpillar_onlyStrucVars_Week_",
  week,
  ".pdf",
  sep = ""
))
{
  dwplot(
    mod,
    conf.level = .83,
    dot_args = list(size = 2, color = "grey20"),
    # , shape = 17
    whisker_args = list(color = "grey60")
  ) +
    geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = 2
    ) +
    labs(x = "Coefficients", y = "") +
    theme_minimal() +
    theme(legend.position = "None")
} %>%
  add_brackets(brackets)
dev.off()


############################
# # INTERACTIONS
# ############################
# get signi topics
mod <-
  caterpillar(
    lfe.topics,
    for.topics = T,
    sign.level = 0.01,
    week = week
  )
gtools::mixedsort(mod$term)

# top.topics <- c('V1', 'V3', 'V10', 'V12', 'V18', 'V21', 'V23', 'V27', 'V29', 'V39', 'V40', 'V45', 'V46', 'V49', 'V50', 'V52', 'V54', 'V55', 'V60', 'V62', 'V64', 'V65', 'V66')

# mod.select <- rbind(head(mod, 5),
#                     tail(mod, 5))
# gtools::mixedsort(mod.select$term)


## interaction w/ EDUC Dummy

select.topics <-
  c("V54", "V45", "V61", "V34", "V24", "V18", "V29", "V64", "V3", "V21")

model.topics <- as.formula(paste(
  "AV.bin~",
  paste(covariates, collapse = " + "),
  " + ",
  paste(select.topics, "*educ.bin", collapse = " + "),
  "| fac.pid",
  collapse = ""
))


lfe.educ <- felm(model.topics,
  data = df
)

# report ROBUST stand.errors ("sandwich", HC3)
models <- list(lfe.bin, lfe.topics, lfe.educ)

# make TABLES
se.v <- list("")
for (i in 1:length(models)) {
  se.v[[i]] <-
    as.vector(summary(models[[i]], robust = T)$coefficients[, 2])
}

stargazer::stargazer(
  lfe.bin,
  lfe.topics,
  lfe.educ,
  se = se.v,
  star.cutoffs = c(0.05, 0.01, 0.001),
  dep.var.labels = c("Being very concerned"),
  type = "text",
  out = paste("vis/reg_table.tex", sep = "")
)


### Reverse AV
## df like for normal models
# 1 = very concerned; switch

df$AV.rev <- ifelse(df$AV == 3, 1, 0)

# formula: with topics
model.rev <- as.formula(paste(
  "AV.rev ~",
  paste(covariates, collapse = " + "),
  "| fac.pid",
  collapse = ""
))


model.rev.topic <- as.formula(paste(
  "AV.rev~",
  paste(covariates, collapse = " + "),
  " + ",
  paste(topics, collapse = " + "),
  "| fac.pid",
  collapse = ""
))

# run model
lfe.rev <- felm(model.rev,
  data = df
)

lfe.rev.topic <- felm(model.rev.topic,
  data = df
)

# report ROBUST stand.errors ("sandwich", HC3)
models <- list(lfe.rev, lfe.rev.topic)

# make TABLES
se.v.rev <- list("")
for (i in 1:length(models)) {
  se.v.rev[[i]] <-
    as.vector(summary(models[[i]], robust = T)$coefficients[, 2])
}


# save
stargazer::stargazer(
  lfe.rev,
  lfe.rev.topic,
  se = se.v.rev,
  star.cutoffs = c(0.05, 0.01, 0.001),
  dep.var.labels = c("Not at all concerned"),
  type = "text",
  out = paste("vis/Robustness_RegTab_Reverse.tex", sep = "")
)


# report Brandt test

subset_topics <- c(
  "V54",
  # increasing
  "V45",
  "V55",
  "V65",
  "V60",
  "V23",
  "V10",
  "V12",
  "V40",
  "V52",
  "V1",
  "V66",
  "V27",
  # decreasing
  "V39",
  "V49",
  "V50",
  "V46",
  "V62",
  "V21",
  "V3",
  "V64",
  "V29",
  "V18"
)


model.subset.topics <- as.formula(paste(
  "AV.bin~",
  paste(covariates, collapse = " + "),
  " + ",
  paste(subset_topics, collapse = " + "),
  "| fac.pid",
  collapse = ""
))

lfe.subset.topics <- felm(model.subset.topics,
  data = df
)

# se:
models <- list(lfe.topics, lfe.subset.topics, lfe.rev.topic)

se.v.brandt <- list("")
for (i in 1:length(models)) {
  se.v.brandt[[i]] <-
    as.vector(summary(models[[i]], robust = T)$coefficients[, 2])
}

stargazer::stargazer(
  lfe.topics,
  lfe.subset.topics,
  lfe.rev.topic,
  se = se.v.brandt,
  star.cutoffs = c(0.05, 0.01, 0.001),
  dep.var.labels = c("very concerned", "Not at all concerned"),
  type = "html",
  out = paste("vis/brandt_test_comparison.html", sep = "")
)


table(df$AV)

