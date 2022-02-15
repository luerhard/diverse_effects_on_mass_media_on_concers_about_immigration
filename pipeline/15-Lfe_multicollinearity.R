options("scipen" = 50, "digits" = 3)

library(foreign)
library(dotwhisker)
library(lfe)
library(dplyr)
library(plyr)
library(broom)

# Load Data

load("data/rdata/final_regression_df.RData")

topics <- grep("^V", names(df), value = TRUE)


# RUN MODELS

subset_topics <- c(
  # increasing
  "V54",
  "V45",
  "V61",
  "V34",
  "V24",
  "V55",
  "V65",
  "V60",
  "V23",
  "V10",
  "V12",
  "V40",
  "V52",
  "V14",
  "V1",
  "V25",
  "V57",
  "V66",
  # decreasing
  "V27",
  "V4",
  "V39",
  "V49",
  "V50",
  "V46",
  "V37",
  "V62",
  "V21",
  "V3",
  "V64",
  "V29",
  "V18"
)

low_vif_topics = c(
  "V52",
  "V25",
  "V37",
  "V5",
  "V40",
  "V4",
  "V8",
  "V21",
  "V1",
  "V49",
  "V18",
  "V64",
  "V27",
  "V53",
  "V10",
  "V68",
  "V39",
  "V55",
  "V35",
  "V66",
  "V57",
  "V45",
  "V26",
  "V28",
  "V16",
  "V14",
  "V29",
  "V47",
  "V42",
  "V34",
  "V24",
  "V56",
  "V46",
  "V32",
  "V9",
  "V54",
  "V3",
  "V50",
  "V65", # ab hier >= 5
  "V67",
  "V33",
  "V67",
  "V33",
  "V31",
  "V62", # ab hier >= 6 
  "V60"
  #"V12", # ab hier >= 7
  #"V61", # ab hier >= 9
)


covariates <- c(
  "party",
  "political.Interest",
  "income.Satisfact",
  "worried.About.Econ",
  "n.Articles",
  "pmonin"
)

low_vif_coviarates <- c(
  "party",
  "income.Satisfact",
  "worried.About.Econ"
  #"n.Articles"
)


lfe.topics <- felm(as.formula(paste(
  "AV.bin ~",
  paste(covariates, collapse = " + "),
  " + ",
  paste(topics, collapse = " + "),
  "| fac.pid",
  collapse = ""
)), data = df)

lfe.subset_topics <- felm(as.formula(paste(
  "AV.bin~",
  paste(covariates, collapse = " + "),
  " + ",
  paste(subset_topics, collapse = " + "),
  "| fac.pid",
  collapse = ""
)), data = df)


lfe.subset_topics_nonarticles <- felm(as.formula(paste(
  "AV.bin~",
  paste(low_vif_coviarates, collapse = " + "),
  " + pmonin + ",
  paste(subset_topics, collapse = " + "),
  "| fac.pid",
  collapse = ""
)), data = df)

lfe.low_vif_topics <- felm(as.formula(paste(
  "AV.bin~",
  #paste(low_vif_coviarates, collapse = " + "),
  #" + ",
  paste(low_vif_topics, collapse = " + "),
  "| fac.pid",
  collapse = ""
)), data = df)

se.v.rev <- list("")
for (i in 1:length(models)) {
  se.v.rev[[i]] <-
    as.vector(summary(models[[i]], robust = T)$coefficients[, 2])
}
err = as.vector(summary(lfe.low_vif_topics, robust = T)$coefficients[, 2])
library(stargazer)
stargazer::stargazer(lfe.low_vif_topics,
se = err,
star.cutoffs = c(0.05, 0.01, 0.001),
dep.var.labels = c("Not at all concerned"),
type = "text",
)

lfe.low_vif_topics_n_articles <- felm(as.formula(paste(
  "AV.bin~",
  paste(low_vif_coviarates, collapse = " + "),
  " + n.Articles +",
  paste(low_vif_topics, collapse = " + "),
  "| fac.pid",
  collapse = ""
)), data = df)

# SIG EFFECTS PLOT

labels <- read.csv("data/lists/Topics_Label_69.csv")

labels$topicV <- paste0("V", as.character(labels$Topics))
labels$name <- paste(labels$topicV, labels$Eng.Label, sep = ".")


m_topics_df <- tidy(lfe.topics) %>%
  filter(term %in% subset_topics) %>%
  mutate(model = "Full")

m_topics_df$term <-
  plyr::mapvalues(
    m_topics_df$term,
    from = labels$topicV,
    to = labels$name,
    warn_missing = FALSE
  )

m_subset_topics_df <- tidy(lfe.subset_topics) %>%
  filter(term %in% subset_topics) %>%
  mutate(model = "Subset")

m_subset_topics_df$term <-
  plyr::mapvalues(
    m_subset_topics_df$term,
    from = labels$topicV,
    to = labels$name,
    warn_missing = FALSE
  )

m_subset_topics_df_nonarticles <- tidy(lfe.subset_topics_nonarticles) %>%
  filter(term %in% subset_topics) %>%
  mutate(model = "Subset - n.Articles")

m_subset_topics_df_nonarticles$term <-
  plyr::mapvalues(
    m_subset_topics_df_nonarticles$term,
    from = labels$topicV,
    to = labels$name,
    warn_missing = FALSE
  )



all_models <-
  rbind(m_subset_topics_df, m_topics_df, m_subset_topics_df_nonarticles)

all_models <- all_models[order(all_models$model, -all_models$estimate),]


pdf("vis/regression/caterpillar_multicollinearity_robustness.pdf")
{
  dwplot(
    all_models,
    ci = .83,
    dot_args = list(aes(colour = model, shape = model)),
    whisker_args = list(aes(colour = model)),
    vline = geom_vline(
      xintercept = 0,
      color = "grey60",
      linetype = 2
    )
  ) + # plot line at 0
    labs(x = "Coefficients (ci = .83)") +
    theme_minimal() + theme(legend.title = element_blank(), legend.position = "bottom") +
    scale_colour_grey(start = .7, end = .3)
}

dev.off()




## LOW VIF PLOT

m_topics_df <- tidy(lfe.topics) %>%
  filter(term %in% low_vif_topics) %>%
  mutate(model = "Full")

m_topics_df$term <-
  plyr::mapvalues(
    m_topics_df$term,
    from = labels$topicV,
    to = labels$name,
    warn_missing = FALSE
  )

m_low_vif_topics_df <- tidy(lfe.low_vif_topics) %>%
  filter(term %in% low_vif_topics) %>%
  mutate(model = "Low VIF")

m_low_vif_topics_df$term <-
  plyr::mapvalues(
    m_low_vif_topics_df$term,
    from = labels$topicV,
    to = labels$name,
    warn_missing = FALSE
  )

m_low_vif_topics_n_articles_df <- tidy(lfe.low_vif_topics_n_articles) %>%
  filter(term %in% low_vif_topics) %>%
  mutate(model = "Low VIF + n.Articles")

m_low_vif_topics_n_articles_df$term <-
  plyr::mapvalues(
    m_low_vif_topics_n_articles_df$term,
    from = labels$topicV,
    to = labels$name,
    warn_missing = FALSE
  )

low_vif_models <- rbind(m_low_vif_topics_df, m_topics_df, m_low_vif_topics_n_articles_df)
low_vif_models <- low_vif_models[order(low_vif_models$model, -low_vif_models$estimate),]

pdf("vis/regression/caterpillar_multicollinearity_robustness_low_vif.pdf")
{
  dwplot(
    low_vif_models,
    ci = .83,
    dot_args = list(aes(colour = model, shape = model)),
    whisker_args = list(aes(colour = model)),
    vline = geom_vline(
      xintercept = 0,
      color = "grey60",
      linetype = 2
    )
  ) + # plot line at 0
    labs(x = "Coefficients (ci = .83)") +
    theme_minimal() + theme(legend.title = element_blank(), legend.position = "bottom") +
    scale_colour_grey(start = .7, end = .3)
}

dev.off()

