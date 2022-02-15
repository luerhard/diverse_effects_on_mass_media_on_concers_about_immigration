# construction of "reg_data_inclMonth_XX" in "Lfe_CreateWeeks"


rm(list=ls())
setwd('~/git/migration_news')


load("data/rdata/final_regression_df.RData")

library(dotwhisker)
library(lfe)
library(dplyr)

# FUNCTION
create.model <- function(df, topics, model.topics){
  # Z-SCALE topics
  df[, topics] <- apply(df[, topics], 2, FUN = scale)
  
  # EDUCATION
  df <- df[df$pgisced97 != '[-1] keine Angabe',]
  df <- df[df$pgisced97 != '[0] in school',]
  
  df$educ.bin <- ifelse( (df$pgisced97 == '[6] higher education') | (df$pgisced97 == '[5] higher vocational'), 1, 0)
  
  ## Rewrite vars
  # Party
  df$party <- as.character(df$party)
  df$party[df$party == 'Rechts'] <- 'Right-Wing'
  df$party <- factor(df$party)
  df$party <- relevel(df$party, ref = 'No.Pref')
  
  # pmonin
  df$pmonin <- as.character(df$pmonin)
  df$pmonin[df$pmonin == 'Januar'] <- 'January'
  df$pmonin[df$pmonin == 'Februar'] <- 'February'
  df$pmonin[df$pmonin == 'Maerz'] <- 'March'
  df$pmonin[df$pmonin == 'Mai'] <- 'May'
  df$pmonin[df$pmonin == 'Juni'] <- 'June'
  df$pmonin[df$pmonin == 'Juli'] <- 'July'
  df$pmonin[df$pmonin == 'Okt/Nov/Dez'] <- 'Oct/Nov/Dec'
  df$pmonin <- factor(df$pmonin)
  df$pmonin <- relevel(df$pmonin, ref = 'January')
  
  # change labels (R&R)
  old <- c('worried.econ', 'income.satis', 'politics', 'n_articles')
  new <- c('worried.About.Econ', 'income.Satisfact', 'political.Interest', 'n.Articles')
  
  for(i in 1:length(old)){
    ix <- which(names(df)==old[i])
    names(df)[ix] <- new[i]
  }
  ## run model w/ topics
  
  lfe.topics <- felm(model.topics,
                     data = df)

  return(lfe.topics)
}




topics <- c(
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

# formula: with topics
model.topics <- as.formula(paste("AV.bin~",
                                 "party +",
                                 "political.Interest +",
                                 "income.Satisfact +",
                                 "worried.About.Econ +",
                                 "n.Articles + ",
                                 "pmonin + ",
                                 paste(topics, collapse = " + "),
                                 "| fac.pid",
                                 collapse = ""))


### SEVERAL weeks
weeks <- c(2, 4:8)

for(week in weeks){
  
  load(file = paste('data/rdata/reg_data_inclMonth_', week, '.RData', sep = '')) # df

  lfe.topics <- create.model(df, topics, model.topics)
  
  mod <- data.frame(summary(lfe.topics, robust = T)$coefficients)
  mod$term <- row.names(mod)
  colnames(mod) <- c("estimate", "std.error", "statistic", "p.value", "term")
  mod <- mod[mod$term %in% topics,]
  mod$model <- paste('Week', week, sep = '.')
  if(week == min(weeks)){
    mod.all <- mod
  }
  else{
    mod.all <- rbind(mod.all, mod)
  }
}

labels <- read.csv("data/lists/Topics_Label_69.csv")

labels$topicV <- paste0("V", as.character(labels$Topics))
labels$name <- paste(labels$topicV, labels$Eng.Label, sep = ".")


mod.all$term <-
  plyr::mapvalues(
    mod.all$term,
    from = labels$topicV,
    to = labels$name,
    warn_missing = FALSE
)

px <- dwplot(mod.all,
             conf.level = .83) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "right")

pdf("vis/regression/Caterpillar_Robustness_SeveralWeeks.pdf")
px
dev.off()

## $note$: was noch fehlt ist den graph auf (i) die in fig4 dargestellten topics zu begrenzen und (ii) die entsprechenden labels zu matchen