library(dplyr)

prepare_df <- function(df) {
  topics <- df %>%
    select(starts_with("V")) %>%
    names()

  # Z-SCALE!!
  df[, topics] <- apply(df[, topics], 2, FUN = scale)

  # EDUCATION
  df <- df[df$pgisced97 != "[-1] keine Angabe", ]
  df <- df[df$pgisced97 != "[0] in school", ]

  df$educ.bin <-
    ifelse((df$pgisced97 == "[6] higher education") |
      (df$pgisced97 == "[5] higher vocational"),
    1,
    0
    )

  ## Rewrite vars
  df$plh0012[!df$plh0012 %in% c(1:7, 27)] <- 99
  df$party <- recode(df$plh0012, `1` = "SPD", `2` = "CDU/CSU", `3` = "CDU/CSU", `4` = "FDP", `6` = "Linke", `5` = "Gruene", `7` = "Rechts", `27` = "Rechts", `99` = "No.Pref")
  df$party <- factor(df$party)
  df$party <- relevel(df$party, "No.Pref")
  # Party
  df$party <- as.character(df$party)
  df$party[df$party == "Rechts"] <- "Right-Wing"
  df$party <- factor(df$party)
  df$party <- relevel(df$party, ref = "No.Pref")

  # pmonin
  df$pmonin <- gsub("\\[.+\\] ", "", df$pmonin)
  df$pmonin <- ifelse((df$pmonin == "Oktober") | (df$pmonin == "November") | (df$pmonin == "Dezember"), "Okt/Nov/Dez", df$pmonin)
  df$pmonin <- as.character(df$pmonin)
  df$pmonin[df$pmonin == "Januar"] <- "January"
  df$pmonin[df$pmonin == "Februar"] <- "February"
  df$pmonin[df$pmonin == "Maerz"] <- "March"
  df$pmonin[df$pmonin == "Mai"] <- "May"
  df$pmonin[df$pmonin == "Juni"] <- "June"
  df$pmonin[df$pmonin == "Juli"] <- "July"
  df$pmonin[df$pmonin == "Okt/Nov/Dez"] <- "Oct/Nov/Dec"
  df$pmonin <- factor(df$pmonin)
  df$pmonin <- relevel(df$pmonin, ref = "January")

  df$worried.econ <- ifelse(df$plh0032 == 1, 1, 0) # 1: very concerned about economy
  df$worried.crime <- ifelse(df$plh0040 == 1, 1, 0) # 1: very concerned about crime



  df$plh0175[df$plh0175 < 7] <- 6
  df$income.satis <- as.factor(df$plh0175) # reference: 6 (low satisfaction)
  df$politics <- factor(df$plh0007) # reference: 1 (strong interest politics)
  df$pgisced97[df$pgisced97 < 3] <- 1
  df$educ <- factor(df$pgisced97) # reference: low educ

  ## delete missings (values <0); check NA: sapply(test, function(x) sum(is.na(x)))
  vars <- c("ix", "plh0175", "plh0007", "n_articles")

  df$ix <- 1:nrow(df)
  temp <- df %>%
    select(all_of(vars)) %>%
    filter_all(any_vars(. < 0))
  df <- df[!df$ix %in% temp$ix, ] # n = 197,499

  # change labels (R&R)
  old <- c("worried.econ", "income.satis", "politics", "n_articles")
  new <-
    c(
      "worried.About.Econ",
      "income.Satisfact",
      "political.Interest",
      "n.Articles"
    )

  for (i in 1:length(old)) {
    ix <- which(names(df) == old[i])
    names(df)[ix] <- new[i]
  }

  df$AV.bin <- ifelse(df$AV == 1, 1, 0)

  return(df)
}
