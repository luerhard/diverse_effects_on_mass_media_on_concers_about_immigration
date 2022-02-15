
library(foreign)
library(dplyr)
library(plm)


loadings.zip.path = "data/theta_datasets/"
topiclist.path = 'data/lists/Topics_Label_69.csv'
soep.zip.path = 'data/SOEP/SOEP-LONG_v33.1_stata_de+en.zip'
save.path = 'data/final_datasets/'

## transform: get only number in parantheses
re <- "\\[([^()]+)\\]"
clean.col <- function(col, re){
  clean <- unlist(stringr::str_extract_all(col, re))
  clean <- as.numeric(gsub('(\\[)|(\\])', '', clean))
  return(clean)
}

create_dataset <- function(week, loadings.zip.path, topiclist.path, soep.zip.path) {
  #### 
  print("Prepare DF")
  #####
  zip.file <- paste(loadings.zip.path, 'loadings_', week,'_influenceweeks.dta.zip', sep = '')
  dta.file <- paste('loadings_', week,'_influenceweeks.dta', sep = '')
  print(zip.file)
  df <- read.dta(file = unzip(zip.file, dta.file))
  
  ## Filter: Nicht-Region Topics
  regio <- read.csv(file = topiclist.path, sep = ';', encoding = 'latin1')
  
  drop <- grep('Region: ', regio$Label)
  keep <- paste('V', regio$Topics[!(regio$Topics %in% drop)], sep = '')
  
  df <- df[, c('pid', 'syear', 'date', 'n_articles', keep)]
  df <- df[!is.na(df$V1),]  # missings?
  
  ######
  print("Merge with SOEP")
  ######
  
  # ## all ind-data: pl
  # pl <- read.dta(file = 'stata/SOEP Long/pl.dta')
  # back <- pl
  # 
  # ## reduce data, takes forever to load...
  # pl <- pl[pl$pid %in% df$pid,] # relevant ID
  # pl <- pl[pl$syear > 2000,] # relevant years
  # 
  # ## save as Rdata (much faster)
  # save(pl, file = 'stata/SOEP Long/pl.RData')
  # 
  
  ######
  print("DEPENDENT Var from pl")
  ######
  ### Start from here
  zip.file <- soep.zip.path
  dta.file <-  'pl.dta'
  pl <- read.dta(file = unzip(zip.file, dta.file))
  
  
  # AV: table(pl$plj0046)
  # – [1] Very Concerned
  # – [2] Somewhat Concerned
  # – [3] Not Concerned At All
  
  clean <- unlist(stringr::str_extract_all(pl$plj0046, re))
  pl$AV <- clean.col(pl$plj0046, re)
  
  ## merge AV with Thetas
  df.reg <- merge(df, pl[, c('pid', 'syear', 'AV')], by = c('pid', 'syear'), all.x = T)
  
  # drop missings in AV
  df.reg <- df.reg[df.reg$AV %in% c(1, 2, 3),] # N = 357920
  
  
  ### IVs from "pl"
  # plh0012: party preference [in doko unter: https://paneldata.org/soep-core/data/pl/plh0012_h]
  # plh0007: interest in politics
  # plh0175: income satisfaction
  # plh0032: worries about economy
  # plh0040: worries about crime
  vars <- c('plh0012', 'plh0007', 'plh0175', 'plh0032','plh0040')
  df.reg <- merge(df.reg, pl[, c('pid', 'syear', vars)], by = c('pid', 'syear'), all.x = T) # N = 357920
  
  
  ######
  print("more INDEPENDENT Vars from ppfadl")
  ######
  # 
  # # arbeitlos: plb0021
  # # Nettoverdienst mind. /Monat (Euro) (ab 2002): plb0420_v2  # not in pl?
  # # Gender:  	pla0009
  # # Born in Germany: pla0045 # many missings!
  # vars <- c('plb0021', 'pla0009')
  # df.reg <- merge(df.reg, pl[, c('pid', 'syear', vars)], by = c('pid', 'syear'), all.x = T)
   
  dta.file = "ppfadl.dta"
  ppfadl <- read.dta(file = unzip(zip.file, dta.file))
  
  # sex: gender, 1 Male, 2 Female
  # migback -- Migrationshintergrund: 1 no migration background, 2 direct migration background, 3 indirect migration background, 4 migrationbackground not differentirable
  # gebjahr -- Geboren (Jahr)
  vars <- c('sex', 'migback', 'gebjahr')
  df.reg <- merge(df.reg, ppfadl[, c('pid', 'syear', vars)], by = c('pid', 'syear'), all.x = T)
  
  print("MORE from pgen")
  dta.file = "pgen.dta"
  pgen <- read.dta(file = unzip(zip.file, dta.file))
  
  # pgisced97 -- Bildung (ISCED2011 hat superviele vorwiegend missings whyever...) [0] in school [1] inadequately [2] general elementary [3] middle vocational [4] vocational + Abi [5] higher vocational [6] higher education
  # pgbilzeit: Amount of Education or Training in Years (als Alternatvie zum ISCED) (hier gäbe es auch noch pgbil als Schulabschluss)
  # pgnation: Nationalität (nur ausgewählte Kategorien), 1 Germany, 7 Ex-GDR (Country of Origin Only)
  # pgstatus_asyl: Status quo, Application for Asylum; relevant hier vor allem die Unterscheidung: -2 does not apply vs. alles höhere außer -1 no answer
  # pgstatus_refu: Refugee residence status, siehe pgstatus_asyl
  # pglabnet: Imputiertes Nettoeinkommen / Person (wie das mit den Haushalten geregelt ist, bin ich noch unsicher)
  # pgemplst: Employment status, [1] Full-Time Employment [2] Regular Part-Time Employment [3] Vocational Training [4] Marginal, Irregular Part-Time Employment [5] Not Employed [6] Sheltered workshop
  
  vars <- c('pgisced97','pgbilzeit', 'pgnation', 'pglabnet', 'pgemplst', 'pgreli')
  df.reg <- merge(df.reg, pgen[, c('pid', 'syear', vars)], by = c('pid', 'syear'), all.x = T)
  
  print("Saving to disk")
  foreign::write.dta(df.reg, paste0(save.path, "reg_", week, ".dta"))
  save(df.reg, file = paste(save.path, 'reg_', week, '.RData', sep = ''))   # n = 357920

}

# To actually create the datasets in .dta and RData Format
#create_dataset(1, loadings.zip.path, topiclist.path, soep.zip.path)
#create_dataset(2, loadings.zip.path, topiclist.path, soep.zip.path)
#create_dataset(3, loadings.zip.path, topiclist.path, soep.zip.path)
#create_dataset(4, loadings.zip.path, topiclist.path, soep.zip.path)
#create_dataset(8, loadings.zip.path, topiclist.path, soep.zip.path)
#create_dataset(12, loadings.zip.path, topiclist.path, soep.zip.path)
#rm(list=ls())
#.rs.restartR()

######
### REGRESSION
######

week = 4
save.path="D:/git/masterarbeit/data/final_datasets/"
load( file = paste(save.path, 'reg_', week, '.RData', sep = '')) # df.reg

## prepare data
topics <- names(df.reg)[-c(1:4)]
topics <- topics[1:47]

# get rid of text
for(col in c('sex', 'migback', 'pgemplst', 'pgisced97','pgnation', 'plh0012', 'plh0007', 'plh0175', 'plh0032', 'plh0040')){
  df.reg[, col] <- clean.col(df.reg[,col], re)
}


### IVs
## binary 
df.reg$pgemplst <- ifelse(df.reg$pgemplst == 5, 1, 0)  # 1: not working 
df.reg$pgnation <- ifelse(df.reg$pgnation == 1, 1, 0) # 1: German
df.reg$migback <- ifelse(df.reg$migback == 1, 1, 0) # 1: no migration background
df.reg$worried.econ <- ifelse(df.reg$plh0032 == 1, 1, 0) # 1: very concerned about economy
df.reg$worried.crime <- ifelse(df.reg$plh0040 == 1, 1, 0) # 1: very concerned about crime
# df.reg$educ <- ifelse(df.reg$pgbilzeit >= 12, 1, 0) # 1: higher educ

## delete missings (values <0); check NA: sapply(test, function(x) sum(is.na(x)))
vars <- c('ix','pgbilzeit', 'pgnation', 'pgemplst', 'pglabnet', 'plh0175', 'plh0007', "n_articles")

df.reg$ix <- 1:nrow(df.reg)
temp <- df.reg %>%
  select(vars) %>% 
  filter_all(any_vars(. < 0))
df.reg <- df.reg[!df.reg$ix %in% temp$ix,] # n = 197,499


## ordinal
df.reg$plh0175[df.reg$plh0175 < 7] <- 6
df.reg$income.satis <- as.factor(df.reg$plh0175)  # reference: 6 (low satisfaction)
df.reg$politics <- factor(df.reg$plh0007) # reference: 1 (strong interest politics)
df.reg$pgisced97[df.reg$pgisced97 < 3] <- 1
df.reg$educ <- factor(df.reg$pgisced97) # reference: low educ

## nominal
df.reg$plh0012[!df.reg$plh0012 %in% c(1:7, 27)] <- 99
df.reg$party <- recode(df.reg$plh0012, `1` = 'SPD', `2` = 'CDU/CSU', `3` = 'CDU/CSU', `4` = 'FDP', `6` = 'Linke', `5` = 'Gruene', `7` = 'Rechts', `27` = 'Rechts', `99` = 'No.Pref' )
df.reg$party <- factor(df.reg$party)
df.reg$party <- relevel(df.reg$party, "No.Pref")


# save(df.reg, file = paste('stata/SOEP Long/Regression_', week, '.RData', sep = ''))


## take sample (n = 40,364 persons)
test <- df.reg[df.reg$pid %in% sample(unique(df.reg$pid), 20000),]
# test <- df.reg
test$AV.reverse <- recode(test$AV, `1` = 3, `3` = 1)


########
### (1A) AV: 1:3, 3= very concerned; WITHIN FIXED EFFECTS, plm
########

# modmat_FE <- model.matrix(pform, data = mf, model = "within")
# detect_lin_dep(modmat_FE)

start.time <- Sys.time()
# switch AV

# formula MODEL 1: non-topic vars
model.fe <- as.formula(paste("AV.reverse ~",
                             "party +",
                             "politics +",
                             "income.satis +",
                             "worried.econ +",
                             "worried.crime +",
                             "pgemplst",
                                    collapse = ""))


# formula MODEL 2: with topics
model.fe.topics <- as.formula(paste("AV.reverse~",
                                    "party +",
                                    "politics +",
                                    "income.satis +",
                                    "worried.econ +",
                                    "worried.crime +",
                                    "pgemplst +",
                                paste(topics, collapse = " + "),
                                collapse = ""))

# formula MODEL 3: with topic[V16]*PARTY interaction
model.fe.party <- as.formula(paste("AV.reverse~",
                                    "party +",
                                    "politics +",
                                    "income.satis +",
                                    "worried.econ +",
                                    "worried.crime +",
                                    "pgemplst +",
                                    "V16:party +",
                                    paste(topics, collapse = " + "),
                                    collapse = ""))

# formula MODEL 4: with topic[V16]*EDUC interaction
model.fe.educ <- as.formula(paste("AV.reverse~",
                                   "party +",
                                   "politics +",
                                   "income.satis +",
                                   "worried.econ +",
                                   "worried.crime +",
                                   "pgemplst +",
                                   "V16:educ +",
                                   paste(topics, collapse = " + "),
                                   collapse = ""))


### MODEL 1: No topics
fe.fit <- plm(model.fe, 
                data = test,
                index = c('pid'), 
                model = 'within')
# summary(fe.fit)


### MODEL 2: WITH topics
fe.topics <- plm(model.fe.topics, 
              data = test,
              index = c('pid'), 
              model = 'within')
# summary(fe.topics)

### MODEL 3: WITH topic*PARTY
fe.party <- plm(model.fe.party, 
                 data = test,
                 index = c('pid'), 
                 model = 'within')


### MODEL 4: WITH topic*interaction
fe.educ <- plm(model.fe.educ, 
                data = test,
                index = c('pid'), 
                model = 'within')


#######
### (1B) AV: binary; WITHIN FIXED EFFECTS, plm
#######
test$AV.bin <- ifelse(test$AV.reverse ==3, 1, 0)  
# formula: non-topic vars
model.fe <- as.formula(paste("AV.bin ~",
                             #"party +",
                             #"politics +",
                             #"income.satis +",
                             #"worried.econ +",
                             #"worried.crime +",
                             #"pgemplst",
                             collapse = ""))


# formula: with topics
model.fe.topics <- as.formula(paste("AV.bin~",
                                    "party +",
                                    "politics +",
                                    "income.satis +",
                                    "worried.econ +",
                                    "worried.crime +",
                                    "pgemplst +",
                                    paste(topics, collapse = " + "),
                                    collapse = ""))

# formula MODEL 3: with topic[V16]*PARTY interaction
model.bin.party <- as.formula(paste("AV.bin~",
                                   "party +",
                                   "politics +",
                                   "income.satis +",
                                   "worried.econ +",
                                   "worried.crime +",
                                   "pgemplst +",
                                   "V16:party +",
                                   paste(topics, collapse = " + "),
                                   collapse = ""))

# formula MODEL 3: with topic[V16]*EDUC interaction
model.bin.educ <- as.formula(paste("AV.bin~",
                                    "party +",
                                    "politics +",
                                    "income.satis +",
                                    "worried.econ +",
                                    "worried.crime +",
                                    "pgemplst +",
                                    "V16:educ +",
                                    paste(topics, collapse = " + "),
                                    collapse = ""))


### MODEL 1: No topics
fe.bin <- plm(model.fe, 
              data = test,
              index = c('pid'), 
              model = 'within')
# summary(fe.bin)


# MODEL 2: WITH topics
fe.bin.topics <- plm(model.fe.topics, 
                 data = test,
                 index = c('pid', 'syear'), 
                 effect = 'twoways',
                 model = 'within')
# summary(fe.bin.topics)

### MODEL 3: WITH topic*PARTY
fe.bin.party <- plm(model.bin.party, 
                data = test,
                index = c('pid'), 
                effect = 'twoways',
                model = 'within')

### MODEL 4: WITH topic*EDUC
fe.bin.educ <- plm(model.bin.educ, 
                    data = test,
                    index = c('pid'), 
                    effect = 'twoways',
                    model = 'within')


### DIAGNOSTICS
# serial correlation
models <- list(fe.fit, fe.topics, fe.party,fe.educ, fe.bin, fe.bin.topics, fe.bin.party, fe.bin.educ)
bg.test <- c('bg.test')
pcd.test <- c('pcd.test')

#peaks at around 26GB RAM
#for(i in 1:length(models)){
#  bg.test[i+1] <- round(pbgtest(models[[i]])$p.value, 4)
#  pcd.test[i+1] <- round(pcdtest(models[[i]])$p.value, 4)
#}

## make table
stargazer::stargazer(
  models,
  star.cutoffs = c(0.05, 0.01, 0.001),
  dep.var.labels=c("Being very concerned (ord)","Being very concerned (bin)" ),
  # add.lines = list(bg.test), # list(bg.test, pcd.test)
  type="text",
  out = paste("Fixed_Within_", week, ".html", sep = '')) 

end.time <- Sys.time()
print(end.time - start.time)  # n = 20000: 20.2min


### POTENTIAL first proxy: keep only significant topics and rerun
fe.sign <- as.data.frame(summary(fe.topics)$coefficients)
topics.sign <- row.names(fe.sign[fe.sign[,4]<0.05,])
topics.sign <- topics.sign[topics.sign %in% topics]
fe.sign[row.names(fe.sign) %in% topics.sign,]



### (B) Ordinal logistic; FIXED Years
library(MASS)

test$AV.ord <- as.factor(test$AV.reverse) # "Große Sorge" als 3
test$syear.fac <- as.factor(test$syear)

# formulas
model.ord <- as.formula(paste("AV.ord ~",
                             "sex + ",
                             "migback + ",
                             # "gebjahr +",
                             # "I(gebjahr^2) +",
                             "party +",
                             "politics +",
                             "income.satis +",
                             "worried.econ +",
                             "worried.crime +",
                             "pgbilzeit +",
                             "pgnation +",
                             "pgemplst + ",
                             "syear.fac",
                             collapse = ""))

model.ord.topics <- as.formula(paste("AV.ord ~",
                              "sex + ",
                              "migback + ",
                              # "gebjahr +",
                              # "I(gebjahr^2) +",
                              "party +",
                              "politics +",
                              "income.satis +",
                              "worried.econ +",
                              "worried.crime +",
                              "pgbilzeit +",
                              "pgnation +",
                              "pgemplst +",
                              "syear.fac +",
                              paste(topics, collapse = " + "),
                              collapse = ""))



# model 1
ord.fit <- polr(model.ord, 
                data = test, 
                Hess=T)

# summary(ord.fit)

# model 2
ord.fit.topics <- polr(model.ord.topics, 
                data = test, 
                Hess=T)
# summary(ord.fit.topics)


### Diagnostics
library(DescTools)
models.ord <- list(ord.fit,ord.fit.topics)

nagel <- c('Nagelkerke')
fadden <- c('McFadden')

for(i in 1:length(models.ord)){
  nagel[i+1] <- round(PseudoR2(models.ord[[i]], which = nagel[1]), 4)
  fadden[i+1] <- round(PseudoR2(models.ord[[i]], which = fadden[1]), 4)
  # Tjur[i+1] <- round(PseudoR2(models.ord[[i]], which = Tjur[1]), 4)
}


## table
stargazer::stargazer(
  models.ord, 
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit = "syear*",
  add.lines = list(nagel, fadden), 
  dep.var.labels=c("Being very concerned (ord)"),
  type="text",
  out = paste("Ordinal_", week, ".html", sep = '') )


## more readable: Odds Ratios and CI
ci <- confint(ord.fit.topics)
odds <- as.data.frame(exp(cbind(OR = coef(ord.fit.topics), ci)))
odds$estimates <- row.names(odds)
odds <- odds[odds$estimates %in% topics,]
names(odds)[2] <- 'lower'
names(odds)[3] <- 'upper'

write.csv(odds, file = paste0('Ordinal_Topics', week, 'Odd.csv'))

## visualize
library(ggplot2)
pdf(paste0('Ordinal_TopicsOdds_', week, '.pdf'))
ggplot(odds[odds$estimates %in% topics.sign[-5],], aes(x = OR, y = estimates)) + 
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = upper, xmin = lower), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange")
dev.off()



### (C) Logistic AV; GLM, WITHIN FIXED EFFECTS, pglm
library(pglm)

# ???Argument "start" fehlt (ohne Standardwert)???

test$AV.bin <- ifelse(test$AV.reverse ==3, 1, 0)  
# formula: non-topic vars
model.fe.bin <- as.formula(paste("AV.bin ~",
                             "party +",
                             "politics +",
                             "income.satis +",
                             "worried.econ +",
                             "worried.crime +",
                             "pgemplst",
                             collapse = ""))


log.fit <- pglm(model.fe.bin, 
                    data = test,
                    family = binomial('logit'),
                    index = c('pid'), 
                    model = 'within')

summary(log.fit)