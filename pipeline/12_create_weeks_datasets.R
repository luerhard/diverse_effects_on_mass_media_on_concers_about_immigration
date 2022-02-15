rm(list = ls())

setwd("~/git/migration_news")

library(foreign)
library(dplyr)

loadings.zip.path = "data/theta_datasets/"
topiclist.path = 'data/lists/Topics_Label_69.csv'
soep.zip.path = 'data/SOEP/SOEP-LONG_v33.1_stata_de+en.zip'
save.path = 'data/final_datasets/'
save.r.path = "data/rdata/"

dir = tempdir()

### Start from here
zip.file <- soep.zip.path
dta.file <-  'pl.dta'
pl <- read.dta(file = unzip(zip.file, dta.file, exdir=dir))

dta.file = "ppfadl.dta"
ppfadl <- read.dta(file = unzip(zip.file, dta.file, exdir=dir))

dta.file = "pgen.dta"
pgen <- read.dta(file = unzip(zip.file, dta.file, exdir=dir))

dta.file = "pl2.dta"
pl2 <- read.dta(file = unzip(zip.file, dta.file, exdir=dir))
pl2 <- pl2[, c('pid', 'syear','pmonin')]

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
  regio <- read.csv(file = topiclist.path, encoding = 'latin1')
  
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
  

  # sex: gender, 1 Male, 2 Female
  # migback -- Migrationshintergrund: 1 no migration background, 2 direct migration background, 3 indirect migration background, 4 migrationbackground not differentirable
  # gebjahr -- Geboren (Jahr)
  vars <- c('sex', 'migback', 'gebjahr')
  df.reg <- merge(df.reg, ppfadl[, c('pid', 'syear', vars)], by = c('pid', 'syear'), all.x = T)
  
  print("MORE from pgen")

  # pgisced97 -- Bildung (ISCED2011 hat superviele vorwiegend missings whyever...) [0] in school [1] inadequately [2] general elementary [3] middle vocational [4] vocational + Abi [5] higher vocational [6] higher education
  # pgbilzeit: Amount of Education or Training in Years (als Alternatvie zum ISCED) (hier gäbe es auch noch pgbil als Schulabschluss)
  # pgnation: Nationalität (nur ausgewählte Kategorien), 1 Germany, 7 Ex-GDR (Country of Origin Only)
  # pgstatus_asyl: Status quo, Application for Asylum; relevant hier vor allem die Unterscheidung: -2 does not apply vs. alles höhere außer -1 no answer
  # pgstatus_refu: Refugee residence status, siehe pgstatus_asyl
  # pglabnet: Imputiertes Nettoeinkommen / Person (wie das mit den Haushalten geregelt ist, bin ich noch unsicher)
  # pgemplst: Employment status, [1] Full-Time Employment [2] Regular Part-Time Employment [3] Vocational Training [4] Marginal, Irregular Part-Time Employment [5] Not Employed [6] Sheltered workshop
  
  vars <- c('pgisced97','pgbilzeit', 'pgnation', 'pglabnet', 'pgemplst', 'pgreli')
  df.reg <- merge(df.reg, pgen[, c('pid', 'syear', vars)], by = c('pid', 'syear'), all.x = T)
  
  # get rid of text
  for(col in c('sex', 'migback', 'pgemplst', 'pgisced97','pgnation', 'plh0012', 'plh0007', 'plh0175', 'plh0032', 'plh0040')){
    df.reg[, col] <- clean.col(df.reg[,col], re)
  }
  
  ## nominal
  df.reg$plh0012[!df.reg$plh0012 %in% c(1:7, 27)] <- 99
  df.reg$party <- recode(df.reg$plh0012, `1` = 'SPD', `2` = 'CDU/CSU', `3` = 'CDU/CSU', `4` = 'FDP', `6` = 'Linke', `5` = 'Gruene', `7` = 'Rechts', `27` = 'Rechts', `99` = 'No.Pref' )
  df.reg$party <- factor(df.reg$party)
  df.reg$party <- relevel(df.reg$party, "No.Pref")
  
  print("MORE from pl2")
  
  df.reg <- merge(df.reg, pl2, by = c('pid', 'syear'), all.x = T)
  
  df <- df.reg
  
  print("Saving to disk")
  #foreign::write.dta(df.reg, paste0(save.path, "reg_", week, ".dta"))
  save(df, file = paste(save.r.path, 'reg_', week, '.RData', sep = ''))   # n = 357920
  
}

weeks = c(2, 4, 5:8)

for (week in weeks) {
  create_dataset(week, loadings.zip.path , topiclist.path, soep.zip.path)
}
