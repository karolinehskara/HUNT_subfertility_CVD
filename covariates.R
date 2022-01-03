# Packages #####
library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(foreign)
## Load data frames #####
Sys.setlocale('LC_TIME', 'English')
# Read study population file
setwd('N:/data/durable/projects/KASK_Infertility_CVD/Data')
pop_dat <- read.table(file = 'pop_dat_211209.txt', header = TRUE)

# Read HUNT questionnaires file
q_hunt <- read.csv('Adult_HUNT.csv', na.strings = c(''), colClasses = 'character')
# Format IDs (PID_110334) to start with 'id_' followed by the unique ID
q_hunt$PID_110334 <- paste('id',q_hunt$PID_110334,sep='_')
# Rename rownames to the unique IDs
rownames(q_hunt) <- q_hunt$PID_110334
# Convert participation dates in character representations to objects of class 'Date'
q_hunt$PartDat_NT1BLQ1 <- as.Date(q_hunt$PartDat_NT1BLQ1, format = '%d %b %y')
q_hunt$PartDat_NT2BLQ1 <- as.Date(q_hunt$PartDat_NT2BLQ1, format = '%d %b %y')
q_hunt$PartDat_NT2BLQ2 <- as.Date(q_hunt$PartDat_NT2BLQ2, format = '%d %b %y')
q_hunt$PartDat_NT3BLQ1 <- as.Date(q_hunt$PartDat_NT3BLQ1, format = '%d %b %y')
q_hunt$PartDat_NT4BLM <- as.Date(q_hunt$PartDat_NT4BLM, format = '%d %b %y')
q_hunt$PartDat_NT4BLQ1 <- as.Date(q_hunt$PartDat_NT4BLQ1, format = '%d %b %y')

## Add blood pressure variable received after access to HUNT questionnaires
# Read file
setwd('N:/data/durable/RAW/HUNT')
newdat <- read.spss('2021-04-15_110334_Tillegg.sav', to.data.frame = TRUE)
# Format IDs (PID.110334) to start with 'id_' followed by the unique ID
newdat$PID.110334 <- paste('id',newdat$PID.110334,sep='_')
# Rename rownames to the unique IDs
rownames(newdat) <- newdat$PID.110334
setwd('N:/data/durable/projects/KASK_Infertility_CVD/Data')

## Medical Birth Registry of Norway
# Read file
mbrn <- read.table('partners_KASK_210428.txt', sep = ',', header = TRUE, na.strings = c('','             ', NA), colClass = 'character')
# Recode IDs (PID_110334_BARN, PID110334_MOR, PID110334_FAR) to start with 'id_' followed by the unique ID
# BARN = child, MOR = mother, FAR = father
mbrn$PID110334_BARN[!is.na(mbrn$PID110334_BARN)] <- paste('id',mbrn$PID110334_BARN[!is.na(mbrn$PID110334_BARN)],sep='_')
mbrn$PID110334_MOR[!is.na(mbrn$PID110334_MOR)] <- paste('id',mbrn$PID110334_MOR[!is.na(mbrn$PID110334_MOR)],sep='_')
mbrn$PID110334_FAR[!is.na(mbrn$PID110334_FAR)] <- paste('id',mbrn$PID110334_FAR[!is.na(mbrn$PID110334_FAR)],sep='_')
# Create IDs for people with missing IDs starting with 'NA_' followed by a unique number between 1 and total number of people (mother, father, child)
mbrn$PID110334_MOR[is.na(mbrn$PID110334_MOR)] <- paste('NA',seq(from = 1, to = length(mbrn$PID110334_MOR[is.na(mbrn$PID110334_MOR)])), sep='_')
mbrn$PID110334_FAR[is.na(mbrn$PID110334_FAR)] <- paste('NA',seq(from = 1, to = length(mbrn$PID110334_FAR[is.na(mbrn$PID110334_FAR)])), sep='_')
mbrn$PID110334_BARN[is.na(mbrn$PID110334_BARN)] <- paste('NA',seq(from = 1, to = length(mbrn$PID110334_BARN[is.na(mbrn$PID110334_BARN)])), sep='_')
# Convert birth dates (FDATO) and death dates (DDATO) in character representations to objects of class 'Date'
mbrn$FDATO <- as.Date(mbrn$FDATO)
mbrn$DDATO <- as.Date(mbrn$DDATO)
# Uniqe children IDs in first column, so mother IDs are not unique if they have more than one child
# Want to use mothers' IDs as rownames
# Create a temprorary table of the mothers' IDs
tmp <- table(mbrn$PID110334_MOR)
# Convert IDs of unknown mothers to missing
mbrn$PID110334_MOR[grep('NA',mbrn$PID110334_MOR)] <- NA
# Create unique mother IDs for each birth
nn <- setNames(paste(rep(names(tmp), tmp), do.call(c,sapply(tmp,function(x)seq_len(x))), sep='_'),  rep(names(tmp), tmp))
# Order data file by order of unique mother IDs above
mbrn <- mbrn[order(factor(mbrn$PID110334_MOR, levels=unique(names(nn)))),]
# Recode names of rows to unique mother IDs
rownames(mbrn) <- unname(nn)

## Add covariates #####
# Age when first experienced subfertility
pop_dat[pop_dat$survey == 'h2', 36] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'PregDiffAg_NT2BLQ2']
pop_dat[pop_dat$survey == 'h3', 36] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'PregDiffAg_NT3BLQ2']
pop_dat[pop_dat$survey == 'h4', 36] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'PregDiffAg_NT4BLQ2']
colnames(pop_dat)[36] <- 'fert_age'

# Civil status
pop_dat[pop_dat$survey == 'h2', 37] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'MaritStat_NT2BLQ1']
pop_dat[pop_dat$survey == 'h3', 37] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'MaritStat_NT3BLQ1']
pop_dat[pop_dat$survey == 'h4', 37] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'MaritStat_NT4BLQ1']
colnames(pop_dat)[37] <- 'civ_stat'
# Recode civil status from Norwegian to English
pop_dat$civ_stat[!is.na(pop_dat$civ_stat) & pop_dat$civ_stat == 'Gift'] <- 'married'
pop_dat$civ_stat[!is.na(pop_dat$civ_stat) & pop_dat$civ_stat %in% c('Enke/enkemann','Enke(mann)','Enke, enkemann')] <- 'widowed'
pop_dat$civ_stat[!is.na(pop_dat$civ_stat) & pop_dat$civ_stat %in% c('Skilt','Separert')] <- 'divorced/separated'
pop_dat$civ_stat[!is.na(pop_dat$civ_stat) & pop_dat$civ_stat == 'Ugift'] <- 'unmarried'

# Cohabitation with spouse
pop_dat[pop_dat$survey == 'h2', 38] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'CohSps_NT2BLQ2']
pop_dat[pop_dat$survey == 'h3', 38] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'CohSps_NT3BLQ2']
pop_dat[pop_dat$survey == 'h4', 38] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'CohSps_NT4BLQ1']
colnames(pop_dat)[38] <- 'coh_sps'
# Recode cohabitation with spouse from Norwegian to English
pop_dat$coh_sps[!is.na(pop_dat$coh_sps) & pop_dat$coh_sps %in% c('Ja','Ektefelle/samboer','Ja, ektefelle/samboer/partner')] <- 'yes'
pop_dat$coh_sps[!is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'Nei'] <- 'no'

# Use civil status to fill in missing information on cohabitation with spouse
pop_dat$coh_sps[pop_dat$survey == 'h2' & is.na(pop_dat$coh_sps)] <- ifelse(!is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2' & is.na(pop_dat$coh_sps)], 'CohOtAd_NT2BLQ2']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2' & is.na(pop_dat$coh_sps)], 'CohChiLes18Y_NT2BLQ2']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2' & is.na(pop_dat$coh_sps)], 'CohAdN_NT2BLQ2']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2' & is.na(pop_dat$coh_sps)], 'CohChiN_NT2BLQ2']), 'no', NA)
pop_dat$coh_sps[pop_dat$survey == 'h3' & is.na(pop_dat$coh_sps)] <- ifelse(!is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3' & is.na(pop_dat$coh_sps)], 'CohNo_NT3BLQ2']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3' & is.na(pop_dat$coh_sps)], 'CohPar_NT3BLQ2']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3' & is.na(pop_dat$coh_sps)], 'CohChiN_NT3BLQ2']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3' & is.na(pop_dat$coh_sps)], 'CohOtMor18Y_NT3BLQ2']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3' & is.na(pop_dat$coh_sps)], 'CohChiLes18Y_NT3BLQ2']), 'no', NA)
pop_dat$coh_sps[pop_dat$survey == 'h4' & is.na(pop_dat$coh_sps)] <- ifelse(!is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4' & is.na(pop_dat$coh_sps)], 'CohNo_NT4BLQ1']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4' & is.na(pop_dat$coh_sps)], 'CohAdN_NT4BLQ1']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4' & is.na(pop_dat$coh_sps)], 'CohChiN_NT4BLQ1']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4' & is.na(pop_dat$coh_sps)], 'CohOtMor18Y_NT4BLQ1']) |
                                                                             !is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4' & is.na(pop_dat$coh_sps)], 'CohChiLes18Y_NT4BLQ1']), 'no', NA)
pop_dat$coh_sps[is.na(pop_dat$coh_sps) & !is.na(pop_dat$civ_stat) & pop_dat$civ_stat == 'married'] <- 'yes'

# BMI
pop_dat[pop_dat$survey == 'h2', 39] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'Bmi_NT2BLM']
pop_dat[pop_dat$survey == 'h3', 39] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'Bmi_NT3BLM']
pop_dat[pop_dat$survey == 'h4', 39] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'Bmi_NT4BLM']
colnames(pop_dat)[39] <- 'bmi'

# Smoking categories
pop_dat[pop_dat$survey == 'h2', 40] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'SmoStat_NT2BLQ1']
pop_dat[pop_dat$survey == 'h3', 40] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'SmoStat_NT3BLQ1']
pop_dat[pop_dat$survey == 'h4', 40] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'SmoStat_NT4BLQ1']
colnames(pop_dat)[40] <- 'smok'
# Recode smoking categories from Norwegian to English
pop_dat[!is.na(pop_dat[,'smok']) & !(grepl('Aldri',pop_dat[,'smok'])) & !(grepl('Tidligere',pop_dat[,'smok'])),'smok'] <- 'smoker'
pop_dat[!is.na(pop_dat[,'smok']) & grepl('Aldri',pop_dat[,'smok']),'smok'] <- 'never smoked'
pop_dat[!is.na(pop_dat[,'smok']) & grepl('Tidligere',pop_dat[,'smok']),'smok'] <- 'former smoker'

# Smoking pack years
pop_dat[pop_dat$survey == 'h2', 41] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'SmoPackYrs_NT2BLQ1']
pop_dat[pop_dat$survey == 'h3', 41] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'SmoPackYrs_NT3BLQ1']
pop_dat[pop_dat$survey == 'h4', 41] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'SmoPackYrs_NT4BLQ1']
colnames(pop_dat)[41] <- 'packyr'

# Combine smoking categories and pack years to new smoking categories
pop_dat[!is.na(pop_dat$smok) & pop_dat$smok == 'never smoked' & is.na(pop_dat$packyr), 42] <- 'non-smoker'
pop_dat[!is.na(pop_dat$smok) & pop_dat$smok == 'smoker' & ((!is.na(pop_dat$packyr) & pop_dat$packyr <= 20) | is.na(pop_dat$packyr)), 42] <- 'smoker,py<=20'
pop_dat[!is.na(pop_dat$smok) & pop_dat$smok == 'smoker' & !is.na(pop_dat$packyr) & pop_dat$packyr > 20, 42] <- 'smoker,py20<'
pop_dat[!is.na(pop_dat$smok) & pop_dat$smok == 'former smoker' & ((!is.na(pop_dat$packyr) & pop_dat$packyr <= 20) | is.na(pop_dat$packyr)), 42] <- 'former smoker,py<=20'
pop_dat[!is.na(pop_dat$smok) & pop_dat$smok == 'former smoker' & !is.na(pop_dat$packyr) & pop_dat$packyr > 20, 42] <- 'former smoker,py20<'
colnames(pop_dat)[42] <- 'smok_cat'

# Education
pop_dat[pop_dat$survey == 'h2', 43] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'Educ_NT2BLQ1']
pop_dat[pop_dat$survey == 'h4', 43] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'Educ_NT4BLQ1']
pop_dat[pop_dat$survey == 'h3', 43] <- ifelse(!is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'Educ_NT4BLQ1']), q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'Educ_NT4BLQ1'], q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'Educ_NT2BLQ1'])
colnames(pop_dat)[43] <- 'educ'
# Recode education from Norwegian to English
pop_dat$educ[pop_dat$educ %in% c('Grunnskole 7-10 Ã¥r, framhaldsskole, folkehÃ¸gskole',
                                 'Realskole, middelskole, yrkesskole 1-2 Ã¥rig videregÃ¥ende skole',
                                 'Grunnskole',
                                 'Fagbrev eller svennebrev',
                                 '1-2Ã¥rig videregÃ¥ende skole')] <- 'secondary school'
pop_dat$educ[pop_dat$educ %in% c('3 Ã¥r i videregÃ¥ende skole',
                                 'Artium, Ã¸k.gymnas, allmennfaglig retning i videregÃ¥ende skole')] <- 'upper secondary school'
pop_dat$educ[pop_dat$educ %in% c('HÃ¸gskole/universitet, mindre enn 4 Ã¥r',
                                 'HÃ¸gskole/universitet, 4 Ã¥r eller mer',
                                 'HÃ¸yskole/universitet, mindre enn 4 Ã¥r',
                                 'HÃ¸yskole/universitet, 4 Ã¥r eller mer')] <- 'higher education'

# Occupation
pop_dat[pop_dat$survey == 'h2', 44] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'WorEGPEd_NT2BLQ2']
pop_dat[pop_dat$survey == 'h3', 44] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'WorTitISCO2_NT3BLI']
pop_dat[pop_dat$survey == 'h4', 44] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'WorTitISCO2_NT4BLI']
colnames(pop_dat)[44] <- 'occup'
# Use occupation to fill in missing education
pop_dat$educ[pop_dat$survey == 'h2' & is.na(pop_dat$educ) & pop_dat$occup %in% c('Selvstendig i akademisk erverv - EGP-klasse I',
                                                                               'Overordnet stilling i offentlig eller privat virksomhet - EGP-klasse I',
                                                                               'Flere yrker - EGP-klasse I',
                                                                               'FagfunksjonÃ¦r - EGP-klasse II',
                                                                               'Flere yrker - EGP-klasse II')] <- 'higher education'
pop_dat$educ[pop_dat$survey == 'h2' & is.na(pop_dat$educ) & pop_dat$occup %in% c('Underordnet funksjonÃ¦r - EGP-klasse III',
                                                                               'Flere yrker - EGP-klasse III',
                                                                               'Annen selvstendig næringsvirksomhet - EGP-klasse IV a+IV b',
                                                                               'Gårdbruker eller skogeier - EGP-klasse IV c',
                                                                               'Fisker - EGP-klasse IV c',
                                                                               'Flere yrker - EGP-klasse IV',
                                                                               'Fagarbeider, handverker, forM - EGP-klasse V+VI',
                                                                               'Flere yrker - EGP-klasse V+VI')] <- 'upper secondary school'
pop_dat$educ[pop_dat$survey == 'h2' & is.na(pop_dat$educ) & pop_dat$occup %in% c('Spesialarbeider eller ufaglÃ¦rt arbeider - EGP-klasse VII a+VII b',
                                                                               'SjÃ¥fÃr - EGP-klasse VII a+VII b',
                                                                               'Flere yrker - EGP-klasse VII a+VII b',
                                                                               'Ikke i arbeid - ikke klassifisert i EGP sosial klasseskjema')] <- 'secondary school'
pop_dat$educ[(pop_dat$survey == 'h3' | pop_dat$survey == 'h4') & is.na(pop_dat$educ) & substring(pop_dat$occup,1,1) %in% c('1','2','3')] <- 'higher education'
pop_dat$educ[(pop_dat$survey == 'h3' | pop_dat$survey == 'h4') & is.na(pop_dat$educ) & substring(pop_dat$occup,1,1) %in% c('0','4','5','6','7','8')] <- 'upper secondary school'
pop_dat$educ[(pop_dat$survey == 'h3' | pop_dat$survey == 'h4') & is.na(pop_dat$educ) & substring(pop_dat$occup,1,1) %in% c('9')] <- 'secondary school'

# Serum cholesterol
pop_dat[pop_dat$survey == 'h2', 45] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'SeChol_NT2BLM']
pop_dat[pop_dat$survey == 'h3', 45] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'SeChol_NT3BLM']
pop_dat[pop_dat$survey == 'h4', 45] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'SeChol_NT4BLM']
colnames(pop_dat)[45] <- 'serum_chol'

# HCL cholesterol
pop_dat[pop_dat$survey == 'h2', 46] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'SeHDLChol_NT2BLM']
pop_dat[pop_dat$survey == 'h3', 46] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'SeHDLChol_NT3BLM']
pop_dat[pop_dat$survey == 'h4', 46] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'SeHDLChol_NT4BLM']
colnames(pop_dat)[46] <- 'hdl_chol'

# Diastolic blood pressure
pop_dat[pop_dat$survey == 'h2', 47] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'BPDiasMn23_NT2BLM']
pop_dat[pop_dat$survey == 'h3', 47] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'BPDiasMn23_NT3BLM']
pop_dat[pop_dat$survey == 'h4', 47] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'BPDiasMn23_NT4BLM']
colnames(pop_dat)[47] <- 'bp_dias'

# Systolic blood pressure
pop_dat[pop_dat$survey == 'h2', 48] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'BPSystMn23_NT2BLM']
pop_dat[pop_dat$survey == 'h3', 48] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'BPSystMn23_NT3BLM']
pop_dat[pop_dat$survey == 'h4', 48] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'BPSystMn23_NT4BLM']
colnames(pop_dat)[48] <- 'bp_syst'

# Blood pressure medication
pop_dat[pop_dat$survey == 'h2', 49] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'BPMedCu_NT2BLQ1']
pop_dat[pop_dat$survey == 'h3', 49] <- as.character(newdat[pop_dat$PID110334[pop_dat$survey == 'h3'], 'BPMedEv.NT3BLQ1'])
pop_dat[pop_dat$survey == 'h4', 49] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'BPMedCu_NT4BLQ1']
colnames(pop_dat)[49] <- 'bp_med'
# Recode blood pressure medication from Norwegian to English
pop_dat$bp_med[!is.na(pop_dat$bp_med) & pop_dat$bp_med %in% c('NÃ¥','Ja')] <- 'yes'
pop_dat$bp_med[!is.na(pop_dat$bp_med) & pop_dat$bp_med == 'FÃ¸r, men ikke nÃ¥'] <- 'no'
pop_dat$bp_med[!is.na(pop_dat$bp_med) & pop_dat$bp_med %in% c('Nei', 'Aldri')] <- 'no'
# Add 10 mmHg to diastolic and systolic blood pressure if participants use blood pressure medication
pop_dat$bp_dias <- as.numeric(pop_dat$bp_dias)
pop_dat$bp_syst <- as.numeric(pop_dat$bp_syst)
pop_dat$bp_dias <- ifelse(pop_dat$survey == 'h4', ifelse(!is.na(pop_dat$bp_med),pop_dat$bp_dias+10,pop_dat$bp_dias),
                          ifelse(is.na(pop_dat$bp_med), NA, ifelse(pop_dat$bp_med == 'yes',pop_dat$bp_dias+10,pop_dat$bp_dias)))
pop_dat$bp_syst <- ifelse(pop_dat$survey == 'h4', ifelse(is.na(pop_dat$bp_med),pop_dat$bp_syst,pop_dat$bp_syst+10),
                          ifelse(is.na(pop_dat$bp_med), NA, ifelse(pop_dat$bp_med == 'yes',pop_dat$bp_syst+10,pop_dat$bp_syst)))

# Early birth before and/or after participation in HUNT
tmp <- lapply(split(mbrn[mbrn$PID110334_MOR %in% pop_dat$PID110334[pop_dat$sex == 'F'],], mbrn$PID110334_MOR[mbrn$PID110334_MOR %in% pop_dat$PID110334[pop_dat$sex == 'F']]),
              function(x) ifelse(x$FDATO >= pop_dat[x$PID110334_MOR, which(colnames(pop_dat) == 'survey_date')] &
                                   !is.na(x$FDATO) & !is.na(pop_dat[x$PID110334_MOR, which(colnames(pop_dat) == 'survey_date')]),
                                 ifelse(x$SVLEN < 37, '1_early_af', '0_normal_af'),
                                 ifelse(x$SVLEN < 37, '1_early_bf', '0_normal_bf')))
# Recode early birth before and after *participation in HUNT to 'both'
tmp <- lapply(tmp, function(x) ifelse(!('1_early_af' %in% unlist(x) | '1_early_bf' %in% unlist(x)),
                                      '0_normal', ifelse('1_early_af' %in% unlist(x) & '1_early_bf' %in% unlist(x), '2_both',
                                                         ifelse('1_early_bf' %in% unlist(x), '1_before',
                                                                ifelse('1_early_af' %in% unlist(x), '1_after',NA)))))
pop_dat[names(tmp),50] <- unlist(tmp)
colnames(pop_dat)[50] <- 'early_birth'
# Recode early birth before, after, or both before and after, to just 'early'
pop_dat$early_birth[which(!is.na(pop_dat$early_birth) & (pop_dat$early_birth == '1_after' | pop_dat$early_birth == '1_before' | pop_dat$early_birth == '2_both'))] <- '1_early'

# Preeclampsia before and/or after participation in HUNT
tmp <- lapply(split(mbrn[mbrn$PID110334_MOR %in% pop_dat$PID110334[pop_dat$sex == 'F'],],
                    mbrn$PID110334_MOR[mbrn$PID110334_MOR %in% pop_dat$PID110334[pop_dat$sex == 'F']]),
              function(x) ifelse(x$FDATO >= pop_dat[x$PID110334_MOR, which(colnames(pop_dat) == 'survey_date')] &
                                   !is.na(x$FDATO) & !is.na(pop_dat[x$PID110334_MOR, which(colnames(pop_dat) == 'survey_date')]),
                                 ifelse(!is.na(x$PREEKL), '1_preekl_af', '0_normal_af'),
                                 ifelse(!is.na(x$PREEKL), '1_preekl_bf', '0_normal_bf')))
# Recode preeclampsia before and after participation in HUNT to 'both'
tmp <- lapply(tmp, function(x) ifelse(!('1_preekl_af' %in% unlist(x) | '1_preekl_bf' %in% unlist(x)),
                                      '0_normal', ifelse('1_preekl_af' %in% unlist(x) & '1_preekl_bf' %in% unlist(x), '2_both',
                                                         ifelse('1_preekl_bf' %in% unlist(x), '1_before',
                                                                ifelse('1_preekl_af' %in% unlist(x), '1_after',NA)))))
pop_dat[names(tmp),51] <- unlist(tmp)
colnames(pop_dat)[51] <- 'preekl'
# Recode preeclampsia before, after, or both before and after, to just 'preeclampsia'
pop_dat$preekl[which(!is.na(pop_dat$preekl) & (pop_dat$preekl == '1_after' | pop_dat$preekl == '1_before' | pop_dat$preekl == '2_both'))] <- '1_preekl'

# Diabetes
pop_dat[pop_dat$survey == 'h2', 52] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'DiaEv_NT2BLQ1']
pop_dat[pop_dat$survey == 'h3', 52] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'DiaEv_NT3BLQ1']
pop_dat[pop_dat$survey == 'h4', 52] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'DiaEv_NT4BLQ1']
colnames(pop_dat)[52] <- 'diabetes'
# Recode diabetes from Norwegian to English
pop_dat[!is.na(pop_dat$diabetes) & pop_dat$diabetes == 'Ja', 52] <- 'yes'
pop_dat[!is.na(pop_dat$diabetes) & pop_dat$diabetes == 'Nei', 52] <- 'no'

# Subfertile women with or without child_tot
pop_dat$fert_child <- ifelse(pop_dat$fert == 'yes' & (pop_dat$child_tot == '0' | is.na(pop_dat$child_tot)), 'infertile',
                             ifelse(pop_dat$fert == 'yes' & !(pop_dat$child_tot == '0' | is.na(pop_dat$child_tot)), 'subfertile', 'fertile'))
# Inspection of subfertile women with missing information on number of children
q_hunt %>% filter(PID_110334 %in% pop_dat[is.na(pop_dat$child_tot),1]) %>% select(BirthYear, DelivN_NT2BLQ1, DelivN_NT2BLQ2, DelivN_NT3BLI, DelivN_NT4BLI) %>% view()
q_hunt %>% filter(PID_110334 %in% pop_dat[is.na(pop_dat$child_tot) & pop_dat$birthyear >= 1959,1]) %>% select(BirthYear, DelivN_NT2BLQ1, DelivN_NT2BLQ2, DelivN_NT3BLI, DelivN_NT4BLI) %>% view()
q_hunt %>% filter(PID_110334 %in% pop_dat[is.na(pop_dat$child_tot) & pop_dat$birthyear < 1959,1]) %>% select(BirthYear, DelivN_NT2BLQ1, DelivN_NT2BLQ2, DelivN_NT3BLI, DelivN_NT4BLI) %>% view()
# If born after 1959, subfertile women are at reproductive age when the birth registry was started,
# and missing in birth registry means they did not conceive -> assume no children and classify them as infertile
# If born before 1959, subfertile women could have conceived before the birth registry was started.
# However, after inspection, we see that they did not report having children in any of the other surveys -> assume no children and classify them as infertile 

# Stratified by age at first experienced subfertility above or below median age
pop_dat$strat_fert_age <- ifelse(pop_dat$fert == 'yes',
                                 ifelse(!is.na(pop_dat$fert_age),
                                        ifelse(pop_dat$fert_age > median(pop_dat$fert_age, na.rm=TRUE), '2_high','1_low'),NA),'0_fertile')

# Stratified by age above or below median age
pop_dat$survey_age <- as.double(pop_dat$survey_age)
pop_dat$strat_age <- ifelse(pop_dat$survey_age > median(pop_dat$survey_age), 'high', 'low')

# Cluster ID for partners
pop_dat[,56] <- rep(NA, length(pop_dat$PID110334))
colnames(pop_dat)[56] <- 'couple'
pop_dat$couple[pop_dat$sex == 'F'] <- seq(1, length(pop_dat$PID110334[pop_dat$sex == 'F']), 1)
pop_dat$couple[pop_dat$sex == 'M'] <- seq(length(pop_dat$PID110334[pop_dat$sex == 'F'])+1, length(pop_dat$PID110334), 1)
tmp <- lapply(split(pop_dat[pop_dat$sex == 'M',], pop_dat$PID110334[pop_dat$sex == 'M']), function(x) ifelse(x$partner %in% pop_dat$PID110334, pop_dat$couple[which(pop_dat$PID110334 == x$partner)], x$couple))
pop_dat[names(tmp), 'couple'] <- unlist(tmp)

pop_dat <- pop_dat[,-c(4:18,24:29,37,40:41,44)]

# Save file
write.table(pop_dat, file = 'pop_dat_cov_211210.txt')
