# Packages ####
library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(foreign)
## Load data frames #####
Sys.setlocale("LC_TIME", "English")
## HUNT questionnaires
# Set working directory
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")
# Read HUNT questionnaires file
q_hunt <- read.csv("Adult_HUNT.csv", na.strings = c(""), colClasses = "character")
# Recode IDs (PID_110334) to start with "id_" followed by the unique ID
q_hunt$PID_110334 <- paste("id",q_hunt$PID_110334,sep="_")
# Recode names of rows to the unique IDs
rownames(q_hunt) <- q_hunt$PID_110334
# Convert participation dates in character representations to objects of class "Date"
q_hunt$PartDat_NT1BLQ1 <- as.Date(q_hunt$PartDat_NT1BLQ1, format = "%d %b %y")
q_hunt$PartDat_NT2BLQ1 <- as.Date(q_hunt$PartDat_NT2BLQ1, format = "%d %b %y")
q_hunt$PartDat_NT2BLQ2 <- as.Date(q_hunt$PartDat_NT2BLQ2, format = "%d %b %y")
q_hunt$PartDat_NT3BLQ1 <- as.Date(q_hunt$PartDat_NT3BLQ1, format = "%d %b %y")
q_hunt$PartDat_NT4BLM <- as.Date(q_hunt$PartDat_NT4BLM, format = "%d %b %y")
q_hunt$PartDat_NT4BLQ1 <- as.Date(q_hunt$PartDat_NT4BLQ1, format = "%d %b %y")

## Medical Birth Registry of Norway
# Read file
mbrn <- read.table('partners_KASK_210428.txt', sep = ",", header = TRUE, na.strings = c("","             ", NA), colClass = "character")
# Recode IDs (PID_110334_BARN, PID110334_MOR, PID110334_FAR) to start with "id_" followed by the unique ID
# BARN = child, MOR = mother, FAR = father
mbrn$PID110334_BARN[!is.na(mbrn$PID110334_BARN)] <- paste("id",mbrn$PID110334_BARN[!is.na(mbrn$PID110334_BARN)],sep="_")
mbrn$PID110334_MOR[!is.na(mbrn$PID110334_MOR)] <- paste("id",mbrn$PID110334_MOR[!is.na(mbrn$PID110334_MOR)],sep="_")
mbrn$PID110334_FAR[!is.na(mbrn$PID110334_FAR)] <- paste("id",mbrn$PID110334_FAR[!is.na(mbrn$PID110334_FAR)],sep="_")
# Create IDs for people with missing IDs starting with "NA_" followed by a unique number between 1 and total number of people (mother, father, child)
mbrn$PID110334_MOR[is.na(mbrn$PID110334_MOR)] <- paste('NA',seq(from = 1, to = length(mbrn$PID110334_MOR[is.na(mbrn$PID110334_MOR)])), sep="_")
mbrn$PID110334_FAR[is.na(mbrn$PID110334_FAR)] <- paste('NA',seq(from = 1, to = length(mbrn$PID110334_FAR[is.na(mbrn$PID110334_FAR)])), sep="_")
mbrn$PID110334_BARN[is.na(mbrn$PID110334_BARN)] <- paste('NA',seq(from = 1, to = length(mbrn$PID110334_BARN[is.na(mbrn$PID110334_BARN)])), sep="_")
# Convert birth dates (FDATO) and death dates (DDATO) in character representations to objects of class "Date"
mbrn$FDATO <- as.Date(mbrn$FDATO)
mbrn$DDATO <- as.Date(mbrn$DDATO)
# Uniqe children IDs in first column, so mother IDs are not unique if they have more than one child
# Want to use mothers' IDs as rownames
# Create a temprorary table of the mothers' IDs
tmp <- table(mbrn$PID110334_MOR)
# Convert IDs of unknown mothers to missing
mbrn$PID110334_MOR[grep("NA",mbrn$PID110334_MOR)] <- NA
# Create unique mother IDs for each birth
nn <- setNames(paste(rep(names(tmp), tmp), do.call(c,sapply(tmp,function(x)seq_len(x))), sep="_"),  rep(names(tmp), tmp))
# Order data file by order of unique mother IDs above
mbrn <- mbrn[order(factor(mbrn$PID110334_MOR, levels=unique(names(nn)))),]
# Recode names of rows to unique mother IDs
rownames(mbrn) <- unname(nn)

## Create working data frame #####
# Include information on ID, sex, birthyear and fertility into the working data frame
pop_dat <- q_hunt[,c("PID_110334" ,"Sex", "BirthYear", "PregDiffMor1Y_NT2BLQ2", "PregDiffMor1Y_NT3BLQ2", "PregDiffMor1Y_NT4BLQ2")]
# Recode names of columns to lowercase and more intuitive names
colnames(pop_dat) <- c("PID110334", "sex", "birthyear", "h2_fert", "h3_fert", "h4_fert")
# Recode names of rows to the unique IDs
rownames(pop_dat) <- pop_dat$PID110334

## Formatting
# Recode fertility information from Norwegian to English
pop_dat$h2_fert[!is.na(pop_dat$h2_fert) & pop_dat$h2_fert == "Ja"] <- "yes"
pop_dat$h2_fert[!is.na(pop_dat$h2_fert) & pop_dat$h2_fert == "Nei"] <- "no"
pop_dat$h3_fert[!is.na(pop_dat$h3_fert) & pop_dat$h3_fert == "Ja"] <- "yes"
pop_dat$h3_fert[!is.na(pop_dat$h3_fert) & pop_dat$h3_fert == "Nei"] <- "no"
pop_dat$h4_fert[!is.na(pop_dat$h4_fert) & pop_dat$h4_fert == "Ja"] <- "yes"
pop_dat$h4_fert[!is.na(pop_dat$h4_fert) & pop_dat$h4_fert == "Nei"] <- "no"
# Recode sex information from Norwegian to English
pop_dat$sex[pop_dat$sex == "Kvinne"] <- "F"
pop_dat$sex[pop_dat$sex == "Mann"] <- "M"

# A total of 51,241 unique women and 45,272 unique men have participated in HUNT
length(pop_dat$PID110334[pop_dat$sex == 'F'])
length(pop_dat$PID110334[pop_dat$sex == 'M'])

# Add participation status in each HUNT survey (from HUNT questionnaires) in the working file
# If answered a questionnaire (BLQ1 or BLQ2) in HUNT2, they are assigned TRUE in the column 'h2', if not FALSE
# If answered a questionnaire (BLQ1 or BLQ2) in HUNT3, they are assigned TRUE in the column 'h3', if not FALSE
# If answered a questionnaire (BLQ1 or BLQ2) in HUNT4, they are assigned TRUE in the column 'h4', if not FALSE
pop_dat[q_hunt$PID_110334[(!is.na(q_hunt$Part_NT2BLQ1) & q_hunt$Part_NT2BLQ1 == 'Deltatt') | (!is.na(q_hunt$Part_NT2BLQ2) & q_hunt$Part_NT2BLQ2 == 'Deltatt')],7] <- TRUE
pop_dat[q_hunt$PID_110334[(!is.na(q_hunt$Part_NT3BLQ1) & q_hunt$Part_NT3BLQ1 == 'Deltatt') | (!is.na(q_hunt$Part_NT3BLQ2) & q_hunt$Part_NT3BLQ2 == 'Deltatt')],8] <- TRUE
pop_dat[q_hunt$PID_110334[(!is.na(q_hunt$Part_NT4BLQ1) & q_hunt$Part_NT4BLQ1 == 'Deltatt') | (!is.na(q_hunt$Part_NT4BLQ2) & q_hunt$Part_NT4BLQ2 == 'Deltatt')],9] <- TRUE
colnames(pop_dat)[c(7,8,9)] <- c('h2','h3','h4')
pop_dat$h2[is.na(pop_dat$h2)] <- FALSE
pop_dat$h3[is.na(pop_dat$h3)] <- FALSE
pop_dat$h4[is.na(pop_dat$h4)] <- FALSE

# A total of 51,228 unique women and 45,218 unique men have participated in at least HUNT2, HUNT3 or HUNT4
length(pop_dat$PID110334[pop_dat$sex == 'F' & (pop_dat$h2 == TRUE | pop_dat$h3 == TRUE | pop_dat$h4 == TRUE)])
length(pop_dat$PID110334[pop_dat$sex == 'M' & (pop_dat$h2 == TRUE | pop_dat$h3 == TRUE | pop_dat$h4 == TRUE)])

# Add participation date in each HUNT survey (from HUNT questionnaires) in the working file
# If participated in HUNT2 ('h2' == TRUE), participation date (from HUNT2 questionnaire BLQ1) is assigned to the column 'h2_date', if not NA
# If participated in HUNT3 ('h3' == TRUE), participation date (from HUNT3 questionnaire BLQ1) is assigned to the column 'h3_date', if not NA
# If participated in HUNT4 ('h4' == TRUE), participation date (from HUNT4 questionnaire BLQ1) is assigned to the column 'h4_date', if not NA
pop_dat[pop_dat$h2 == TRUE,10] <- q_hunt[pop_dat$PID110334[pop_dat$h2 == TRUE],which(colnames(q_hunt) == 'PartDat_NT2BLQ1')]
pop_dat[pop_dat$h3 == TRUE,11] <- q_hunt[pop_dat$PID110334[pop_dat$h3 == TRUE],which(colnames(q_hunt) == 'PartDat_NT3BLQ1')]
pop_dat[pop_dat$h4 == TRUE,12] <- q_hunt[pop_dat$PID110334[pop_dat$h4 == TRUE],which(colnames(q_hunt) == 'PartDat_NT4BLQ1')]
colnames(pop_dat)[c(10,11,12)] <- c('h2_date','h3_date','h4_date')
# Add participation date from BLQ2 in HUNT2 if participation date from HUNT2 questionnaire BLQ1 is not available
pop_dat$h2_date[is.na(pop_dat$h2_date) & pop_dat$h2 == TRUE] <- q_hunt[pop_dat$PID110334[is.na(pop_dat$h2_date) & pop_dat$h2 == TRUE],which(colnames(q_hunt) == 'PartDat_NT2BLQ2')]
# Add participation date from interviews in HUNT4 if participation date from HUNT4 questionnaire BLQ1 is not available
pop_dat$h4_date[is.na(pop_dat$h4_date) & pop_dat$h4 == TRUE] <- q_hunt[pop_dat$PID110334[is.na(pop_dat$h4_date) & pop_dat$h4 == TRUE],which(colnames(q_hunt) == 'PartDat_NT4BLM')]

# Add age at participation in each HUNT survey (from HUNT questionnaires) in the working file
# If participated in HUNT2 ('h2' == TRUE), age at participation (from HUNT2 questionnaire BLQ1) is assigned to the column 'h2_age', if not NA
# If participated in HUNT3 ('h3' == TRUE), age at participation (from HUNT3 questionnaire BLQ1) is assigned to the column 'h3_age', if not NA
# If participated in HUNT4 ('h4' == TRUE), age at participation (from HUNT4 questionnaire BLQ1) is assigned to the column 'h4_age', if not NA
pop_dat[pop_dat$h2 == TRUE,13] <- q_hunt[pop_dat$PID110334[pop_dat$h2 == TRUE],which(colnames(q_hunt) == 'PartAg_NT2BLQ1')]
pop_dat[pop_dat$h3 == TRUE,14] <- q_hunt[pop_dat$PID110334[pop_dat$h3 == TRUE],which(colnames(q_hunt) == 'PartAg_NT3BLQ1')]
pop_dat[pop_dat$h4 == TRUE,15] <- q_hunt[pop_dat$PID110334[pop_dat$h4 == TRUE],which(colnames(q_hunt) == 'PartAg_NT4BLQ1')]
colnames(pop_dat)[c(13,14,15)] <- c('h2_age','h3_age','h4_age')

# Add indicator in the working file for prioritizing which HUNT surveys to use if women have participated in more than one survey
# Higher prioritizing (1) if women are below the age of 50 when participating and have provided fertility information
# Lower prioritizing (2) if women are 50 years or older when participating and have provided fertility information
# Add indicator to women participating in HUNT2
pop_dat$h2_pri[pop_dat$sex == 'F' & !is.na(pop_dat$h2_fert) & pop_dat$h2 == TRUE & !is.na(pop_dat$h2_age) & pop_dat$h2_age < 50] <- '1'
pop_dat$h2_pri[pop_dat$sex == 'F' & !is.na(pop_dat$h2_fert) & pop_dat$h2 == TRUE & pop_dat$h2_age >= 50] <- '2'
# Add indicator to women participating in HUNT3
pop_dat$h3_pri[pop_dat$sex == 'F' & !is.na(pop_dat$h3_fert) & pop_dat$h3 == TRUE & !is.na(pop_dat$h3_age) & pop_dat$h3_age < 50] <- '1'
pop_dat$h3_pri[pop_dat$sex == 'F' & !is.na(pop_dat$h3_fert) & pop_dat$h3 == TRUE & pop_dat$h3_age >= 50] <- '2'
# Add indicator to women participating in HUNT4
pop_dat$h4_pri[pop_dat$sex == 'F' & !is.na(pop_dat$h4_fert) & pop_dat$h4 == TRUE & !is.na(pop_dat$h4_age) & pop_dat$h4_age < 50] <- '1'
pop_dat$h4_pri[pop_dat$sex == 'F' & !is.na(pop_dat$h4_fert) & pop_dat$h4 == TRUE & pop_dat$h4_age >= 50] <- '2'

# Add chosen HUNT survey to the female participants in the working file based on indicators from each HUNT surveys above
# If women have participated in only one survey regardless of indicators (2 or 1), choose the survey which they have participated in
# If women have participated in more than one survey and have one of each of both indicators (2 and 1) for different surveys, choose the survey with indicator 2 where they are below the age of 50
# If women have participated in more than one survey and have more than one of indicator 1 (1 and 1), choose the survey closest to the age of 50 (e.g. HUNT3 over HUNT4)
# If women have participated in more than one survey and have more than one of indicator 2 (2 and 2), choose the survey latest in time (e.g. HUNT4 over HUNT3)
pop_dat$survey[(pop_dat$h2_pri == '1' & (pop_dat$h3_pri == '2' | is.na(pop_dat$h3_pri)) & (pop_dat$h4_pri == '2' | is.na(pop_dat$h4_pri))) | pop_dat$h2_pri == '2'] <- 'h2'
pop_dat$survey[(pop_dat$h3_pri == '1' & (is.na(pop_dat$h4_pri) | pop_dat$h4_pri == '2')) | (pop_dat$h3_pri == '2' & is.na(pop_dat$h2_pri) & (pop_dat$h4_pri == '2' | is.na(pop_dat$h4_pri)))] <- 'h3'
pop_dat$survey[pop_dat$h4_pri == '1' | (pop_dat$h4_pri == '2' & is.na(pop_dat$h2_pri) & is.na(pop_dat$h3_pri))] <- 'h4'

# Add corresponding fertility information from chosen HUNT survey to a new column
pop_dat$fert[pop_dat$survey == 'h2' & !is.na(pop_dat$survey)] <- pop_dat$h2_fert[pop_dat$survey == 'h2' & !is.na(pop_dat$survey)]
pop_dat$fert[pop_dat$survey == 'h3' & !is.na(pop_dat$survey)] <- pop_dat$h3_fert[pop_dat$survey == 'h3' & !is.na(pop_dat$survey)]
pop_dat$fert[pop_dat$survey == 'h4' & !is.na(pop_dat$survey)] <- pop_dat$h4_fert[pop_dat$survey == 'h4' & !is.na(pop_dat$survey)]

# Add corresponding participation date from chosen HUNT survey to a new column
pop_dat$survey_date[pop_dat$survey == 'h2' & !is.na(pop_dat$survey)] <- as.character(pop_dat$h2_date[pop_dat$survey == 'h2' & !is.na(pop_dat$survey)])
pop_dat$survey_date[pop_dat$survey == 'h3' & !is.na(pop_dat$survey)] <- as.character(pop_dat$h3_date[pop_dat$survey == 'h3' & !is.na(pop_dat$survey)])
pop_dat$survey_date[pop_dat$survey == 'h4' & !is.na(pop_dat$survey)] <- as.character(pop_dat$h4_date[pop_dat$survey == 'h4' & !is.na(pop_dat$survey)])
pop_dat$survey_date <- as.Date(pop_dat$survey_date)

# Add corresponding age at participation from chosen HUNT survey to a new column
pop_dat$survey_age[pop_dat$survey == 'h2' & !is.na(pop_dat$survey)] <- pop_dat$h2_age[pop_dat$survey == 'h2' & !is.na(pop_dat$survey)]
pop_dat$survey_age[pop_dat$survey == 'h3' & !is.na(pop_dat$survey)] <- pop_dat$h3_age[pop_dat$survey == 'h3' & !is.na(pop_dat$survey)]
pop_dat$survey_age[pop_dat$survey == 'h4' & !is.na(pop_dat$survey)] <- pop_dat$h4_age[pop_dat$survey == 'h4' & !is.na(pop_dat$survey)]

# Exclude female participants who are registered as fathers in the Medical Birth registry, or who are registered as mothers but have female partners who are registered as fathers,to exclude any same-sex couples
pop_dat <- pop_dat[which(!(pop_dat$sex == 'F' & (pop_dat$PID110334 %in% mbrn$PID110334_FAR | pop_dat$PID110334 %in% mbrn$PID110334_MOR[which(mbrn$PID110334_FAR %in% pop_dat$PID110334[pop_dat$sex == 'F'])]))),]

# A total of 37,981 unique women who provided information about fertility
length(pop_dat$PID110334[pop_dat$sex == 'F' & !is.na(pop_dat$fert) & !is.na(pop_dat$survey)])

# Exclude female participants who did not participate in either HUNT2, HUNT3 or HUNT4, or who did not report information on fertility
pop_dat <- pop_dat[pop_dat$sex == 'M' | (pop_dat$sex == 'F' & !is.na(pop_dat$fert) & !is.na(pop_dat$survey)),]

## Identification of the male population
# Add children, mothers and fathers from the Medical Birth Registry of Norway (MBRN) who participated in HUNT and their birth date to a new data frame
partn_dat <- mbrn[which(map(strsplit(rownames(mbrn), "(?<=[0-9])(_)", perl = T),1) %in% pop_dat$PID110334[!is.na(pop_dat$survey)]), c(1,2,3,which(colnames(mbrn) == 'FDATO'))]

# Add column with date of participation in HUNT
partn_dat$survey_date <- pop_dat[partn_dat$PID110334_MOR, 21]

# Add column specifying if birth minus approx. pregnancy length happened before or after participation in HUNT
partn_dat$child_b4survey <- ifelse(!is.na(partn_dat$FDATO) & !is.na(partn_dat$survey_date) & (partn_dat$FDATO - 280) < partn_dat$survey_date, TRUE, FALSE)

## Remove any fathers who have children with mothers who have children with multiple fathers
# Add column specifying if mothers (f = female) had children with different fathers
tmp <- rep(sapply(split(partn_dat$PID110334_FAR, partn_dat$PID110334_MOR), function(x) length(unique(x)) == 1), sapply(split(partn_dat$PID110334_FAR ,partn_dat$PID110334_MOR), length))
partn_dat$samepartn_f[order(factor(partn_dat$PID110334_MOR, levels=unique(names(tmp))))] <- unlist(tmp)

# Add information on whether mothers had children with several fathers, and whether it happened before or after participation in HUNT, to the correct IDs in partn_dat
tmp <- rep(lapply(split(partn_dat, partn_dat$PID110334_MOR), function(x) ifelse(x$samepartn_f[1], TRUE, ifelse(sum(x$child_b4survey) != 0, ifelse(length(x$PID110334_FAR[x$PID110334_FAR == x$PID110334_FAR[1]]) >= sum(x$child_b4survey), TRUE, FALSE), FALSE))), sapply(split(partn_dat$PID110334_FAR, partn_dat$PID110334_MOR), length))
partn_dat$samepartn_f_b4survey[order(factor(partn_dat$PID110334_MOR, levels=unique(names(tmp))))] <- unlist(tmp)


# Include only fathers and mothers where the mother had only given birth to children with the same partner before participation in HUNT
partn_dat <- partn_dat[partn_dat$samepartn_f == TRUE | (partn_dat$samepartn_f_b4survey == TRUE & partn_dat$child_b4survey == TRUE),]

# Convert IDs of unkown fathers to missing
partn_dat$PID110334_FAR[grep("NA",partn_dat$PID110334_FAR)] <- NA

# Remove missing fathers from data frame
partn_dat <- partn_dat[which(!is.na(partn_dat$PID110334_FAR)),]

# Add information on whether fathers (m = male) had children with different mothers
tmp <- rep(sapply(split(partn_dat$PID110334_MOR, partn_dat$PID110334_FAR), function(x) length(unique(x)) == 1), sapply(split(partn_dat$PID110334_MOR, partn_dat$PID110334_FAR), length))
partn_dat$samepartn_m[order(factor(partn_dat$PID110334_FAR, levels=unique(names(tmp))))] <- unlist(tmp)

# Add information on whether fathers had children with several mothers, and whether it happened before or after participation in HUNT, to the correct IDs in partn_dat
tmp <- rep(sapply(split(partn_dat, partn_dat$PID110334_FAR), function(x) ifelse(x$samepartn_m[1], TRUE, ifelse(sum(x$child_b4survey) != 0, ifelse(length(x$PID110334_MOR[x$PID110334_MOR == x$PID110334_MOR[order(x$FDATO)][1]]) >= length(x$PID110334_MOR[x$child_b4survey == TRUE]), TRUE, FALSE), FALSE))), sapply(split(partn_dat$PID110334_MOR, partn_dat$PID110334_FAR), length))
partn_dat$samepartn_m_b4survey[order(factor(partn_dat$PID110334_FAR, levels=unique(names(tmp))))] <- unlist(tmp)

# Include only fathers and mothers where the father had only given birth to children with the same partner before participation in HUNT
partn_dat <- partn_dat[partn_dat$samepartn_m == TRUE | (partn_dat$samepartn_m_b4survey == TRUE & partn_dat$child_b4survey == TRUE),]


# Study population restricted to women without registered children and women and men who are different-sex parents. The male population is also restricted to those in
# parentships where the mothers have only had children with a single father before their participation in HUNT and where the fathers are only linked to a single
# mother or to multiple mothers of identical fertility status
pop_dat <- pop_dat[pop_dat$sex == 'F' | (pop_dat$sex == 'M' & pop_dat$PID110334 %in% partn_dat$PID110334_FAR),]


## Add partner IDs from partners identified through the Medical Birth Registry of Norway to the pop_dat data frame
# Create a temporary list storing the male partner IDs for each female participant with a registered partner in the Medical Birth Registry of Norway
tmp <- lapply(split(pop_dat[pop_dat$sex == 'F',], pop_dat$PID110334[pop_dat$sex == 'F']), function(x) ifelse(partn_dat$PID110334_FAR[partn_dat$PID110334_MOR == x$PID110334] %in% pop_dat$PID110334[pop_dat$sex == 'M'] &
                                                                                                               (partn_dat$samepartn_f[partn_dat$PID110334_MOR == x$PID110334] == TRUE |
                                                                                                                  (partn_dat$samepartn_f_b4survey[partn_dat$PID110334_MOR == x$PID110334] == TRUE)),
                                                                                                             partn_dat$PID110334_FAR[partn_dat$PID110334_MOR == x$PID110334 & partn_dat$FDATO == min(partn_dat$FDATO[partn_dat$PID110334_MOR == x$PID110334])], NA))
# Remove duplicate IDs and missing/no observation of partners
tmp <- lapply(tmp, function(x) unique(unlist(x)))
tmp <- lapply(tmp, na.omit)
tmp <- tmp[tmp != 'logical(0)']
pop_dat[names(tmp),23] <- unlist(tmp)
colnames(pop_dat)[23] <- 'partner'

# Create a temporary list storing the female partner IDs for each male participant
tmp <- lapply(split(pop_dat[pop_dat$sex == 'M',], pop_dat$PID110334[pop_dat$sex == 'M']), function(x) ifelse(partn_dat$samepartn_f[partn_dat$PID110334_FAR == x$PID110334] == TRUE |
                                                                                                               partn_dat$samepartn_f_b4survey[partn_dat$PID110334_FAR == x$PID110334] == TRUE,
                                                                                                                    partn_dat$PID110334_MOR[partn_dat$PID110334_FAR == x$PID110334 & partn_dat$FDATO == min(partn_dat$FDATO[partn_dat$PID110334_FAR == x$PID110334])], NA))
# Remove duplicate IDs and missing/no observation of partners
tmp <- lapply(tmp, function(x) unique(unlist(x)))
tmp <- lapply(tmp, na.omit)
tmp <- tmp[tmp != 'logical(0)']
pop_dat[names(tmp),23] <- unlist(tmp)

# If male participants are registered as partners more than once, remove the partner ID from 
pop_dat$partner[pop_dat$PID110334 %in% pop_dat$PID110334[which(pop_dat$partner %in% pop_dat$partner[!is.na(pop_dat$partner) & duplicated(pop_dat$partner)])] &
                  !(pop_dat$PID110334 %in% pop_dat$partner[which(pop_dat$partner %in% pop_dat$PID110334[which(pop_dat$partner %in% pop_dat$partner[!is.na(pop_dat$partner) & duplicated(pop_dat$partner)])])])] <- NA


# Add indicator in the working file for prioritizing which HUNT surveys to use if men have participated in more than one survey
# Higher prioritizing (1) if men have answered both questionnaires (BLQ1 and BLQ2)
# Lower prioritizing (2) if men have answered only one of the questionnaires (BLQ1 or BLQ2)
# Add indicator to men participating in HUNT2
pop_dat$h2_pri[pop_dat$sex == 'M' & pop_dat$h2 == TRUE & pop_dat$PID110334 %in% q_hunt$PID_110334[!is.na(q_hunt$Part_NT2BLQ1) & q_hunt$Part_NT2BLQ1 == 'Deltatt' &
                                                                                                    !is.na(q_hunt$Part_NT2BLQ2) & q_hunt$Part_NT2BLQ2 == 'Deltatt']] <- '1'
pop_dat$h2_pri[pop_dat$sex == 'M' & pop_dat$h2 == TRUE & pop_dat$PID110334 %in% q_hunt$PID_110334[(!is.na(q_hunt$Part_NT2BLQ1) & q_hunt$Part_NT2BLQ1 == 'Deltatt' &
                                                                                                     (is.na(q_hunt$Part_NT2BLQ2) | q_hunt$Part_NT2BLQ2 == 'Invitert, ikke deltatt')) |
                                                                                                    (!is.na(q_hunt$Part_NT2BLQ2) & q_hunt$Part_NT2BLQ2 == 'Deltatt' &
                                                                                                       (is.na(q_hunt$Part_NT2BLQ1) | q_hunt$Part_NT2BLQ1 == 'Invitert, ikke deltatt'))]] <- '2'
# Add indicator to men participating in HUNT3
pop_dat$h3_pri[pop_dat$sex == 'M' & pop_dat$h3 == TRUE & pop_dat$PID110334 %in% q_hunt$PID_110334[!is.na(q_hunt$Part_NT3BLQ1) & q_hunt$Part_NT3BLQ1 == 'Deltatt' &
                                                                                                    !is.na(q_hunt$Part_NT3BLQ2) & q_hunt$Part_NT3BLQ2 == 'Deltatt']] <- '1'
pop_dat$h3_pri[pop_dat$sex == 'M' & pop_dat$h3 == TRUE & pop_dat$PID110334 %in% q_hunt$PID_110334[(!is.na(q_hunt$Part_NT3BLQ1) & q_hunt$Part_NT3BLQ1 == 'Deltatt' &
                                                                                                     (is.na(q_hunt$Part_NT3BLQ2) | q_hunt$Part_NT3BLQ2 == 'Invitert, ikke deltatt')) |
                                                                                                    (!is.na(q_hunt$Part_NT3BLQ2) & q_hunt$Part_NT3BLQ2 == 'Deltatt' &
                                                                                                       (is.na(q_hunt$Part_NT3BLQ1) | q_hunt$Part_NT3BLQ1 == 'Invitert, ikke deltatt'))]] <- '2'
# Add indicator to men participating in HUNT4
pop_dat$h4_pri[pop_dat$sex == 'M' & pop_dat$h4 == TRUE & pop_dat$PID110334 %in% q_hunt$PID_110334[!is.na(q_hunt$Part_NT4BLQ1) & q_hunt$Part_NT4BLQ1 == 'Deltatt' &
                                                                                                    !is.na(q_hunt$Part_NT4BLQ2) & q_hunt$Part_NT4BLQ2 == 'Deltatt']] <- '1'
pop_dat$h4_pri[pop_dat$sex == 'M' & pop_dat$h4 == TRUE & pop_dat$PID110334 %in% q_hunt$PID_110334[(!is.na(q_hunt$Part_NT4BLQ1) & q_hunt$Part_NT4BLQ1 == 'Deltatt' &
                                                                                                     (is.na(q_hunt$Part_NT4BLQ2) | q_hunt$Part_NT4BLQ2 == 'Invitert, ikke deltatt')) |
                                                                                                    (!is.na(q_hunt$Part_NT4BLQ2) & q_hunt$Part_NT4BLQ2 == 'Deltatt' &
                                                                                                       (is.na(q_hunt$Part_NT4BLQ1) | q_hunt$Part_NT4BLQ1 == 'Invitert, ikke deltatt'))]] <- '2'

# Add chosen HUNT survey to male participants in the working file based on indicators from each HUNT surveys above
# If men have a partner participating in HUNT2, and they have participated in HUNT2, choose HUNT2
# If men have a partner participating in HUNT2, but they have not participated in HUNT2, choose the survey closest in time (HUNT3 if possible, HUNT4 if not)
# If men have a partner participating in HUNT3, and they have participated in HUNT3, choose HUNT3
# If men have a partner participating in HUNT3, but they have not participated in HUNT3, choose the survey latest in time (HUNT4 if possible, HUNT2 if not)
# If men have a partner participating in HUNT4, and they have participated in HUNT4, choose HUNT4
# If men have a partner participating in HUNT4, but they have not participated in HUNT4, choose the survey closest in time (HUNT3 if possible, HUNT2 if not)
# If men have more than one registered partner (with equal fertility status), choose the first survey in time
tmp <- lapply(split(pop_dat[pop_dat$sex == 'M',], pop_dat$PID110334[pop_dat$sex == 'M']),
              function(x) ifelse(pop_dat$survey[which(pop_dat$partner == x$PID110334 & x$partner == pop_dat$PID110334)] == 'h2',
                                 ifelse(x$h2 == TRUE,
                                        ifelse(x$h2_pri == '1', 'h2',
                                               ifelse(x$h2_pri == '2',
                                                      ifelse(x$h3 == TRUE & x$h3_pri == '1', 'h3',
                                                             ifelse(x$h4 == TRUE & x$h4_pri == '1', 'h4', 'h2')), NA)),
                                        ifelse(x$h3 == TRUE,
                                               ifelse(x$h3_pri == '1', 'h3',
                                                      ifelse(x$h3_pri == '2',
                                                             ifelse(x$h4 == TRUE & x$h4_pri == '1','h4', 'h3'), NA)),
                                               ifelse(x$h4 == TRUE, 'h4', NA))),
                                 
                                 ifelse(pop_dat$survey[which(pop_dat$partner == x$PID110334 & x$partner == pop_dat$PID110334)] == 'h3',
                                        ifelse(x$h3 == TRUE,
                                               ifelse(x$h3_pri == '1', 'h3',
                                                      ifelse(x$h3_pri == '2',
                                                             ifelse(x$h4 == TRUE & x$h4_pri == '1', 'h4',
                                                                    ifelse(x$h2 == TRUE & x$h2_pri == '1', 'h2', 'h3')), NA)),
                                               ifelse(x$h4 == TRUE,
                                                      ifelse(x$h4_pri == '1', 'h4',
                                                             ifelse(x$h4_pri == '2',
                                                                    ifelse(x$h2 == TRUE & x$h2_pri == '1', 'h2', 'h4'))),
                                                      ifelse(x$h2 == TRUE, 'h2', NA))),
                                        
                                        ifelse(pop_dat$survey[which(pop_dat$partner == x$PID110334 & x$partner == pop_dat$PID110334)] == 'h4',
                                               ifelse(x$h4 == TRUE,
                                                      ifelse(x$h4_pri == '1', 'h4',
                                                             ifelse(x$h4_pri == '2',
                                                                    ifelse(x$h3 == TRUE & x$h3_pri == '1', 'h3',
                                                                           ifelse(x$h2 == TRUE & x$h2_pri == '1', 'h2', 'h4')), NA)),
                                                      ifelse(x$h3 == TRUE,
                                                             ifelse(x$h3_pri == '1', 'h3',
                                                                    ifelse(x$h3_pri == '2',
                                                                           ifelse(x$h2 == TRUE & x$h2_pri == '1', 'h2', 'h3'), NA)),
                                                             ifelse(x$h2 == TRUE, 'h2', NA))), NA))))
pop_dat[names(tmp), 'survey'] <- unlist(tmp)

# Exclude participants who did not participate in either HUNT2, HUNT3 or HUNT4
pop_dat <- pop_dat[!is.na(pop_dat$survey),]

# Add fertility information for males from the information provided by their partners
tmp <- lapply(split(pop_dat[pop_dat$sex == 'M',], pop_dat$PID110334[pop_dat$sex == 'M']), function(x) pop_dat$fert[which(pop_dat$partner == x$PID110334 & x$partner == pop_dat$PID110334)])
pop_dat[names(tmp), 'fert'] <- unlist(tmp)

# Add corresponding participation date for males from chosen HUNT survey
pop_dat[pop_dat$sex == 'M' & pop_dat$survey == 'h2','survey_date'] <- q_hunt[pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$survey == 'h2'], 'PartDat_NT2BLQ1']
pop_dat[pop_dat$sex == 'M' & pop_dat$survey == 'h3','survey_date'] <- q_hunt[pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$survey == 'h3'], 'PartDat_NT3BLQ1']
pop_dat[pop_dat$sex == 'M' & pop_dat$survey == 'h4','survey_date'] <- ifelse(!is.na(q_hunt[pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$survey == 'h4'], 'PartDat_NT4BLQ1']),
                                                                         as.character(q_hunt[pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$survey == 'h4'], 'PartDat_NT4BLQ1']),
                                                                         as.character(q_hunt[pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$survey == 'h4'], 'PartDat_NT4BLM']))
pop_dat[pop_dat$sex == 'M' & pop_dat$survey == 'h4','survey_date'] <- as.Date(pop_dat[pop_dat$sex == 'M' & pop_dat$survey == 'h4','survey_date'])

# Add corresponding age at participation for males from chosen HUNT survey
pop_dat[pop_dat$sex == 'M' & pop_dat$survey == 'h2','survey_age'] <- q_hunt[pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$survey == 'h2'], 'PartAg_NT2BLQ1']
pop_dat[pop_dat$sex == 'M' & pop_dat$survey == 'h3','survey_age'] <- q_hunt[pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$survey == 'h3'], 'PartAg_NT3BLQ1']
pop_dat[pop_dat$sex == 'M' & pop_dat$survey == 'h4','survey_age'] <- ifelse(!is.na(q_hunt[pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$survey == 'h4'], 'PartAg_NT4BLQ1']),
                                                                        q_hunt[pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$survey == 'h4'], 'PartAg_NT4BLQ1'],
                                                                        q_hunt[pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$survey == 'h4'], 'PartAg_NT4BLM'])

## Identify participants who are voluntarily childless or who have not yet tried to conceive
# Add information on number of children reported by participants in their chosen HUNT survey
# This information was reported in both questionnaire 1 (BLQ1) and questionnaire 2 (BLQ2) in HUNT 2. Use BLQ1 if available, if not BLQ2
pop_dat$child_rep[pop_dat$survey == 'h2'] <- ifelse(!is.na(q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'DelivN_NT2BLQ1']), q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'DelivN_NT2BLQ1'], q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'DelivN_NT2BLQ2'])
pop_dat$child_rep[pop_dat$survey == 'h3'] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'DelivN_NT3BLI']
pop_dat$child_rep[pop_dat$survey == 'h4'] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'DelivN_NT4BLI']
# Recode 8 or more children to '8<'
pop_dat$child_rep[!is.na(pop_dat$child_rep) & (pop_dat$child_rep == '8 eller flere' | pop_dat$child_rep == '8 eller mer')] <- '8<'

# Add information on number of children registered in the medical birth registry for female participants who had conceived
# Do not include abortions in total number of children
# Do not include still births if baby is born before week 22 or with a weight below 500g in total number of children
# Do not include any births after participation in HUNT in total number of children
tmp <- lapply(split(mbrn[mbrn$PID110334_MOR %in% pop_dat$PID110334,], mbrn$PID110334_MOR[mbrn$PID110334_MOR %in% pop_dat$PID110334]),
              function(x) min(as.numeric(x$PARITET_5)) +
                length(x$PID110334_MOR) - sum(x$DODKAT == '10' | (x$DODKAT %in% c('7','8','9') &
                                                                    ((!is.na(x$VEKT) & x$VEKT < '500' & !is.na(x$SVLEN) & x$SVLEN < '22') |
                                                                       (is.na(x$VEKT) & !is.na(x$SVLEN) & x$SVLEN < '22') |
                                                                       (is.na(x$SVLEN) & !is.na(x$VEKT) & x$VEKT < '500'))) |
                                                (x$FDATO >= pop_dat[x$PID110334_MOR, 'survey_date'] &
                                                   !is.na(x$FDATO) & !is.na(pop_dat[x$PID110334_MOR, 'survey_date'])), na.rm = T))
pop_dat[names(tmp),25] <- unlist(tmp)
colnames(pop_dat)[25] <- 'birth_reg'

# Add information on number of children registered in the medical birth registry for male participants
# Do not include abortions in total number of children
# Do not include still births if baby is born before week 22 or with a weight below 500g in total number of children
# Do not include any births after participation in HUNT in total number of children
tmp <- lapply(split(mbrn[mbrn$PID110334_FAR %in% pop_dat$PID110334,], mbrn$PID110334_FAR[mbrn$PID110334_FAR %in% pop_dat$PID110334]),
              function(x) min(as.numeric(x$PARITET_5)) +
                length(x$PID110334_FAR) - sum(x$DODKAT == '10' | (x$DODKAT %in% c('7','8','9') &
                                                                    ((!is.na(x$VEKT) & x$VEKT < '500' & !is.na(x$SVLEN) & x$SVLEN < '22') |
                                                                       (is.na(x$VEKT) & !is.na(x$SVLEN) & x$SVLEN < '22') |
                                                                       (is.na(x$SVLEN) & !is.na(x$VEKT) & x$VEKT < '500'))) |
                                                (x$FDATO >= pop_dat[which(pop_dat$partner == unique(x$PID110334_FAR)), 'survey_date'] &
                                                   !is.na(x$FDATO) & !is.na(pop_dat[which(pop_dat$partner == unique(x$PID110334_FAR)), 'survey_date'])), na.rm = T))
pop_dat[names(tmp),25] <- unlist(tmp)

# If information on number of children is reported in the chosen HUNT survey, use this information
# If participants reported no children, yet had a registration in the birth registry before participation in HUNT, use information from birth registry
# If information on number of child_tot is not reported in the chosen HUNT survey, use the information from the birth registry
pop_dat$child_tot <- ifelse(!is.na(pop_dat$child_rep),
                           ifelse((pop_dat$child_rep == '0' & is.na(pop_dat$birth_reg)) | pop_dat$child_rep != '0',
                                  pop_dat$child_rep, pop_dat$birth_reg), pop_dat$birth_reg)

# Recode 3 or more child_tot to '2<'
pop_dat$child_tot[!is.na(pop_dat$child_tot) & pop_dat$child_tot %in% c('3','4','5','6','7','8','9','10','11','8<')] <- '2<'

# A total of 4857 women responded that they had never spent more than 12 months trying to get pregnant, yet did not have child_tot by the time of HUNT participation
length(pop_dat[((!is.na(pop_dat$child_tot) & pop_dat$child_tot == '0') | is.na(pop_dat$child_tot)) & pop_dat$fert == 'no' & pop_dat$sex == 'F',1])
# A total of 463 men were identified as partners of female participants in HUNT, yet did not have child_tot by the time of their partners' HUNT participation
length(pop_dat[((!is.na(pop_dat$child_tot) & pop_dat$child_tot == '0') | is.na(pop_dat$child_tot)) & pop_dat$fert == 'no' & pop_dat$sex == 'M',1])

# Exclude any participants who responded that they had never spent more than 12 months trying to get pregnant, yet did not have child_tot
pop_dat <- pop_dat[!(((!is.na(pop_dat$child_tot) & pop_dat$child_tot == '0') | is.na(pop_dat$child_tot)) & pop_dat$fert == 'no'),]

# A total of 33,089 unique women and 19,161 unique men included in the study population
length(pop_dat[pop_dat$sex == 'F',])
length(pop_dat[pop_dat$sex == 'M',])

## Exclude participants with a history of stroke or coronary heart disease
## Add reported history of disease from HUNT questionnaires
# Add information on history of myocardial infarction
pop_dat[pop_dat$survey == 'h2', 27] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'CarInfEv_NT2BLQ1']
pop_dat[pop_dat$survey == 'h3', 27] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'CarInfEv_NT3BLQ1']
pop_dat[pop_dat$survey == 'h4', 27] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'CarInfEv_NT4BLQ1']
colnames(pop_dat)[27] <- 'infarct_q'
pop_dat[!is.na(pop_dat$infarct) & pop_dat$infarct == 'Ja', 'infarct_q'] <- TRUE
pop_dat[!is.na(pop_dat$infarct) & pop_dat$infarct == 'Nei', 'infarct_q'] <- FALSE

# Add information on history of stroke
pop_dat[pop_dat$survey == 'h2', 28] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'ApoplEv_NT2BLQ1']
pop_dat[pop_dat$survey == 'h3', 28] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'ApoplEv_NT3BLQ1']
pop_dat[pop_dat$survey == 'h4', 28] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'ApoplEv_NT4BLQ1']
colnames(pop_dat)[28] <- 'stroke_q'
pop_dat[!is.na(pop_dat$stroke) & pop_dat$stroke == 'Ja', 'stroke_q'] <- TRUE
pop_dat[!is.na(pop_dat$stroke) & pop_dat$stroke == 'Nei', 'stroke_q'] <- FALSE

# Add information on history of angina
pop_dat[pop_dat$survey == 'h2', 29] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h2'], 'CarAngEv_NT2BLQ1']
pop_dat[pop_dat$survey == 'h3', 29] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h3'], 'CarAngEv_NT3BLQ1']
pop_dat[pop_dat$survey == 'h4', 29] <- q_hunt[pop_dat$PID110334[pop_dat$survey == 'h4'], 'CarAngEv_NT4BLQ1']
colnames(pop_dat)[29] <- 'angina_q'
pop_dat[!is.na(pop_dat$angina) & pop_dat$angina == 'Ja', 'angina_q'] <- TRUE
pop_dat[!is.na(pop_dat$angina) & pop_dat$angina == 'Nei', 'angina_q'] <- FALSE

## Add history of disease from hospital records
# Read file
cvd_diag_hunt <- read.csv("HNT_Data_HUNT_Adults.csv",na.strings = c(" ", NA), colClasses = 'character')
# Recode IDs (PID_110334) to start with "id_" followed by the unique ID
cvd_diag_hunt$PID_110334 <- paste("id",cvd_diag_hunt$PID_110334,sep="_")
# Recode names of columns
colnames(cvd_diag_hunt)[c(2,3,4,5,6,9)] <- c('death','sex','diagnosis','ward','polyclinic','diag_type')
# Convert dates in character representations to objects of class "Date"
cvd_diag_hunt$death <- as.Date(paste(15, str_sub(cvd_diag_hunt$death, 5,6), str_sub(cvd_diag_hunt$death, 1,4)), format = "%d %m %Y")
cvd_diag_hunt$diagnosis <- as.Date(paste(15, str_sub(cvd_diag_hunt$diagnosis, 5,6), str_sub(cvd_diag_hunt$diagnosis, 1,4)), format = "%d %m %Y")
cvd_diag_hunt$ward <- as.Date(paste(15, str_sub(cvd_diag_hunt$ward, 5,6), str_sub(cvd_diag_hunt$ward, 1,4)), format = "%d %m %Y")
cvd_diag_hunt$polyclinic <- as.Date(paste(15, str_sub(cvd_diag_hunt$polyclinic, 5,6), str_sub(cvd_diag_hunt$polyclinic, 1,4)), format = "%d %m %Y")
# Recode diagnosis information from Norwegian to English
cvd_diag_hunt$diag_type[cvd_diag_hunt$diag_type == 'H'] <- 'P'
cvd_diag_hunt$diag_type[cvd_diag_hunt$diag_type == 'B'] <- 'S'
cvd_diag_hunt$sex[cvd_diag_hunt$sex == 'K'] <- 'F'

# Recode names of rows to the unique IDs
tmp <- table(cvd_diag_hunt$PID_110334)
nn <- setNames(paste(rep(names(tmp), tmp), do.call(c,sapply(tmp,function(x)seq_len(x))), sep="_"),  rep(names(tmp), tmp))
cvd_diag_hunt <- cvd_diag_hunt[order(factor(cvd_diag_hunt$PID_110334, levels=unique(names(nn)))),]
rownames(cvd_diag_hunt) <- unname(nn)

# Add information on history of stroke (ICD9: 430, 431, 432, 433, 434, 436; ICD10: I60, I61, I63, I64)
cvd_diag_hunt[(!is.na(cvd_diag_hunt$ICD9) & (grepl('430', cvd_diag_hunt$ICD9) | grepl('431', cvd_diag_hunt$ICD9) | grepl('432', cvd_diag_hunt$ICD9) | grepl('433', cvd_diag_hunt$ICD9) | grepl('434', cvd_diag_hunt$ICD9) | grepl('436', cvd_diag_hunt$ICD9))) |
                (!is.na(cvd_diag_hunt$ICD10) & (grepl('60', cvd_diag_hunt$ICD10) | grepl('61', cvd_diag_hunt$ICD10) | grepl('63', cvd_diag_hunt$ICD10) | grepl('64', cvd_diag_hunt$ICD10))),10] <- TRUE
colnames(cvd_diag_hunt)[10] <- 'stroke_h'
# Add information on history of coronary heart disaese (ICD9: 410, 411, 412, 413, 414; ICD10: I20, I21, I22, I23, I24, I25)
cvd_diag_hunt[(!is.na(cvd_diag_hunt$ICD9) & (grepl('410', cvd_diag_hunt$ICD9) | grepl('411', cvd_diag_hunt$ICD9) | grepl('412', cvd_diag_hunt$ICD9) | grepl('413', cvd_diag_hunt$ICD9) | grepl('414', cvd_diag_hunt$ICD9))) |
                (!is.na(cvd_diag_hunt$ICD10) & (grepl('20', cvd_diag_hunt$ICD10) | grepl('21', cvd_diag_hunt$ICD10) | grepl('22', cvd_diag_hunt$ICD10) | grepl('23', cvd_diag_hunt$ICD10) | grepl('24', cvd_diag_hunt$ICD10) | grepl('25', cvd_diag_hunt$ICD10))),11] <- TRUE
colnames(cvd_diag_hunt)[11] <- 'coronary_h'
# Add information on history of myocardial infarction (ICD9: 410; ICD10: I21, I22)
cvd_diag_hunt[(!is.na(cvd_diag_hunt$ICD9) & grepl('410', cvd_diag_hunt$ICD9)) | (!is.na(cvd_diag_hunt$ICD10) & (grepl('21', cvd_diag_hunt$ICD10) | grepl('22', cvd_diag_hunt$ICD10))),12] <- TRUE
colnames(cvd_diag_hunt)[12] <- 'infarct_h'
# Add information on history of angina (ICD9: 411, 413; ICD10: I20)
cvd_diag_hunt[(!is.na(cvd_diag_hunt$ICD9) & (grepl('411', cvd_diag_hunt$ICD9) | grepl('413', cvd_diag_hunt$ICD9))) | (!is.na(cvd_diag_hunt$ICD10) & grepl('20', cvd_diag_hunt$ICD10)),13] <- TRUE
colnames(cvd_diag_hunt)[13] <- 'angina_h'

# If experienced stroke, add date of first experienced stroke
tmp <- lapply(split(cvd_diag_hunt[!is.na(cvd_diag_hunt$stroke_h) & cvd_diag_hunt$stroke_h == TRUE & cvd_diag_hunt$PID_110334 %in% pop_dat$PID110334,],
                    cvd_diag_hunt$PID_110334[!is.na(cvd_diag_hunt$stroke) & cvd_diag_hunt$stroke_h == TRUE & cvd_diag_hunt$PID_110334 %in% pop_dat$PID110334]),
              function(x) as.character(min(x$death, x$diagnosis, x$ward, x$polyclinic, na.rm = T)))
pop_dat[names(tmp), 30] <- unlist(tmp)
colnames(pop_dat)[30] <- 'fd_stroke'
pop_dat$fd_stroke <- as.Date(pop_dat$fd_stroke)

# If experienced coronary heart disaese, add date of first experienced coronary heart disase
tmp <- lapply(split(cvd_diag_hunt[!is.na(cvd_diag_hunt$coronary_h) & cvd_diag_hunt$coronary_h == TRUE & cvd_diag_hunt$PID_110334 %in% pop_dat$PID110334,],
                    cvd_diag_hunt$PID_110334[!is.na(cvd_diag_hunt$coronary) & cvd_diag_hunt$coronary_h == TRUE & cvd_diag_hunt$PID_110334 %in% pop_dat$PID110334]),
              function(x) as.character(min(x$death, x$diagnosis, x$ward, x$polyclinic, na.rm = T)))
pop_dat[names(tmp), 31] <- unlist(tmp)
colnames(pop_dat)[31] <- 'fd_coronary'
pop_dat$fd_coronary <- as.Date(pop_dat$fd_coronary)

# If experienced myocardial infarction, add date of first experienced myocardial infarction
tmp <- lapply(split(cvd_diag_hunt[!is.na(cvd_diag_hunt$infarct_h) & cvd_diag_hunt$infarct_h == TRUE & cvd_diag_hunt$PID_110334 %in% pop_dat$PID110334,],
                    cvd_diag_hunt$PID_110334[!is.na(cvd_diag_hunt$infarct) & cvd_diag_hunt$infarct_h == TRUE & cvd_diag_hunt$PID_110334 %in% pop_dat$PID110334 == TRUE]),
              function(x) as.character(min(x$death, x$diagnosis, x$ward, x$polyclinic, na.rm = T)))
pop_dat[names(tmp), 32] <- unlist(tmp)
colnames(pop_dat)[32] <- 'fd_infarct'
pop_dat$fd_infarct <- as.Date(pop_dat$fd_infarct)

# If experienced angina, add date of first experienced angina
tmp <- lapply(split(cvd_diag_hunt[!is.na(cvd_diag_hunt$angina_h) & cvd_diag_hunt$angina_h == TRUE & cvd_diag_hunt$PID_110334 %in% pop_dat$PID110334,],
                    cvd_diag_hunt$PID_110334[!is.na(cvd_diag_hunt$angina) & cvd_diag_hunt$angina_h == TRUE & cvd_diag_hunt$PID_110334 %in% pop_dat$PID110334]),
              function(x) as.character(min(x$death, x$diagnosis, x$ward, x$polyclinic, na.rm = T)))
pop_dat[names(tmp), 33] <- unlist(tmp)
colnames(pop_dat)[33] <- 'fd_angina'
pop_dat$fd_angina <- as.Date(pop_dat$fd_angina)

## Add end of observation date
death_hunt <- read.table('HNT_Data_HUNT_Adults_updated052021.txt', sep = ",", header = T, colClasses = 'character')
# Recode IDs (PID_110334) to start with "id_" followed by the unique ID
death_hunt$PID.110334 <- paste("id",death_hunt$PID.110334,sep="_")
# Recode names of rows to the unique IDs
rownames(death_hunt) <- death_hunt$PID.110334
# Convert dates in character representations to objects of class "Date"
death_hunt$RegisStatDat <- ymd(str_sub(as.character(as.POSIXct(as.numeric(death_hunt$RegisStatDat), origin = "1582-10-14")),1,10))
death_hunt$ObsEndDat <- ymd(str_sub(as.character(as.POSIXct(as.numeric(death_hunt$ObsEndDat), origin = "1582-10-14")),1,10))
# Remove participants not in study population
death_hunt <- death_hunt[death_hunt$PID.110334 %in% pop_dat$PID110334,]

# Add end of observation date to each participant
pop_dat[death_hunt$PID.110334,34] <- death_hunt$ObsEndDat
colnames(pop_dat)[34] <- 'end_obs'

# Add end of follow-up date for study
pop_dat$end_all <- rep(as.character(with(cvd_diag_hunt, max(c(diagnosis, ward, polyclinic), na.rm = T))), nrow(pop_dat))
pop_dat$end_all <- as.Date(pop_dat$end_all, format = '%Y-%m-%d')

# Exclude participants with a history of stroke, coronary heart disease, myocardial infarction or angina at start of follow-up
pop_dat <- pop_dat[((is.na(pop_dat$angina_q) | pop_dat$angina_q == FALSE) & ((!is.na(pop_dat$fd_angina) & pop_dat$survey_date < pop_dat$fd_angina) | is.na(pop_dat$fd_angina))) &
                     ((is.na(pop_dat$infarct_q) | pop_dat$infarct_q == FALSE) & ((!is.na(pop_dat$survey_date) & pop_dat$survey_date < pop_dat$fd_infarct) | is.na(pop_dat$fd_infarct))) &
                     ((is.na(pop_dat$stroke_q) | pop_dat$stroke_q == FALSE) & ((!is.na(pop_dat$fd_stroke) & pop_dat$survey_date < pop_dat$fd_stroke) | is.na(pop_dat$fd_stroke)) &
                        ((!is.na(pop_dat$fd_coronary) & pop_dat$survey_date < pop_dat$fd_coronary) | is.na(pop_dat$fd_coronary))),]

# Exclude participants with end of observation date prior to participation in HUNT
pop_dat <- pop_dat[which(!(pop_dat$end_obs < pop_dat$survey_date) | is.na(pop_dat$end_obs)),]

# Save working file
write.table(pop_dat, file = "pop_dat_211209.txt")
