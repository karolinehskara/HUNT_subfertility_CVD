# Packages #####
library(dplyr)
library(EValue)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)
library(mice)
library(miceadds)
library(patchwork)
library(ranger)
library(survival)
library(survminer)

# Load required libraries #####

# Recode variables
pop_dat$PID110334 <- as.character(pop_dat$PID110334)
pop_dat$sex <- as.factor(pop_dat$sex)
pop_dat$birthyear <- as.numeric(pop_dat$birthyear)
pop_dat$survey <- as.factor(pop_dat$survey)
pop_dat$fert <- as.factor(pop_dat$fert)
pop_dat$survey_date <- as.Date(pop_dat$survey_date)
pop_dat$survey_age <- as.double(pop_dat$survey_age)
pop_dat$partner <- as.character(pop_dat$partner)
pop_dat$fd_stroke <- as.Date(pop_dat$fd_stroke)
pop_dat$fd_coronary <- as.Date(pop_dat$fd_coronary)
pop_dat$fd_infarct <- as.Date(pop_dat$fd_infarct)
pop_dat$fd_angina <- as.Date(pop_dat$fd_angina)
pop_dat$end_obs <- as.Date(pop_dat$end_obs)
pop_dat$end_all <- as.Date(pop_dat$end_all)
pop_dat$fert_age <- as.numeric(pop_dat$fert_age)
pop_dat$coh_sps <- as.factor(pop_dat$coh_sps)
pop_dat$coh_sps <- relevel(pop_dat$coh_sps, 'yes')
pop_dat$bmi <- as.double(pop_dat$bmi)
pop_dat$smok_cat <- as.factor(pop_dat$smok_cat)
pop_dat$smok_cat <- relevel(pop_dat$smok_cat, 'former smoker,py<=20')
pop_dat$smok_cat <- relevel(pop_dat$smok_cat, 'smoker,py20<')
pop_dat$smok_cat <- relevel(pop_dat$smok_cat, 'smoker,py<=20')
pop_dat$smok_cat <- relevel(pop_dat$smok_cat, 'non-smoker')
pop_dat$educ <- as.factor(pop_dat$educ)
pop_dat$educ <- relevel(pop_dat$educ, 'upper secondary school')
pop_dat$educ <- relevel(pop_dat$educ, 'higher education')
pop_dat$serum_chol <- as.numeric(pop_dat$serum_chol)
pop_dat$hdl_chol <- as.numeric(pop_dat$hdl_chol)
pop_dat$bp_dias <- as.numeric(pop_dat$bp_dias)
pop_dat$bp_syst <- as.numeric(pop_dat$bp_syst)
pop_dat$bp_med <- as.factor(pop_dat$bp_med)
pop_dat$early_birth <- as.factor(pop_dat$early_birth)
pop_dat$preekl <- as.factor(pop_dat$preekl)
pop_dat$diabetes <- as.factor(pop_dat$diabetes)
pop_dat$fert_child <- as.factor(pop_dat$fert_child)
pop_dat$fert_child <- relevel(pop_dat$fert_child, 'subfertile')
pop_dat$fert_child <- relevel(pop_dat$fert_child, 'fertile')
pop_dat$strat_fert_age <- as.factor(pop_dat$strat_fert_age)
pop_dat$strat_age <- as.factor(pop_dat$strat_age)
pop_dat$strat_age <- relevel(pop_dat$strat_age, 'low')
pop_dat$couple <- as.factor(pop_dat$couple)

pop_dat$survey_ageq <- pop_dat$survey_age^2

## End of follow-up
pop_dat$end_followup_strok <- ifelse(!is.na(pop_dat$fd_stroke), as.character(pop_dat$fd_stroke),
                                     ifelse(!is.na(pop_dat$end_obs),
                                            ifelse(pop_dat$end_obs < pop_dat$end_all,
                                                   as.character(pop_dat$end_obs),
                                                   as.character(pop_dat$end_all)),
                                            as.character(pop_dat$end_all)))
pop_dat$end_followup_chd <- ifelse(!is.na(pop_dat$fd_coronary), as.character(pop_dat$fd_coronary),
                                   ifelse(!is.na(pop_dat$end_obs),
                                          ifelse(pop_dat$end_obs < pop_dat$end_all,
                                                 as.character(pop_dat$end_obs),
                                                 as.character(pop_dat$end_all)),
                                          as.character(pop_dat$end_all)))
pop_dat$end_followup_myinf <- ifelse(!is.na(pop_dat$fd_infarct), as.character(pop_dat$fd_infarct),
                                     ifelse(!is.na(pop_dat$end_obs),
                                            ifelse(pop_dat$end_obs < pop_dat$end_all,
                                                   as.character(pop_dat$end_obs),
                                                   as.character(pop_dat$end_all)),
                                            as.character(pop_dat$end_all)))
pop_dat$end_followup_ang <- ifelse(!is.na(pop_dat$fd_angina), as.character(pop_dat$fd_angina),
                                   ifelse(!is.na(pop_dat$end_obs),
                                          ifelse(pop_dat$end_obs < pop_dat$end_all,
                                                 as.character(pop_dat$end_obs),
                                                 as.character(pop_dat$end_all)),
                                          as.character(pop_dat$end_all)))
pop_dat$end_followup_tot <- ifelse(!is.na(pop_dat$fd_stroke) | !is.na(pop_dat$fd_coronary) | !is.na(pop_dat$fd_infarct) | !is.na(pop_dat$fd_angina),
                                   as.character(min(c(pop_dat$fd_stroke, pop_dat$fd_coronary, pop_dat$fd_infarct, pop_dat$fd_angina), na.rm = TRUE)),
                                   ifelse(!is.na(pop_dat$end_obs),
                                          ifelse(pop_dat$end_obs < pop_dat$end_all,
                                                 as.character(pop_dat$end_obs),
                                                 as.character(pop_dat$end_all)),
                                          as.character(pop_dat$end_all)))

pop_dat$end_followup_strok <- as.Date(pop_dat$end_followup_strok)
pop_dat$end_followup_chd <- as.Date(pop_dat$end_followup_chd)
pop_dat$end_followup_myinf <- as.Date(pop_dat$end_followup_myinf)
pop_dat$end_followup_ang <- as.Date(pop_dat$end_followup_ang)
pop_dat$end_followup_tot <- as.Date(pop_dat$end_followup_tot)

## Duration
pop_dat$os_yrs_strok <- as.numeric(difftime(pop_dat$end_followup_strok, pop_dat$survey_date, unit = 'days')) / 365.25
pop_dat$os_yrs_chd <- as.numeric(difftime(pop_dat$end_followup_chd, pop_dat$survey_date, unit = 'days')) / 365.25
pop_dat$os_yrs_myinf <- as.numeric(difftime(pop_dat$end_followup_myinf, pop_dat$survey_date, unit = 'days')) / 365.25
pop_dat$os_yrs_ang <- as.numeric(difftime(pop_dat$end_followup_ang, pop_dat$survey_date, unit = 'days')) / 365.25
pop_dat$os_yrs_tot <- as.numeric(difftime(pop_dat$end_followup_tot, pop_dat$survey_date, unit = 'days')) / 365.25

# Update end of follow-up
pop_dat$end_followup_strok[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_strok)) > 100.0] <- pop_dat$survey_date[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_strok)) > 100.0] -
  dyears(pop_dat$survey_age[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_strok)) > 100.0]) + dyears(100)
pop_dat$end_followup_chd[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_chd)) > 100.0] <- pop_dat$survey_date[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_chd)) > 100.0] -
  dyears(pop_dat$survey_age[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_chd)) > 100.0]) + dyears(100)
pop_dat$end_followup_myinf[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_myinf)) > 100.0] <- pop_dat$survey_date[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_myinf)) > 100.0] -
  dyears(pop_dat$survey_age[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_myinf)) > 100.0]) + dyears(100)
pop_dat$end_followup_ang[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_ang)) > 100.0] <- pop_dat$survey_date[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_ang)) > 100.0] -
  dyears(pop_dat$survey_age[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_ang)) > 100.0]) + dyears(100)
pop_dat$end_followup_tot[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_tot)) > 100.0] <- pop_dat$survey_date[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_tot)) > 100.0] -
  dyears(pop_dat$survey_age[as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_tot)) > 100.0]) + dyears(100)

# Update end of follow-up
pop_dat$os_yrs_strok <- as.numeric(difftime(pop_dat$end_followup_strok, pop_dat$survey_date, unit = 'days')) / 365.25
pop_dat$os_yrs_chd <- as.numeric(difftime(pop_dat$end_followup_chd, pop_dat$survey_date, unit = 'days')) / 365.25
pop_dat$os_yrs_myinf <- as.numeric(difftime(pop_dat$end_followup_myinf, pop_dat$survey_date, unit = 'days')) / 365.25
pop_dat$os_yrs_ang <- as.numeric(difftime(pop_dat$end_followup_ang, pop_dat$survey_date, unit = 'days')) / 365.25
pop_dat$os_yrs_tot <- as.numeric(difftime(pop_dat$end_followup_tot, pop_dat$survey_date, unit = 'days')) / 365.25

# Add status
pop_dat$status_strok <- ifelse(!is.na(pop_dat$fd_stroke) & as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_strok)) < 100.0,1,0)
pop_dat$status_chd <- ifelse(!is.na(pop_dat$fd_coronary) & as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_chd)) < 100.0,1,0)
pop_dat$status_myinf <- ifelse(!is.na(pop_dat$fd_infarct) & as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_myinf)) < 100.0,1,0)
pop_dat$status_ang <- ifelse(!is.na(pop_dat$fd_angina) & as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_ang)) < 100.0,1,0)
pop_dat$status_tot <- ifelse((!is.na(pop_dat$fd_stroke) | !is.na(pop_dat$fd_coronary) | !is.na(pop_dat$fd_infarct) | !is.na(pop_dat$fd_angina)) &
                               as.numeric(sprintf('%.1f', pop_dat$survey_age + pop_dat$os_yrs_tot)) < 100.0,1,0)

# Characteristics #####
cov_tab <- rbind(
  ## Count
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']),
        NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']),
        NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']),
        NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']),
        NA, NA),
  ## Age at participation, mean (SD)
  cbind(mean(pop_dat$survey_age[pop_dat$sex == 'F' & pop_dat$fert == 'no']),
        sd(pop_dat$survey_age[pop_dat$sex == 'F' & pop_dat$fert == 'no']),
        NA,
        mean(pop_dat$survey_age[pop_dat$sex == 'F' & pop_dat$fert == 'yes']),
        sd(pop_dat$survey_age[pop_dat$sex == 'F' & pop_dat$fert == 'yes']),
        NA,
        mean(pop_dat$survey_age[pop_dat$sex == 'M' & pop_dat$fert == 'no']),
        sd(pop_dat$survey_age[pop_dat$sex == 'M' & pop_dat$fert == 'no']),
        NA,
        mean(pop_dat$survey_age[pop_dat$sex == 'M' & pop_dat$fert == 'yes']),
        sd(pop_dat$survey_age[pop_dat$sex == 'M' & pop_dat$fert == 'yes']),
        NA),
  ## Age at participation, N (%)
  # 19-29
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age < 30]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age < 30]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age < 30]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age < 30]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age < 30]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age < 30]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age < 30]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age < 30]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # 30-39
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age >= 30 & pop_dat$survey_age < 40]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age >= 30 & pop_dat$survey_age < 40]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 30 & pop_dat$survey_age < 40]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 30 & pop_dat$survey_age < 40]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age >= 30 & pop_dat$survey_age < 40]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age >= 30 & pop_dat$survey_age < 40]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 30 & pop_dat$survey_age < 40]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 30 & pop_dat$survey_age < 40]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # 40-49
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age >= 40 & pop_dat$survey_age < 50]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age >= 40 & pop_dat$survey_age < 50]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 40 & pop_dat$survey_age < 50]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 40 & pop_dat$survey_age < 50]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age >= 40 & pop_dat$survey_age < 50]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age >= 40 & pop_dat$survey_age < 50]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 40 & pop_dat$survey_age < 50]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 40 & pop_dat$survey_age < 50]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # 50-59
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age >= 50 & pop_dat$survey_age < 60]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age >= 50 & pop_dat$survey_age < 60]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 50 & pop_dat$survey_age < 60]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 50 & pop_dat$survey_age < 60]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age >= 50 & pop_dat$survey_age < 60]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age >= 50 & pop_dat$survey_age < 60]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 50 & pop_dat$survey_age < 60]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 50 & pop_dat$survey_age < 60]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # 60-69
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age >= 60 & pop_dat$survey_age < 70]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age >= 60 & pop_dat$survey_age < 70]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 60 & pop_dat$survey_age < 70]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 60 & pop_dat$survey_age < 70]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age >= 60 & pop_dat$survey_age < 70]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age >= 60 & pop_dat$survey_age < 70]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 60 & pop_dat$survey_age < 70]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 60 & pop_dat$survey_age < 70]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # 69<
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age >= 70]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & pop_dat$survey_age >= 70]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 70]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 70]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age >= 70]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & pop_dat$survey_age >= 70]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 70]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & pop_dat$survey_age >= 70]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  ## BMI, median (Q1, Q3)
  cbind(median(pop_dat$bmi[pop_dat$sex == 'F' & pop_dat$fert == 'no'], na.rm = TRUE),
        quantile(pop_dat$bmi[pop_dat$sex == 'F' & pop_dat$fert == 'no'], 0.25, na.rm = TRUE),
        quantile(pop_dat$bmi[pop_dat$sex == 'F' & pop_dat$fert == 'no'], 0.75, na.rm = TRUE),
        median(pop_dat$bmi[pop_dat$sex == 'F' & pop_dat$fert == 'yes'], na.rm = TRUE),
        quantile(pop_dat$bmi[pop_dat$sex == 'F' & pop_dat$fert == 'yes'], 0.25, na.rm = TRUE),
        quantile(pop_dat$bmi[pop_dat$sex == 'F' & pop_dat$fert == 'yes'], 0.75, na.rm = TRUE),
        median(pop_dat$bmi[pop_dat$sex == 'M' & pop_dat$fert == 'no'], na.rm = TRUE),
        quantile(pop_dat$bmi[pop_dat$sex == 'M' & pop_dat$fert == 'no'], 0.25, na.rm = TRUE),
        quantile(pop_dat$bmi[pop_dat$sex == 'M' & pop_dat$fert == 'no'], 0.75, na.rm = TRUE),
        median(pop_dat$bmi[pop_dat$sex == 'M' & pop_dat$fert == 'yes'], na.rm = TRUE),
        quantile(pop_dat$bmi[pop_dat$sex == 'M' & pop_dat$fert == 'yes'], 0.25, na.rm = TRUE),
        quantile(pop_dat$bmi[pop_dat$sex == 'M' & pop_dat$fert == 'yes'], 0.75, na.rm = TRUE)),
  # Missing
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$bmi)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$bmi)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$bmi)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$bmi)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$bmi)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$bmi)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$bmi)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$bmi)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  ## Blood pressure, mean (SD)
  # Systolic
  cbind(mean(pop_dat$bp_syst[pop_dat$sex == 'F' & pop_dat$fert == 'no'], na.rm = TRUE),
        sd(pop_dat$bp_syst[pop_dat$sex == 'F' & pop_dat$fert == 'no'], na.rm = TRUE),
        NA,
        mean(pop_dat$bp_syst[pop_dat$sex == 'F' & pop_dat$fert == 'yes'], na.rm = TRUE),
        sd(pop_dat$bp_syst[pop_dat$sex == 'F' & pop_dat$fert == 'yes'], na.rm = TRUE),
        NA,
        mean(pop_dat$bp_syst[pop_dat$sex == 'M' & pop_dat$fert == 'no'], na.rm = TRUE),
        sd(pop_dat$bp_syst[pop_dat$sex == 'M' & pop_dat$fert == 'no'], na.rm = TRUE),
        NA,
        mean(pop_dat$bp_syst[pop_dat$sex == 'M' & pop_dat$fert == 'yes'], na.rm = TRUE),
        sd(pop_dat$bp_syst[pop_dat$sex == 'M' & pop_dat$fert == 'yes'], na.rm = TRUE),
        NA),
  # Diastolic
  cbind(mean(pop_dat$bp_dias[pop_dat$sex == 'F' & pop_dat$fert == 'no'], na.rm = TRUE),
        sd(pop_dat$bp_dias[pop_dat$sex == 'F' & pop_dat$fert == 'no'], na.rm = TRUE),
        NA,
        mean(pop_dat$bp_dias[pop_dat$sex == 'F' & pop_dat$fert == 'yes'], na.rm = TRUE),
        sd(pop_dat$bp_dias[pop_dat$sex == 'F' & pop_dat$fert == 'yes'], na.rm = TRUE),
        NA,
        mean(pop_dat$bp_dias[pop_dat$sex == 'M' & pop_dat$fert == 'no'], na.rm = TRUE),
        sd(pop_dat$bp_dias[pop_dat$sex == 'M' & pop_dat$fert == 'no'], na.rm = TRUE),
        NA,
        mean(pop_dat$bp_dias[pop_dat$sex == 'M' & pop_dat$fert == 'yes'], na.rm = TRUE),
        sd(pop_dat$bp_dias[pop_dat$sex == 'M' & pop_dat$fert == 'yes'], na.rm = TRUE),
        NA),
  # Missing
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & (is.na(pop_dat$bp_syst) | is.na(pop_dat$bp_dias))]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & (is.na(pop_dat$bp_syst) | is.na(pop_dat$bp_dias))]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & (is.na(pop_dat$bp_syst) | is.na(pop_dat$bp_dias))]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & (is.na(pop_dat$bp_syst) | is.na(pop_dat$bp_dias))]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & (is.na(pop_dat$bp_syst) | is.na(pop_dat$bp_dias))]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & (is.na(pop_dat$bp_syst) | is.na(pop_dat$bp_dias))]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & (is.na(pop_dat$bp_syst) | is.na(pop_dat$bp_dias))]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & (is.na(pop_dat$bp_syst) | is.na(pop_dat$bp_dias))]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  ## Blood pressure medication
  # No
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Yes
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$bp_med) & pop_dat$bp_med == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Missing
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$bp_med)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$bp_med)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$bp_med)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$bp_med)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$bp_med)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$bp_med)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$bp_med)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$bp_med)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  ## Diabetes mellitus, N (%)
  # No
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Yes
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$diabetes) & pop_dat$diabetes == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Missing
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$diabetes)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$diabetes)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$diabetes)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$diabetes)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$diabetes)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$diabetes)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$diabetes)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$diabetes)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  ## Serum cholesterol, mean (SD)
  cbind(mean(pop_dat$serum_chol[pop_dat$sex == 'F' & pop_dat$fert == 'no'], na.rm = TRUE),
        sd(pop_dat$serum_chol[pop_dat$sex == 'F' & pop_dat$fert == 'no'], na.rm = TRUE),
        NA,
        mean(pop_dat$serum_chol[pop_dat$sex == 'F' & pop_dat$fert == 'yes'], na.rm = TRUE),
        sd(pop_dat$serum_chol[pop_dat$sex == 'F' & pop_dat$fert == 'yes'], na.rm = TRUE),
        NA,
        mean(pop_dat$serum_chol[pop_dat$sex == 'M' & pop_dat$fert == 'no'], na.rm = TRUE),
        sd(pop_dat$serum_chol[pop_dat$sex == 'M' & pop_dat$fert == 'no'], na.rm = TRUE),
        NA,
        mean(pop_dat$serum_chol[pop_dat$sex == 'M' & pop_dat$fert == 'yes'], na.rm = TRUE),
        sd(pop_dat$serum_chol[pop_dat$sex == 'M' & pop_dat$fert == 'yes'], na.rm = TRUE),
        NA),
  # Missing, N (%)
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$serum_chol)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$serum_chol)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$serum_chol)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$serum_chol)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$serum_chol)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$serum_chol)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$serum_chol)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$serum_chol)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  ## Smoking status and pack year (PY), N (%)
  # Non-smoker
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'non-smoker']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'non-smoker']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'non-smoker']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'non-smoker']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'non-smoker']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'non-smoker']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'non-smoker']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'non-smoker']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Former smoker, PY 0-20
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py<=20']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py<=20']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py<=20']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py<=20']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py<=20']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py<=20']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py<=20']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py<=20']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Former smoker, PY 20<
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py20<']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py20<']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py20<']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py20<']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py20<']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py20<']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py20<']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'former smoker,py20<']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Smoker, PY 0-20
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py<=20']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py<=20']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py<=20']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py<=20']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py<=20']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py<=20']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py<=20']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py<=20']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Smoker, PY 20<
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py20<']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py20<']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py20<']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py20<']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py20<']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py20<']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py20<']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$smok_cat) & pop_dat$smok_cat == 'smoker,py20<']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Missing
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$smok_cat)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$smok_cat)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$smok_cat)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$smok_cat)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$smok_cat)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$smok_cat)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$smok_cat)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$smok_cat)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  ## Living without cohabitant
  # No
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'no']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'no']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Yes
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'yes']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$coh_sps) & pop_dat$coh_sps == 'yes']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Missing
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$coh_sps)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$coh_sps)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$coh_sps)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$coh_sps)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$coh_sps)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$coh_sps)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$coh_sps)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$coh_sps)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  ## Education, N (%)
  # Higher education
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'higher education']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'higher education']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'higher education']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'higher education']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'higher education']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'higher education']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'higher education']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'higher education']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Upper secondary school
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'upper secondary school']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'upper secondary school']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'upper secondary school']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'upper secondary school']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'upper secondary school']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'upper secondary school']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'upper secondary school']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'upper secondary school']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Secondary school
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'secondary school']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'secondary school']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'secondary school']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'secondary school']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'secondary school']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & !is.na(pop_dat$educ) & pop_dat$educ == 'secondary school']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'secondary school']),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & !is.na(pop_dat$educ) & pop_dat$educ == 'secondary school']) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  # Missing
  cbind(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$educ)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no' & is.na(pop_dat$educ)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$educ)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$educ)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$educ)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no' & is.na(pop_dat$educ)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'no']))*100,
        NA,
        length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$educ)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes' & is.na(pop_dat$educ)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$fert == 'yes']))*100,
        NA),
  ## Age at first experienced subfertility, N (%)
  # 19-29
  cbind(NA, NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$fert_age) & pop_dat$fert_age < 30]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$fert_age) & pop_dat$fert_age < 30]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        NA, NA, NA,
        NA, NA, NA),
  # 30-39
  cbind(NA, NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$fert_age) & pop_dat$fert_age >= 30 & pop_dat$fert_age < 40]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$fert_age) & pop_dat$fert_age >= 30 & pop_dat$fert_age < 40]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        NA, NA, NA,
        NA, NA, NA),
  # 39<
  cbind(NA, NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$fert_age) & pop_dat$fert_age >= 40]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$fert_age) & pop_dat$fert_age >= 40]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        NA, NA, NA,
        NA, NA, NA),
  # Missing
  cbind(NA, NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$fert_age)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$fert_age)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        NA, NA, NA,
        NA, NA, NA),
  ## Early birth
  # No
  cbind(NA, NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$early_birth) & pop_dat$early_birth == '0_normal']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$early_birth) & pop_dat$early_birth == '0_normal']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        NA, NA, NA,
        NA, NA, NA),
  # Yes
  cbind(NA, NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$early_birth) & pop_dat$early_birth == '1_early']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes'& !is.na(pop_dat$early_birth) & pop_dat$early_birth== '1_early']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        NA, NA, NA,
        NA, NA, NA),
  # Missing
  cbind(NA, NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$early_birth)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$early_birth)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        NA, NA, NA,
        NA, NA, NA),
  #Preeclampsia
  # No
  cbind(NA, NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$preekl) & pop_dat$preekl == '0_normal']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$preekl) & pop_dat$preekl == '0_normal']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        NA, NA, NA,
        NA, NA, NA),
  # Yes
  cbind(NA, NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$preekl) & pop_dat$preekl == '1_preekl']),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & !is.na(pop_dat$preekl) & pop_dat$preekl== '1_preekl']) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        NA, NA, NA,
        NA, NA, NA),
  # Missing
  cbind(NA, NA, NA,
        length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$preekl)]),
        (length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes' & is.na(pop_dat$preekl)]) /
           length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$fert == 'yes']))*100,
        NA,
        NA, NA, NA,
        NA, NA, NA)
)

cov_tab

setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data/Export this")
write.table(cov_tab, 'cov_tab.txt', col.names = FALSE, row.names = FALSE)
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")

## Rates of incident events
# Stroke
(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$status_strok == 1])/sum(pop_dat$os_yrs_strok[pop_dat$sex == 'F']))*10000
(length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$status_strok == 1])/sum(pop_dat$os_yrs_strok[pop_dat$sex == 'M']))*10000
# Coronary heart disease
(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$status_chd == 1])/sum(pop_dat$os_yrs_chd[pop_dat$sex == 'F']))*10000
(length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$status_chd == 1])/sum(pop_dat$os_yrs_chd[pop_dat$sex == 'M']))*10000
# Myocardial infarction
(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$status_myinf == 1])/sum(pop_dat$os_yrs_myinf[pop_dat$sex == 'F']))*10000
(length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$status_myinf == 1])/sum(pop_dat$os_yrs_myinf[pop_dat$sex == 'M']))*10000
# Angina
(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$status_ang == 1])/sum(pop_dat$os_yrs_ang[pop_dat$sex == 'F']))*10000
(length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$status_ang == 1])/sum(pop_dat$os_yrs_ang[pop_dat$sex == 'M']))*10000
# Any CVD
(length(pop_dat$PID110334[pop_dat$sex == 'F' & pop_dat$status_tot == 1])/sum(pop_dat$os_yrs_tot[pop_dat$sex == 'F']))*10000
(length(pop_dat$PID110334[pop_dat$sex == 'M' & pop_dat$status_tot == 1])/sum(pop_dat$os_yrs_tot[pop_dat$sex == 'M']))*10000


# Create imputed data set #####
# Nelson Aalen for stroke and coronary heart disease
pop_dat[,48] <- nelsonaalen(pop_dat, timevar = os_yrs_strok, statusvar = status_strok)
colnames(pop_dat)[48] <- 'H0tilde_strok'
pop_dat[,49] <- nelsonaalen(pop_dat, timevar = os_yrs_chd, statusvar = status_chd)
colnames(pop_dat)[49] <- 'H0tilde_chd'

# Update predictor matrix and methods
imp_dat <- mice(pop_dat, maxit=0)
predimp <- imp_dat$predictorMatrix
predimp[,c('PID110334','birthyear', 'fert','survey_date','partner','fd_stroke','fd_coronary','fd_infarct','fd_angina','end_obs','end_all','fert_age',
           'early_birth','preekl','strat_fert_age','strat_age','couple','survey_ageq','status_myinf','status_ang','os_yrs_strok','os_yrs_chd','os_yrs_myinf',
           'os_yrs_ang','end_followup_strok','end_followup_myinf','end_followup_ang')] <- 0
predimp[c('PID110334','sex','birthyear','survey','fert','survey_date','survey_age','partner','fd_stroke','fd_coronary','fd_infarct','fd_angina',
          'end_obs','end_all','fert_age','early_birth','preekl','fert_child','strat_fert_age','strat_age','couple','survey_ageq',
          'end_followup_strok','end_followup_chd','end_followup_myinf','end_followup_ang','os_yrs_strok','os_yrs_chd','os_yrs_myinf','os_yrs_ang',
          'status_strok','status_chd','status_myinf','status_ang','H0tilde_strok','H0tilde_chd'),] <- 0

methimp <- imp_dat$method
methimp[[13]] <- ''
methimp[[15]] <- ''
methimp[[18]] <- 'polr'
methimp[[19]] <- 'polr'
methimp[[25]] <- ''
methimp[[26]] <- ''
methimp[[29]] <- ''

# Run imputation
imp <- mice(pop_dat, m=20, maxit=15, seed=500, predictorMatrix = predimp, method = methimp)

# Save imputed data set
impdir <- "./imp"
write.mice.imputation(imp, impdir, include.varnames = TRUE, long = TRUE, mids2spss = FALSE)

# Load imputet data set #####
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")
load("imp/imp.Rdata")
imp <- mi.res
rm(mi.res)

# Cox proportional hazards models #####
## Imputed data
# Stroke
strok_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
strok_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
strok_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq, subset = (sex == 'M')))), conf.int = TRUE, exponentiate = TRUE)
strok_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M')))), conf.int = TRUE, exponentiate = TRUE)
# Coronary heart disease
chd_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
chd_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
chd_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq, subset = (sex == 'M')))), conf.int = TRUE, exponentiate = TRUE)
chd_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M')))), conf.int = TRUE, exponentiate = TRUE)
# Myocardial infarction
myinf_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
myinf_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
myinf_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq, subset = (sex == 'M')))), conf.int = TRUE, exponentiate = TRUE)
myinf_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M')))), conf.int = TRUE, exponentiate = TRUE)
# Angina
ang_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
ang_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
ang_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq, subset = (sex == 'M')))), conf.int = TRUE, exponentiate = TRUE)
ang_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M')))), conf.int = TRUE, exponentiate = TRUE)
# Any CVD
tot_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
tot_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
tot_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq, subset = (sex == 'M')))), conf.int = TRUE, exponentiate = TRUE)
tot_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M')))), conf.int = TRUE, exponentiate = TRUE)

## Complete case data
# Stroke
strok_compl_f1 <- coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq, data = pop_dat[pop_dat$sex == 'F' & !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) & !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) & !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),])
strok_compl_f2 <- coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, data = pop_dat[pop_dat$sex == 'F',])
strok_compl_m1 <- coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq, data = pop_dat[pop_dat$sex == 'M' & !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) & !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) & !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),])
strok_compl_m2 <- coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, data = pop_dat[pop_dat$sex == 'M',])
# Coronary heart disease
chd_compl_f1 <- coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq, data = pop_dat[pop_dat$sex == 'F' & !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) & !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) & !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),])
chd_compl_f2 <- coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, data = pop_dat[pop_dat$sex == 'F',])
chd_compl_m1 <- coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq, data = pop_dat[pop_dat$sex == 'M' & !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) & !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) & !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),])
chd_compl_m2 <- coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, data = pop_dat[pop_dat$sex == 'M',])
# Myocardial infarction
myinf_compl_f1 <- coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq, data = pop_dat[pop_dat$sex == 'F' & !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) & !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) & !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),])
myinf_compl_f2 <- coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, data = pop_dat[pop_dat$sex == 'F',])
myinf_compl_m1 <- coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq, data = pop_dat[pop_dat$sex == 'M' & !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) & !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) & !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),])
myinf_compl_m2 <- coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, data = pop_dat[pop_dat$sex == 'M',])
# Angina
ang_compl_f1 <- coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq, data = pop_dat[pop_dat$sex == 'F' & !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) & !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) & !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),])
ang_compl_f2 <- coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, data = pop_dat[pop_dat$sex == 'F',])
ang_compl_m1 <- coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq, data = pop_dat[pop_dat$sex == 'M' & !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) & !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) & !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),])
ang_compl_m2 <- coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, data = pop_dat[pop_dat$sex == 'M',])
# Any CVD
tot_compl_f1 <- coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq, data = pop_dat[pop_dat$sex == 'F' & !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) & !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) & !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),])
tot_compl_f2 <- coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, data = pop_dat[pop_dat$sex == 'F',])
tot_compl_m1 <- coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq, data = pop_dat[pop_dat$sex == 'M' & !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) & !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) & !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),])
tot_compl_m2 <- coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, data = pop_dat[pop_dat$sex == 'M',])

## Subfertility and infertility
# Stroke
strok_subf_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert_child + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
strok_subf_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert_child + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Coronary heart disease
chd_subf_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert_child + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
chd_subf_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert_child + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Myocardial infarction
myinf_subf_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert_child + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
myinf_subf_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert_child + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Angina
ang_subf_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert_child + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
ang_subf_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert_child + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Any CVD
tot_subf_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert_child + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
tot_subf_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert_child + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)

## Age stratified, women
# Stroke
strok_strat_low_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
strok_strat_low_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
strok_strat_high_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
strok_strat_high_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
# Coronary heart disease
chd_strat_low_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
chd_strat_low_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
chd_strat_high_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
chd_strat_high_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
# Myocardial infarction
myinf_strat_low_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
myinf_strat_low_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
myinf_strat_high_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
myinf_strat_high_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
# Angina
ang_strat_low_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
ang_strat_low_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
ang_strat_high_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
ang_strat_high_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
# Any CVD
tot_strat_low_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
tot_strat_low_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
tot_strat_high_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
tot_strat_high_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)

## Age stratified, men
# Stroke
strok_strat_low_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq, subset = (sex == 'M' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
strok_strat_low_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
strok_strat_high_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq, subset = (sex == 'M' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
strok_strat_high_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
# Coronary heart disease
chd_strat_low_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq, subset = (sex == 'M' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
chd_strat_low_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
chd_strat_high_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq, subset = (sex == 'M' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
chd_strat_high_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
# Myocardial infarction
myinf_strat_low_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq, subset = (sex == 'M' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
myinf_strat_low_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
myinf_strat_high_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq, subset = (sex == 'M' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
myinf_strat_high_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
# Angina
ang_strat_low_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq, subset = (sex == 'M' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
ang_strat_low_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
ang_strat_high_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq, subset = (sex == 'M' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
ang_strat_high_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
# Any CVD
tot_strat_low_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq, subset = (sex == 'M' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
tot_strat_low_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
tot_strat_high_m1 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq, subset = (sex == 'M' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)
tot_strat_high_m2 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'M' & strat_age == 'high')))), conf.int = TRUE, exponentiate = TRUE)

## Women with partners
# Stroke
strok_partn_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & !is.na(partner))))), conf.int = TRUE, exponentiate = TRUE)
strok_partn_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & !is.na(partner))))), conf.int = TRUE, exponentiate = TRUE)
# Coronary heart disease
chd_partn_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & !is.na(partner))))), conf.int = TRUE, exponentiate = TRUE)
chd_partn_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & !is.na(partner))))), conf.int = TRUE, exponentiate = TRUE)
# Myocardial infarction
myinf_partn_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & !is.na(partner))))), conf.int = TRUE, exponentiate = TRUE)
myinf_partn_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & !is.na(partner))))), conf.int = TRUE, exponentiate = TRUE)
# Angina
ang_partn_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & !is.na(partner))))), conf.int = TRUE, exponentiate = TRUE)
ang_partn_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & !is.na(partner))))), conf.int = TRUE, exponentiate = TRUE)
# Any CVD
tot_partn_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq, subset = (sex == 'F' & !is.na(partner))))), conf.int = TRUE, exponentiate = TRUE)
tot_partn_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & !is.na(partner))))), conf.int = TRUE, exponentiate = TRUE)

# Stroke
strok_stratfert_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ strat_fert_age + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
strok_stratfert_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ strat_fert_age + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Coronary heart disease
chd_stratfert_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ strat_fert_age + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
chd_stratfert_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ strat_fert_age + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Myocardial infarction
myinf_stratfert_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ strat_fert_age + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
myinf_stratfert_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ strat_fert_age + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Angina
ang_stratfert_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ strat_fert_age + survey_age + survey_ageq, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
ang_stratfert_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ strat_fert_age + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)

# Any CVD
tot_stratfert_f1 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ strat_fert_age + survey_age + survey_ageq, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)
tot_stratfert_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ strat_fert_age + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F' & strat_age == 'low')))), conf.int = TRUE, exponentiate = TRUE)

## Mediator analysis
# Stroke
strok_preg_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
strok_preg_f3 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear + early_birth + preekl, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Coronary heart disease
chd_preg_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
chd_preg_f3 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear + early_birth + preekl, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Myocardial infarction
myinf_preg_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
myinf_preg_f3 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear + early_birth + preekl, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Angina
ang_preg_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
ang_preg_f3 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear + early_birth + preekl, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
# Any CVD
tot_preg_f2 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)
tot_preg_f3 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear + early_birth + preekl, subset = (sex == 'F')))), conf.int = TRUE, exponentiate = TRUE)


# Additional tests #####
## Interaction
# Stroke
strok_inter1 <- summary(pool(with(imp, coxph(Surv(os_yrs_strok, status_strok) ~ fert*sex + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, cluster = couple))), conf.int = TRUE, exponentiate = FALSE)
# Coronary heart disease
chd_inter1 <- summary(pool(with(imp, coxph(Surv(os_yrs_chd, status_chd) ~ fert*sex + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, cluster = couple))), conf.int = TRUE, exponentiate = FALSE)
# Myocardial infarction
myinf_inter1 <- summary(pool(with(imp, coxph(Surv(os_yrs_myinf, status_myinf) ~ fert*sex + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, cluster = couple))), conf.int = TRUE, exponentiate = FALSE)
# Angina
ang_inter1 <- summary(pool(with(imp, coxph(Surv(os_yrs_ang, status_ang) ~ fert*sex + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, cluster = couple))), conf.int = TRUE, exponentiate = FALSE)
# Any CVD
tot_inter1 <- summary(pool(with(imp, coxph(Surv(os_yrs_tot, status_tot) ~ fert*sex + survey_age + survey_ageq + coh_sps + bmi + smok_cat + educ + diabetes + bp_dias + bp_syst + serum_chol + birthyear, cluster = couple))), conf.int = TRUE, exponentiate = FALSE)

## E-values
# Stroke
evalues.HR(est = strok_f2$estimate[1], lo = strok_f2$`2.5 %`[1], hi = strok_f2$`97.5 %`[1], rare = 1)
evalues.HR(est = strok_m2$estimate[1], lo = strok_m2$`2.5 %`[1], hi = strok_m2$`97.5 %`[1], rare = 1)
# Coronary heart disease
evalues.HR(est = chd_f2$estimate[1], lo = chd_f2$`2.5 %`[1], hi = chd_f2$`97.5 %`[1], rare = 1)
evalues.HR(est = chd_m2$estimate[1], lo = chd_m2$`2.5 %`[1], hi = chd_m2$`97.5 %`[1], rare = 1)
# Myocardial infarction
evalues.HR(est = myinf_f2$estimate[1], lo = myinf_f2$`2.5 %`[1], hi = myinf_f2$`97.5 %`[1], rare = 1)
evalues.HR(est = myinf_m2$estimate[1], lo = myinf_m2$`2.5 %`[1], hi = myinf_m2$`97.5 %`[1], rare = 1)
# Angina
evalues.HR(est = ang_f2$estimate[1], lo = ang_f2$`2.5 %`[1], hi = ang_f2$`97.5 %`[1], rare = 1)
evalues.HR(est = ang_m2$estimate[1], lo = ang_m2$`2.5 %`[1], hi = ang_m2$`97.5 %`[1], rare = 1)
# Any CVD
evalues.HR(est = tot_f2$estimate[1], lo = tot_f2$`2.5 %`[1], hi = tot_f2$`97.5 %`[1], rare = 1)
evalues.HR(est = tot_m2$estimate[1], lo = tot_m2$`2.5 %`[1], hi = tot_m2$`97.5 %`[1], rare = 1)

## Test of proportionality assumption
# Stroke
cox.zph(strok_compl_f2)
cox.zph(strok_compl_m)
# Coronary heart disease
cox.zph(chd_compl_f2)
cox.zph(chd_compl_m2)
# Myocardial infarction
cox.zph(myinf_compl_f2)
cox.zph(myinf_compl_m2)
# Angina
cox.zph(ang_compl_f2)
cox.zph(ang_compl_m2)
# Any CVD
cox.zph(tot_compl_f2)
cox.zph(tot_compl_m2)

# Figure 2 #####
## Kaplan Meier plots
# Stroke
s_strok_f <- survfit(Surv(survey_age, survey_age + os_yrs_strok, status_strok) ~ fert, data = pop_dat[pop_dat$sex == 'F',])
s_strok_m <- survfit(Surv(survey_age, survey_age + os_yrs_strok, status_strok) ~ fert, data = pop_dat[pop_dat$sex == 'M',])
p_strok_f <- ggsurvplot(fit = s_strok_f, data = pop_dat[pop_dat$sex == 'F',],
                        xlab = "Age", ylab = "Cumulative hazard",
                        xlim = c(19.5,100),
                        break.x.by=10,
                        legend = "bottom",
                        legend.labs = c("Fertile", "Subfertile"),
                        legend.title = "",
                        palette = c("black","gray40"),
                        linetype = c("dotted","solid"),
                        short.panel.labs = TRUE,
                        fun = "cumhaz",
                        conf.int = TRUE,
                        conf.int.alpha = 0.2,
                        censor.shape = "") +
  ggtitle("Stroke")
p_strok_m <- ggsurvplot(s_strok_m, data = pop_dat[pop_dat$sex == 'M',],
                        xlab = "Age", ylab = "Cumulative hazard",
                        xlim = c(19.5,100),
                        break.x.by=10,
                        legend = "bottom",
                        legend.labs = c("Fertile", "Subfertile"),
                        legend.title = "",
                        palette = c("black","gray40"),
                        linetype = c("dotted","solid"),
                        short.panel.labs = TRUE,
                        fun = "cumhaz",
                        conf.int = TRUE,
                        conf.int.alpha = 0.2,
                        censor.shape = "") +
  ggtitle("Stroke")

# Coronary heart disease
s_chd_f <- survfit(Surv(survey_age, survey_age + os_yrs_chd, status_chd) ~ fert, data = pop_dat[pop_dat$sex == 'F',])
s_chd_m <- survfit(Surv(survey_age, survey_age + os_yrs_chd, status_chd) ~ fert, data = pop_dat[pop_dat$sex == 'M',])
p_chd_f <- ggsurvplot(s_chd_f, data = pop_dat[pop_dat$sex == 'F',],
                      xlab = "Age", ylab = "Cumulative hazard",
                      xlim = c(19.5,100),
                      break.x.by=10,
                      legend = "bottom",
                      legend.labs = c("Fertile", "Subfertile"),
                      legend.title = "",
                      palette = c("black","gray40"),
                      linetype = c('dotted','solid'),
                      short.panel.labs = TRUE,
                      fun = "cumhaz",
                      conf.int = TRUE,
                      conf.int.alpha = 0.2,
                      censor.shape = "") +
  ggtitle("Coronary heart disease")
p_chd_m <- ggsurvplot(s_chd_m, data = pop_dat[pop_dat$sex == 'M',],
                      xlab = "Age", ylab = "Cumulative hazard",
                      xlim = c(19.5,100),
                      break.y.by=0.2,
                      break.x.by=10,
                      legend = "bottom",
                      legend.labs = c("Fertile", "Subfertile"),
                      legend.title = "",
                      palette = c("black","gray40"),
                      linetype = c("dotted","solid"),
                      short.panel.labs = TRUE,
                      fun = "cumhaz",
                      conf.int = TRUE,
                      conf.int.alpha = 0.2,
                      censor.shape = "") +
  ggtitle("Coronary heart disease")

# Myocardial infarction
s_myinf_f <- survfit(Surv(survey_age, survey_age + os_yrs_myinf, status_myinf) ~ fert, data = pop_dat[pop_dat$sex == 'F',])
s_myinf_m <- survfit(Surv(survey_age, survey_age + os_yrs_myinf, status_myinf) ~ fert, data = pop_dat[pop_dat$sex == 'M',])
p_myinf_f <- ggsurvplot(s_myinf_f, data = pop_dat[pop_dat$sex == 'F',],
                        xlab = "Age", ylab = "Cumulative hazard",
                        xlim = c(19.5,100),
                        break.x.by=10,
                        legend = "bottom",
                        legend.labs = c("Fertile", "Subfertile"),
                        legend.title = "",
                        palette = c("black","gray40"),
                        linetype = c("dotted","solid"),
                        short.panel.labs = TRUE,
                        fun = "cumhaz",
                        conf.int = TRUE,
                        conf.int.alpha = 0.2,
                        censor.shape = "") +
  ggtitle("Myocardial infarction")
p_myinf_m <- ggsurvplot(s_myinf_m, data = pop_dat[pop_dat$sex == 'M',],
                        xlab = "Age", ylab = "Cumulative hazard",
                        xlim = c(19.5,100),
                        break.x.by=10,
                        break.y.by=0.1,
                        legend = "bottom",
                        legend.labs = c("Fertile", "Subfertile"),
                        legend.title = "",
                        palette = c("black","gray40"),
                        linetype = c("dotted","solid"),
                        short.panel.labs = TRUE,
                        fun = "cumhaz",
                        conf.int = TRUE,
                        conf.int.alpha = 0.2,
                        censor.shape = "") +
  ggtitle("Myocardial infarction")

# Angina
s_ang_f <- survfit(Surv(survey_age, survey_age + os_yrs_ang, status_ang) ~ fert, data = pop_dat[pop_dat$sex == 'F',])
s_ang_m <- survfit(Surv(survey_age, survey_age + os_yrs_ang, status_ang) ~ fert, data = pop_dat[pop_dat$sex == 'M',])
p_ang_f <- ggsurvplot(s_ang_f, data = pop_dat[pop_dat$sex == 'F',],
                      xlab = "Age", ylab = "Cumulative hazard",
                      xlim = c(19.5,100),
                      break.y.by=0.05,
                      break.x.by=10,
                      legend = 'bottom',
                      legend.labs = c("Fertile", "Subfertile"),
                      legend.title = "",
                      palette = c("black","gray40"),
                      linetype = c("dotted","solid"),
                      short.panel.labs = TRUE,
                      fun = "cumhaz",
                      conf.int = TRUE,
                      conf.int.alpha = 0.2,
                      censor.shape = "") +
  ggtitle("Angina")
p_ang_m <- ggsurvplot(s_ang_m, data = pop_dat[pop_dat$sex == 'M',],
                      xlab = "Age", ylab = "Cumulative hazard",
                      xlim = c(19.5,100),
                      break.x.by=10,
                      break.y.by=0.1,
                      legend = 'bottom',
                      legend.labs = c("Fertile", "Subfertile"),
                      legend.title = "",
                      palette = c("black","gray40"),
                      linetype = c("dotted","solid"),
                      short.panel.labs = TRUE,
                      fun = "cumhaz",
                      conf.int = TRUE,
                      conf.int.alpha = 0.2,
                      censor.shape = "") +
  ggtitle("Angina")

# Save plots
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data/Export this")
p_f <- (p_strok_f$plot + p_chd_f$plot) / (p_myinf_f$plot + p_ang_f$plot) / guide_area() +
  plot_layout(guides = "collect", heights = c(2.4,2.4,0.2))
p_m <- (p_strok_m$plot + p_chd_m$plot) / (p_myinf_m$plot + p_ang_m$plot) / guide_area() +
  plot_layout(guides = "collect", heights = c(2.4,2.4,0.2))
p_kaplan <- wrap_elements(p_f) + wrap_elements(p_m) +
  plot_annotation(tag_levels = list(c('A) Women','B) Men'))) & theme(plot.tag.position = c(0,1), plot.tag = element_text(hjust =-0.2))
ggsave("kaplanmeier.jpg", p_kaplan, dpi=300, width=16,height=8,unit="in")
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")

# Figure 3 #####
## Main forest plots
# Women
p_main_f <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_f1[1,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_f1[1,7],
                     xmax = strok_f1[1,8],
                     y = 14)) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_f2[1,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_f2[1,7],
                     xmax = strok_f2[1,8],
                     y = 13)) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_f1[1,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_f1[1,7],
                     xmax = chd_f1[1,8],
                     y = 11)) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_f2[1,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_f2[1,7],
                     xmax = chd_f2[1,8],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +

  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_f1[1,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_f1[1,7],
                     xmax = myinf_f1[1,8],
                     y = 8)) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_f2[1,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_f2[1,7],
                     xmax = myinf_f2[1,8],
                     y = 7)) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +

  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_f1[1,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_f1[1,7],
                     xmax = 1.40,
                     y = 5)) +
  geom_segment(aes(x = 1.35, y = 5, xend = 1.40, yend = 5),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_f2[1,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_f2[1,7],
                     xmax = 1.40,
                     y = 4)) +
  geom_segment(aes(x = 1.35, y = 4, xend = 1.40, yend = 4),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD outcome
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_f1[1,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_f1[1,7],
                     xmax = tot_f1[1,8],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_f2[1,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_f2[1,7],
                     xmax = tot_f2[1,8],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('A) Women') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

# Men
p_main_m <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'no' & imp$data$sex == 'M',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'no' & imp$data$sex == 'M'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'yes' & imp$data$sex == 'M'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_m1[1,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_m1[1,7],
                     xmax = strok_m1[1,8],
                     y = 14)) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_m2[1,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_m2[1,7],
                     xmax = strok_m2[1,8],
                     y = 13)) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'no' & imp$data$sex == 'M',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'no' & imp$data$sex == 'M'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'yes' & imp$data$sex == 'M'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_m1[1,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_m1[1,7],
                     xmax = chd_m1[1,8],
                     y = 11)) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_m2[1,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_m2[1,7],
                     xmax = chd_m2[1,8],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'no' & imp$data$sex == 'M',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'no' & imp$data$sex == 'M'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'yes' & imp$data$sex == 'M'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_m1[1,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_m1[1,7],
                     xmax = myinf_m1[1,8],
                     y = 8)) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_m2[1,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_m2[1,7],
                     xmax = myinf_m2[1,8],
                     y = 7)) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'no' & imp$data$sex == 'M',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'no' & imp$data$sex == 'M'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'yes' & imp$data$sex == 'M'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_m1[1,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_m1[1,7],
                     xmax = ang_m1[1,8],
                     y = 5)) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_m2[1,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_m2[1,7],
                     xmax = ang_m2[1,8],
                     y = 4)) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD outcome
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'no' & imp$data$sex == 'M',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'no' & imp$data$sex == 'M'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'yes' & imp$data$sex == 'M'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_m1[1,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_m1[1,7],
                     xmax = tot_m1[1,8],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_m2[1,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_m2[1,7],
                     xmax = tot_m2[1,8],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +

  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('B) Men') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

# Save plots
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data/Export this")
p_forest <- grid.arrange(p_main_f, p_main_m, ncol=2)
ggsave("forest_main.jpg",p_forest, dpi=1000, width=16,height=6,unit="in")
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")


# Figure S1 #####
## Complete-case analysis
# Women
p_f_S1 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(pop_dat[pop_dat$status_strok == 1 & pop_dat$fert == 'no' & pop_dat$sex == 'F' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_strok[pop_dat$fert == 'no' & pop_dat$sex == 'F' & 
                                                                                   !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                   !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                   !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(pop_dat[pop_dat$status_strok == 1 & pop_dat$fert == 'yes' & pop_dat$sex == 'F' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_strok[pop_dat$fert == 'yes' & pop_dat$sex == 'F' & 
                                                                                   !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                   !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                   !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = exp(strok_compl_f1$coefficients[1]), y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(strok_compl_f1))[1,1],
                     xmax = exp(confint(strok_compl_f1))[1,2],
                     y = 14)) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', exp(strok_compl_f1$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(strok_compl_f1))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(strok_compl_f1))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = exp(strok_compl_f2$coefficients[1]), y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(strok_compl_f2))[1,1],
                     xmax = exp(confint(strok_compl_f2))[1,2],
                     y = 13)) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', exp(strok_compl_f2$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(strok_compl_f2))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(strok_compl_f2))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(pop_dat[pop_dat$status_chd == 1 & pop_dat$fert == 'no' & pop_dat$sex == 'F' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_chd[pop_dat$fert == 'no' & pop_dat$sex == 'F' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(pop_dat[pop_dat$status_chd == 1 & pop_dat$fert == 'yes' & pop_dat$sex == 'F' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_chd[pop_dat$fert == 'yes' & pop_dat$sex == 'F' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = exp(chd_compl_f1$coefficients[1]), y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(chd_compl_f1))[1,1],
                     xmax = exp(confint(chd_compl_f1))[1,2],
                     y = 11)) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', exp(chd_compl_f1$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(chd_compl_f1))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(chd_compl_f1))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = exp(chd_compl_f2$coefficients[1]), y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(chd_compl_f2))[1,1],
                     xmax = exp(confint(chd_compl_f2))[1,2],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', exp(chd_compl_f2$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(chd_compl_f2))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(chd_compl_f2))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(pop_dat[pop_dat$status_myinf == 1 & pop_dat$fert == 'no' & pop_dat$sex == 'F' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_myinf[pop_dat$fert == 'no' & pop_dat$sex == 'F' & 
                                                                                   !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                   !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                   !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(pop_dat[pop_dat$status_myinf == 1 & pop_dat$fert == 'yes' & pop_dat$sex == 'F' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_myinf[pop_dat$fert == 'yes' & pop_dat$sex == 'F' & 
                                                                                   !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                   !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                   !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = exp(myinf_compl_f1$coefficients[1]), y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(myinf_compl_f1))[1,1],
                     xmax = exp(confint(myinf_compl_f1))[1,2],
                     y = 8)) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', exp(myinf_compl_f1$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(myinf_compl_f1))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(myinf_compl_f1))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = exp(myinf_compl_f2$coefficients[1]), y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(myinf_compl_f2))[1,1],
                     xmax = exp(confint(myinf_compl_f2))[1,2],
                     y = 7)) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', exp(myinf_compl_f2$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(myinf_compl_f2))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(myinf_compl_f2))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(pop_dat[pop_dat$status_ang == 1 & pop_dat$fert == 'no' & pop_dat$sex == 'F' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_ang[pop_dat$fert == 'no' & pop_dat$sex == 'F' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(pop_dat[pop_dat$status_ang == 1 & pop_dat$fert == 'yes' & pop_dat$sex == 'F' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_ang[pop_dat$fert == 'yes' & pop_dat$sex == 'F' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = exp(ang_compl_f1$coefficients[1]), y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(ang_compl_f1))[1,1],
                     xmax = 1.40,
                     y = 5)) +
  geom_segment(aes(x = 1.35, y = 5, xend = 1.40, yend = 5),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', exp(ang_compl_f1$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(ang_compl_f1))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(ang_compl_f1))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = exp(ang_compl_f2$coefficients[1]), y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(ang_compl_f2))[1,1],
                     xmax = 1.40,
                     y = 4)) +
  geom_segment(aes(x = 1.35, y = 4, xend = 1.40, yend = 4),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', exp(ang_compl_f2$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(ang_compl_f2))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(ang_compl_f2))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD outcome
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(pop_dat[pop_dat$status_tot == 1 & pop_dat$fert == 'no' & pop_dat$sex == 'F' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_tot[pop_dat$fert == 'no' & pop_dat$sex == 'F' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(pop_dat[pop_dat$status_tot == 1 & pop_dat$fert == 'yes' & pop_dat$sex == 'F' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_tot[pop_dat$fert == 'yes' & pop_dat$sex == 'F' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = exp(tot_compl_f1$coefficients[1]), y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(tot_compl_f1))[1,1],
                     xmax = exp(confint(tot_compl_f1))[1,2],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', exp(tot_compl_f1$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(tot_compl_f1))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(tot_compl_f1))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = exp(tot_compl_f2$coefficients[1]), y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(tot_compl_f2))[1,1],
                     xmax = exp(confint(tot_compl_f2))[1,2],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', exp(tot_compl_f2$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(tot_compl_f2))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(tot_compl_f2))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('A) Women') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

# Men
p_m_S1 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(pop_dat[pop_dat$status_strok == 1 & pop_dat$fert == 'no' & pop_dat$sex == 'M' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_strok[pop_dat$fert == 'no' & pop_dat$sex == 'M' & 
                                                                                   !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                   !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                   !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(pop_dat[pop_dat$status_strok == 1 & pop_dat$fert == 'yes' & pop_dat$sex == 'M' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_strok[pop_dat$fert == 'yes' & pop_dat$sex == 'M' & 
                                                                                   !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                   !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                   !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = exp(strok_compl_m1$coefficients[1]), y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(strok_compl_m1))[1,1],
                     xmax = exp(confint(strok_compl_m1))[1,2],
                     y = 14)) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', exp(strok_compl_m1$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(strok_compl_m1))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(strok_compl_m1))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = exp(strok_compl_m2$coefficients[1]), y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(strok_compl_m2))[1,1],
                     xmax = exp(confint(strok_compl_m2))[1,2],
                     y = 13)) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', exp(strok_compl_m2$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(strok_compl_m2))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(strok_compl_m2))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(pop_dat[pop_dat$status_chd == 1 & pop_dat$fert == 'no' & pop_dat$sex == 'M' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_chd[pop_dat$fert == 'no' & pop_dat$sex == 'M' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(pop_dat[pop_dat$status_chd == 1 & pop_dat$fert == 'yes' & pop_dat$sex == 'M' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_chd[pop_dat$fert == 'yes' & pop_dat$sex == 'M' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = exp(chd_compl_m1$coefficients[1]), y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(chd_compl_m1))[1,1],
                     xmax = exp(confint(chd_compl_m1))[1,2],
                     y = 11)) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', exp(chd_compl_m1$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(chd_compl_m1))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(chd_compl_m1))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = exp(chd_compl_m2$coefficients[1]), y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(chd_compl_m2))[1,1],
                     xmax = exp(confint(chd_compl_m2))[1,2],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', exp(chd_compl_m2$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(chd_compl_m2))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(chd_compl_m2))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(pop_dat[pop_dat$status_myinf == 1 & pop_dat$fert == 'no' & pop_dat$sex == 'M' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_myinf[pop_dat$fert == 'no' & pop_dat$sex == 'M' & 
                                                                                   !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                   !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                   !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(pop_dat[pop_dat$status_myinf == 1 & pop_dat$fert == 'yes' & pop_dat$sex == 'M' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_myinf[pop_dat$fert == 'yes' & pop_dat$sex == 'M' & 
                                                                                   !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                   !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                   !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = exp(myinf_compl_m1$coefficients[1]), y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(myinf_compl_m1))[1,1],
                     xmax = exp(confint(myinf_compl_m1))[1,2],
                     y = 8)) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', exp(myinf_compl_m1$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(myinf_compl_m1))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(myinf_compl_m1))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = exp(myinf_compl_m2$coefficients[1]), y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(myinf_compl_m2))[1,1],
                     xmax = exp(confint(myinf_compl_m2))[1,2],
                     y = 7)) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', exp(myinf_compl_m2$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(myinf_compl_m2))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(myinf_compl_m2))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(pop_dat[pop_dat$status_ang == 1 & pop_dat$fert == 'no' & pop_dat$sex == 'M' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_ang[pop_dat$fert == 'no' & pop_dat$sex == 'M' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(pop_dat[pop_dat$status_ang == 1 & pop_dat$fert == 'yes' & pop_dat$sex == 'M' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_ang[pop_dat$fert == 'yes' & pop_dat$sex == 'M' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = exp(ang_compl_m1$coefficients[1]), y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(ang_compl_m1))[1,1],
                     xmax = exp(confint(ang_compl_m1))[1,2],
                     y = 5)) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', exp(ang_compl_m1$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(ang_compl_m1))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(ang_compl_m1))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = exp(ang_compl_m2$coefficients[1]), y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(ang_compl_m2))[1,1],
                     xmax = exp(confint(ang_compl_m2))[1,2],
                     y = 4)) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', exp(ang_compl_m2$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(ang_compl_m2))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(ang_compl_m2))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD outcome
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(pop_dat[pop_dat$status_tot == 1 & pop_dat$fert == 'no' & pop_dat$sex == 'M' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_tot[pop_dat$fert == 'no' & pop_dat$sex == 'M' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(pop_dat[pop_dat$status_tot == 1 & pop_dat$fert == 'yes' & pop_dat$sex == 'M' & 
                                               !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                               !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                               !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear),1]),
                              prettyNum(sprintf("%1.0f",sum(pop_dat$os_yrs_tot[pop_dat$fert == 'yes' & pop_dat$sex == 'M' & 
                                                                                 !is.na(pop_dat$coh_sps) & !is.na(pop_dat$bmi) & !is.na(pop_dat$smok_cat) &
                                                                                 !is.na(pop_dat$educ) & !is.na(pop_dat$diabetes) & !is.na(pop_dat$bp_dias) &
                                                                                 !is.na(pop_dat$bp_syst) & !is.na(pop_dat$serum_chol) & !is.na(pop_dat$birthyear)])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = exp(tot_compl_m1$coefficients[1]), y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(tot_compl_m1))[1,1],
                     xmax = exp(confint(tot_compl_m1))[1,2],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', exp(tot_compl_m1$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(tot_compl_m1))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(tot_compl_m1))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = exp(tot_compl_m2$coefficients[1]), y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = exp(confint(tot_compl_m2))[1,1],
                     xmax = exp(confint(tot_compl_m2))[1,2],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', exp(tot_compl_m2$coefficients[1])),
                              paste(paste('(', sprintf('%.2f', exp(confint(tot_compl_m2))[1,1]), sep = ""),
                                    paste(sprintf('%.2f', exp(confint(tot_compl_m2))[1,2]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('B) Men') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

# Save plots
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data/Export this")
p_forest_S1 <- grid.arrange(p_f_S1, p_m_S1, ncol=2)
ggsave("forest_S1.jpg",p_forest_S1, dpi=1000, width=16,height=6,unit="in")
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")

# Figure S2 #####
## Women with known partners
p_S2 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_partn_f1[1,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_partn_f1[1,7],
                     xmax = 1.40,
                     y = 14)) +
  geom_segment(aes(x = 1.35, y = 14, xend = 1.40, yend = 14),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_partn_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_partn_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_partn_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_partn_f2[1,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_partn_f2[1,7],
                     xmax = 1.40,
                     y = 13)) +
  geom_segment(aes(x = 1.35, y = 13, xend = 1.40, yend = 13),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_partn_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_partn_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_partn_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_partn_f1[1,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_partn_f1[1,7],
                     xmax = 1.40,
                     y = 11)) +
  geom_segment(aes(x = 1.35, y = 11, xend = 1.40, yend = 11),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_partn_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_partn_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_partn_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_partn_f2[1,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_partn_f2[1,7],
                     xmax = 1.40,
                     y = 10)) +
  geom_segment(aes(x = 1.35, y = 10, xend = 1.40, yend = 10),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_partn_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_partn_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_partn_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_partn_f1[1,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_partn_f1[1,7],
                     xmax = myinf_partn_f1[1,8],
                     y = 8)) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_partn_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_partn_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_partn_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_partn_f2[1,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_partn_f2[1,7],
                     xmax = myinf_partn_f2[1,8],
                     y = 7)) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_partn_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_partn_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_partn_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_partn_f1[1,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_partn_f1[1,7],
                     xmax = 1.40,
                     y = 5)) +
  geom_segment(aes(x = 1.35, y = 5, xend = 1.40, yend = 5),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_partn_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_partn_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_partn_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_partn_f2[1,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_partn_f2[1,7],
                     xmax = 1.40,
                     y = 4)) +
  geom_segment(aes(x = 1.35, y = 4, xend = 1.40, yend = 4),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_partn_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_partn_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_partn_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD outcome
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_partn_f1[1,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_partn_f1[1,7],
                     xmax = tot_partn_f1[1,8],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_partn_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_partn_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_partn_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_partn_f2[1,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_partn_f2[1,7],
                     xmax = 1.40,
                     y = 1)) +
  geom_segment(aes(x = 1.35, y = 1, xend = 1.40, yend = 1),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_partn_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_partn_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_partn_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('Women with known partners') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

p_S2

# Save plot
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data/Export this")
ggsave("forest_S2.jpg",p_S2, dpi=1000, width=8,height=6,unit="in")
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")

# Figure S3 #####
## Subfertility and infertility
# Subfertile women
p_sub_S3 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = 0.31125, y = 16.5),
            label = 'Events / person years') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert_child == 'fertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert_child == 'fertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert_child == 'subfertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert_child == 'subfertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_subf_f1[1,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_subf_f1[1,7],
                     xmax = 1.40,
                     y = 14)) +
  geom_segment(aes(x = 1.35, y = 14, xend = 1.40, yend = 14),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_subf_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_subf_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_subf_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_subf_f2[1,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_subf_f2[1,7],
                     xmax = 1.40,
                     y = 13)) +
  geom_segment(aes(x = 1.35, y = 13, xend = 1.40, yend = 13),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_subf_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_subf_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_subf_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert_child == 'fertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert_child == 'fertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert_child == 'subfertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert_child == 'subfertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_subf_f1[1,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_subf_f1[1,7],
                     xmax = chd_subf_f1[1,8],
                     y = 11)) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_subf_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_subf_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_subf_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_subf_f2[1,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_subf_f2[1,7],
                     xmax = chd_subf_f2[1,8],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_subf_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_subf_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_subf_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert_child == 'fertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert_child == 'fertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert_child == 'subfertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert_child == 'subfertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_subf_f1[1,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_subf_f1[1,7],
                     xmax = myinf_subf_f1[1,8],
                     y = 8)) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_subf_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_subf_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_subf_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_subf_f2[1,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_subf_f2[1,7],
                     xmax = myinf_subf_f2[1,8],
                     y = 7)) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_subf_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_subf_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_subf_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert_child == 'fertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert_child == 'fertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert_child == 'subfertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert_child == 'subfertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_subf_f1[1,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_subf_f1[1,7],
                     xmax = 1.40,
                     y = 5)) +
  geom_segment(aes(x = 1.35, y = 5, xend = 1.40, yend = 5),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_subf_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_subf_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_subf_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_subf_f2[1,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_subf_f2[1,7],
                     xmax = 1.40,
                     y = 4)) +
  geom_segment(aes(x = 1.35, y = 4, xend = 1.40, yend = 4),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_subf_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_subf_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_subf_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert_child == 'fertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert_child == 'fertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert_child == 'subfertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert_child == 'subfertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_subf_f1[1,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_subf_f1[1,7],
                     xmax = tot_subf_f1[1,8],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_subf_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_subf_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_subf_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_subf_f2[1,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_subf_f2[1,7],
                     xmax = tot_subf_f2[1,8],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_subf_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_subf_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_subf_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  #geom_vline(aes(xintercept = 1)) +
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('A) Subfertile women') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

# Infertile women
p_inf_S3 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = 0.31125, y = 16.5),
            label = 'Events / person years') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert_child == 'fertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert_child == 'fertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert_child == 'infertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert_child == 'infertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_subf_f1[2,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_subf_f1[2,7],
                     xmax = 1.40,
                     y = 14)) +
  geom_segment(aes(x = 1.35, y = 14, xend = 1.40, yend = 14),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_subf_f1[2,2]),
                              paste(paste('(', sprintf('%.2f', strok_subf_f1[2,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_subf_f1[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_subf_f2[2,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_subf_f2[2,7],
                     xmax = strok_subf_f2[2,8],
                     y = 13)) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_subf_f2[2,2]),
                              paste(paste('(', sprintf('%.2f', strok_subf_f2[2,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_subf_f2[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert_child == 'fertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert_child == 'fertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert_child == 'infertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert_child == 'infertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_subf_f1[2,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_subf_f1[2,7],
                     xmax = 1.40,
                     y = 11)) +
  geom_segment(aes(x = 1.35, y = 11, xend = 1.40, yend = 11),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_subf_f1[2,2]),
                              paste(paste('(', sprintf('%.2f', chd_subf_f1[2,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_subf_f1[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_subf_f2[2,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_subf_f2[2,7],
                     xmax = chd_subf_f2[2,8],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_subf_f2[2,2]),
                              paste(paste('(', sprintf('%.2f', chd_subf_f2[2,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_subf_f2[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert_child == 'fertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert_child == 'fertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert_child == 'infertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert_child == 'infertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_subf_f1[2,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_subf_f1[2,7],
                     xmax = 1.40,
                     y = 8)) +
  geom_segment(aes(x = 1.35, y = 8, xend = 1.40, yend = 8),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_subf_f1[2,2]),
                              paste(paste('(', sprintf('%.2f', myinf_subf_f1[2,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_subf_f1[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_subf_f2[2,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_subf_f2[2,7],
                     xmax = 1.40,
                     y = 7)) +
  geom_segment(aes(x = 1.35, y = 7, xend = 1.40, yend = 7),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_subf_f2[2,2]),
                              paste(paste('(', sprintf('%.2f', myinf_subf_f2[2,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_subf_f2[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert_child == 'fertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert_child == 'fertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert_child == 'infertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert_child == 'infertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_subf_f1[2,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_subf_f1[2,7],
                     xmax = 1.40,
                     y = 5)) +
  geom_segment(aes(x = 1.35, y = 5, xend = 1.40, yend = 5),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_subf_f1[2,2]),
                              paste(paste('(', sprintf('%.2f', ang_subf_f1[2,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_subf_f1[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_subf_f2[2,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_subf_f2[2,7],
                     xmax = ang_subf_f2[2,8],
                     y = 4)) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_subf_f2[2,2]),
                              paste(paste('(', sprintf('%.2f', ang_subf_f2[2,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_subf_f2[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert_child == 'fertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert_child == 'fertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert_child == 'infertile' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert_child == 'infertile' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_subf_f1[2,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_subf_f1[2,7],
                     xmax = tot_subf_f1[2,8],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_subf_f1[2,2]),
                              paste(paste('(', sprintf('%.2f', tot_subf_f1[2,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_subf_f1[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_subf_f2[2,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_subf_f2[2,7],
                     xmax = tot_subf_f2[2,8],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_subf_f2[2,2]),
                              paste(paste('(', sprintf('%.2f', tot_subf_f2[2,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_subf_f2[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('B) Infertile women') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data/Export this")
p_forest_subinf <- grid.arrange(p_sub_S3, p_inf_S3, ncol=2)
ggsave("forest_S3.jpg",p_forest_subinf, dpi=1000, width=16,height=6,unit="in")
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")

# Figure S4 #####
## Age stratified, women
# Age below median
p_lowf_S4 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'low'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'low'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_strat_low_f1[1,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_strat_low_f1[1,7],
                     xmax = 1.40,
                     y = 14)) +
  geom_segment(aes(x = 1.35, y = 14, xend = 1.40, yend = 14),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_strat_low_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_strat_low_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_strat_low_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_strat_low_f2[1,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_strat_low_f2[1,7],
                     xmax = 1.40,
                     y = 13)) +
    geom_segment(aes(x = 1.35, y = 13, xend = 1.40, yend = 13),
                 arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_strat_low_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_strat_low_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_strat_low_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'low'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'low'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_strat_low_f1[1,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_strat_low_f1[1,7],
                     xmax = 1.40,
                     y = 11)) +
    geom_segment(aes(x = 1.35, y = 11, xend = 1.40, yend = 11),
                 arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_strat_low_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_strat_low_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_strat_low_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_strat_low_f2[1,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_strat_low_f2[1,7],
                     xmax = 1.40,
                     y = 10)) +
    geom_segment(aes(x = 1.35, y = 10, xend = 1.40, yend = 10),
                 arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_strat_low_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_strat_low_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_strat_low_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'low'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'low'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_strat_low_f1[1,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_strat_low_f1[1,7],
                     xmax = 1.40,
                     y = 8)) +
    geom_segment(aes(x = 1.35, y = 8, xend = 1.40, yend = 8),
                 arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_strat_low_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_strat_low_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_strat_low_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_strat_low_f2[1,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_strat_low_f2[1,7],
                     xmax = 1.40,
                     y = 7)) +
    geom_segment(aes(x = 1.35, y = 7, xend = 1.40, yend = 7),
                 arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) + 
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_strat_low_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_strat_low_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_strat_low_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'low'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'low'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_strat_low_f1[1,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_strat_low_f1[1,7],
                     xmax = 1.40,
                     y = 5)) +
  geom_segment(aes(x = 1.35, y = 5, xend = 1.40, yend = 5),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_strat_low_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_strat_low_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_strat_low_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_strat_low_f2[1,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_strat_low_f2[1,7],
                     xmax = 1.40,
                     y = 4)) +
  geom_segment(aes(x = 1.35, y = 4, xend = 1.40, yend = 4),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_strat_low_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_strat_low_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_strat_low_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD outcome
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'low'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'low'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_strat_low_f1[1,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_strat_low_f1[1,7],
                     xmax = tot_strat_low_f1[1,8],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_strat_low_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_strat_low_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_strat_low_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_strat_low_f2[1,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_strat_low_f2[1,7],
                     xmax = tot_strat_low_f2[1,8],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_strat_low_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_strat_low_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_strat_low_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('A) Age \u2264 47') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

# Age above median
p_highf_S4 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'high'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'high'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_strat_high_f1[1,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_strat_high_f1[1,7],
                     xmax = strok_strat_high_f1[1,8],
                     y = 14)) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_strat_high_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_strat_high_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_strat_high_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_strat_high_f2[1,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_strat_high_f2[1,7],
                     xmax = strok_strat_high_f2[1,8],
                     y = 13)) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_strat_high_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_strat_high_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_strat_high_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'high'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'high'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_strat_high_f1[1,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_strat_high_f1[1,7],
                     xmax = chd_strat_high_f1[1,8],
                     y = 11)) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_strat_high_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_strat_high_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_strat_high_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_strat_high_f2[1,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_strat_high_f2[1,7],
                     xmax = chd_strat_high_f2[1,8],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_strat_high_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_strat_high_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_strat_high_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'high'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'high'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_strat_high_f1[1,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_strat_high_f1[1,7],
                     xmax = myinf_strat_high_f1[1,8],
                     y = 8)) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_strat_high_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_strat_high_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_strat_high_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_strat_high_f2[1,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_strat_high_f2[1,7],
                     xmax = myinf_strat_high_f2[1,8],
                     y = 7)) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_strat_high_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_strat_high_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_strat_high_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'high'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'high'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_strat_high_f1[1,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_strat_high_f1[1,7],
                     xmax = 1.40,
                     y = 5)) +
  geom_segment(aes(x = 1.35, y = 5, xend = 1.40, yend = 5),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_strat_high_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_strat_high_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_strat_high_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_strat_high_f2[1,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_strat_high_f2[1,7],
                     xmax = 1.40,
                     y = 4)) +
  geom_segment(aes(x = 1.35, y = 4, xend = 1.40, yend = 4),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_strat_high_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_strat_high_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_strat_high_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD outcome
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'no' & imp$data$sex == 'F' & imp$data$strat_age == 'high'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'yes' & imp$data$sex == 'F' & imp$data$strat_age == 'high'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_strat_high_f1[1,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_strat_high_f1[1,7],
                     xmax = tot_strat_high_f1[1,8],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_strat_high_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_strat_high_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_strat_high_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_strat_high_f2[1,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_strat_high_f2[1,7],
                     xmax = tot_strat_high_f2[1,8],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_strat_high_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_strat_high_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_strat_high_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('B) Age > 47') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

# Save plots
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data/Export this")
p_forest_S4 <- grid.arrange(p_lowf_S4, p_highf_S4, ncol=2)
ggsave("forest_S4.jpg",p_forest_S4, dpi=1000, width=16,height=6,unit="in")
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")

# Figure S5 #####
## Age stratified, men
# Age below median
p_lowm_S5 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'low'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'low'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_strat_low_m1[1,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_strat_low_m1[1,7],
                     xmax = 1.40,
                     y = 14)) +
  geom_segment(aes(x = 1.35, y = 14, xend = 1.40, yend = 14),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_strat_low_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_strat_low_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_strat_low_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_strat_low_m2[1,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_strat_low_m2[1,7],
                     xmax = 1.40,
                     y = 13)) +
  
  geom_segment(aes(x = 1.35, y = 13, xend = 1.40, yend = 13),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_strat_low_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_strat_low_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_strat_low_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'low'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'low'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_strat_low_m1[1,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_strat_low_m1[1,7],
                     xmax = chd_strat_low_m1[1,8],
                     y = 11)) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_strat_low_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_strat_low_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_strat_low_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_strat_low_m2[1,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_strat_low_m2[1,7],
                     xmax = chd_strat_low_m2[1,8],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_strat_low_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_strat_low_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_strat_low_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'low'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'low'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_strat_low_m1[1,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = 0.60,
                     xmax = myinf_strat_low_m1[1,8],
                     y = 8)) +
  geom_segment(aes(x = 0.65, y = 8, xend = 0.60, yend = 8),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_strat_low_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_strat_low_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_strat_low_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_strat_low_m2[1,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = 0.60,
                     xmax = myinf_strat_low_m2[1,8],
                     y = 7)) +
  geom_segment(aes(x = 0.65, y = 7, xend = 0.60, yend = 7),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_strat_low_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_strat_low_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_strat_low_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'low'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'low'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_strat_low_m1[1,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_strat_low_m1[1,7],
                     xmax = ang_strat_low_m1[1,8],
                     y = 5)) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_strat_low_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_strat_low_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_strat_low_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_strat_low_m2[1,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_strat_low_m2[1,7],
                     xmax = ang_strat_low_m2[1,8],
                     y = 4)) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_strat_low_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_strat_low_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_strat_low_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD outcome
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'low'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'low',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'low'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_strat_low_m1[1,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_strat_low_m1[1,7],
                     xmax = tot_strat_low_m1[1,8],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_strat_low_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_strat_low_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_strat_low_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_strat_low_m2[1,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_strat_low_m2[1,7],
                     xmax = tot_strat_low_m2[1,8],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_strat_low_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_strat_low_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_strat_low_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('A) Age \u2264 47') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))


# Age above median
p_highm_S5 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'high'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'high'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_strat_high_m1[1,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_strat_high_m1[1,7],
                     xmax = strok_strat_high_m1[1,8],
                     y = 14)) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_strat_high_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_strat_high_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_strat_high_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_strat_high_m2[1,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_strat_high_m2[1,7],
                     xmax = strok_strat_high_m2[1,8],
                     y = 13)) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_strat_high_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_strat_high_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_strat_high_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'high'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'high'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_strat_high_m1[1,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_strat_high_m1[1,7],
                     xmax = chd_strat_high_m1[1,8],
                     y = 11)) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_strat_high_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_strat_high_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_strat_high_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_strat_high_m2[1,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_strat_high_m2[1,7],
                     xmax = chd_strat_high_m2[1,8],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_strat_high_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_strat_high_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_strat_high_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'high'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'high'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_strat_high_m1[1,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_strat_high_m1[1,7],
                     xmax = myinf_strat_high_m1[1,8],
                     y = 8)) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_strat_high_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_strat_high_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_strat_high_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_strat_high_m2[1,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_strat_high_m2[1,7],
                     xmax = myinf_strat_high_m2[1,8],
                     y = 7)) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_strat_high_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_strat_high_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_strat_high_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'high'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'high'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_strat_high_m1[1,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_strat_high_m1[1,7],
                     xmax = ang_strat_high_m1[1,8],
                     y = 5)) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_strat_high_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_strat_high_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_strat_high_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_strat_high_m2[1,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_strat_high_m2[1,7],
                     xmax = ang_strat_high_m2[1,8],
                     y = 4)) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_strat_high_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_strat_high_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_strat_high_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD outcome
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'no' & imp$data$sex == 'M' & imp$data$strat_age == 'high'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'high',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'yes' & imp$data$sex == 'M' & imp$data$strat_age == 'high'])), big.mark = ','), sep = ' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_strat_high_m1[1,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_strat_high_m1[1,7],
                     xmax = tot_strat_high_m1[1,8],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_strat_high_m1[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_strat_high_m1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_strat_high_m1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_strat_high_m2[1,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_strat_high_m2[1,7],
                     xmax = tot_strat_high_m2[1,8],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_strat_high_m2[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_strat_high_m2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_strat_high_m2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('B) Age > 47') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

# Save plots
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data/Export this")
p_forest_S5 <- grid.arrange(p_lowm_S5, p_highm_S5, ncol=2)
ggsave("forest_S5.jpg",p_forest_S5, dpi=1000, width=16,height=6,unit="in")
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")


# Figure S6 #####
# Below
p_lowf_S6 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = 0.31125, y = 16.5),
            label = 'Events / person years') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$strat_fert_age == '1_low' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$strat_fert_age == '1_low' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_stratfert_f1[1,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_stratfert_f1[1,7],
                     xmax = 1.40,
                     y = 14)) +
  geom_segment(aes(x = 1.35, y = 14, xend = 1.40, yend = 14),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_stratfert_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_stratfert_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_stratfert_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_stratfert_f2[1,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_stratfert_f2[1,7],
                     xmax = 1.40,
                     y = 13)) +
  geom_segment(aes(x = 1.35, y = 13, xend = 1.40, yend = 13),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_stratfert_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_stratfert_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_stratfert_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$strat_fert_age == '1_low' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$strat_fert_age == '1_low' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_stratfert_f1[1,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_stratfert_f1[1,7],
                     xmax = 1.40,
                     y = 11)) +
  geom_segment(aes(x = 1.35, y = 11, xend = 1.40, yend = 11),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_stratfert_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_stratfert_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_stratfert_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_stratfert_f2[1,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_stratfert_f2[1,7],
                     xmax = chd_stratfert_f2[1,8],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_stratfert_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_stratfert_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_stratfert_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$strat_fert_age == '1_low' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$strat_fert_age == '1_low' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_stratfert_f1[1,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_stratfert_f1[1,7],
                     xmax = 1.40,
                     y = 8)) +
  geom_segment(aes(x = 1.35, y = 8, xend = 1.40, yend = 8),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_stratfert_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_stratfert_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_stratfert_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_stratfert_f2[1,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_stratfert_f2[1,7],
                     xmax = myinf_stratfert_f2[1,8],
                     y = 7)) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_stratfert_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_stratfert_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_stratfert_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$strat_fert_age == '1_low' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$strat_fert_age == '1_low' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_stratfert_f1[1,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_stratfert_f1[1,7],
                     xmax = 1.40,
                     y = 5)) +
  geom_segment(aes(x = 1.35, y = 5, xend = 1.40, yend = 5),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_stratfert_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_stratfert_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_stratfert_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_stratfert_f2[1,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_stratfert_f2[1,7],
                     xmax = ang_stratfert_f2[1,8],
                     y = 4)) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_stratfert_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_stratfert_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_stratfert_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$strat_fert_age == '1_low' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$strat_fert_age == '1_low' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_stratfert_f1[1,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = 0.60,
                     xmax = 1.40,
                     y = 2)) +
  geom_segment(aes(x = 0.65, y = 2, xend = 0.60, yend = 2),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_segment(aes(x = 1.35, y = 2, xend = 1.40, yend = 2),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_stratfert_f1[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_stratfert_f1[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_stratfert_f1[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_stratfert_f2[1,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_stratfert_f2[1,7],
                     xmax = 1.40,
                     y = 1)) +
  geom_segment(aes(x = 1.35, y = 1, xend = 1.40, yend = 1),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_stratfert_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_stratfert_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_stratfert_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('A) Age at subfertility \u2264 25') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

#
p_highf_S6 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = 0.31125, y = 16.5),
            label = 'Events / person years') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$strat_fert_age == '2_high' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$strat_fert_age == '2_high' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = strok_stratfert_f1[2,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_stratfert_f1[2,7],
                     xmax = 1.40,
                     y = 14)) +
  geom_segment(aes(x = 1.35, y = 14, xend = 1.40, yend = 14),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 14, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_stratfert_f1[2,2]),
                              paste(paste('(', sprintf('%.2f', strok_stratfert_f1[2,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_stratfert_f1[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = strok_stratfert_f2[2,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_stratfert_f2[2,7],
                     xmax = 1.40,
                     y = 13)) +
  geom_segment(aes(x = 1.35, y = 13, xend = 1.40, yend = 13),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 13, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_stratfert_f2[2,2]),
                              paste(paste('(', sprintf('%.2f', strok_stratfert_f2[2,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_stratfert_f2[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$strat_fert_age == '2_high' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$strat_fert_age == '2_high' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = chd_stratfert_f1[2,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_stratfert_f1[2,7],
                     xmax = chd_stratfert_f1[2,8],
                     y = 11)) +
  geom_text(aes(x = -0.22, y = 11, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_stratfert_f1[2,2]),
                              paste(paste('(', sprintf('%.2f', chd_stratfert_f1[2,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_stratfert_f1[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = chd_stratfert_f2[2,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_stratfert_f2[2,7],
                     xmax = chd_stratfert_f2[2,8],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_stratfert_f2[2,2]),
                              paste(paste('(', sprintf('%.2f', chd_stratfert_f2[2,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_stratfert_f2[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$strat_fert_age == '2_high' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$strat_fert_age == '2_high' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = myinf_stratfert_f1[2,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_stratfert_f1[2,7],
                     xmax = 1.40,
                     y = 8)) +
  geom_segment(aes(x = 1.35, y = 8, xend = 1.40, yend = 8),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 8, label = 'Age adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_stratfert_f1[2,2]),
                              paste(paste('(', sprintf('%.2f', myinf_stratfert_f1[2,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_stratfert_f1[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = myinf_stratfert_f2[2,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_stratfert_f2[2,7],
                     xmax = 1.40,
                     y = 7)) +
  geom_segment(aes(x = 1.35, y = 7, xend = 1.40, yend = 7),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 7, label = 'Fully adjusted'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_stratfert_f2[2,2]),
                              paste(paste('(', sprintf('%.2f', myinf_stratfert_f2[2,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_stratfert_f2[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$strat_fert_age == '2_high' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$strat_fert_age == '2_high' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = ang_stratfert_f1[2,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_stratfert_f1[2,7],
                     xmax = ang_stratfert_f1[2,8],
                     y = 5)) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_stratfert_f1[2,2]),
                              paste(paste('(', sprintf('%.2f', ang_stratfert_f1[2,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_stratfert_f1[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = ang_stratfert_f2[2,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_stratfert_f2[2,7],
                     xmax = ang_stratfert_f2[2,8],
                     y = 4)) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_stratfert_f2[2,2]),
                              paste(paste('(', sprintf('%.2f', ang_stratfert_f2[2,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_stratfert_f2[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$strat_fert_age == '0_fertile' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$strat_fert_age == '2_high' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$strat_fert_age == '2_high' & !is.na(imp$data$strat_fert_age) & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  # Age adjusted
  geom_point(aes(x = tot_stratfert_f1[2,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = 0.60,
                     xmax = tot_stratfert_f1[2,8],
                     y = 2)) +
  geom_segment(aes(x = 0.65, y = 2, xend = 0.60, yend = 2),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Age adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_stratfert_f1[2,2]),
                              paste(paste('(', sprintf('%.2f', tot_stratfert_f1[2,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_stratfert_f1[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Fully adjusted
  geom_point(aes(x = tot_stratfert_f2[2,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_stratfert_f2[2,7],
                     xmax = 1.40,
                     y = 1)) +
  geom_segment(aes(x = 1.35, y = 1, xend = 1.40, yend = 1),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Fully adjusted'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_stratfert_f2[2,2]),
                              paste(paste('(', sprintf('%.2f', tot_stratfert_f2[2,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_stratfert_f2[2,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +

  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('B) Age at subfertility > 25') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

# Save plots
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data/Export this")
p_forest_S6 <- grid.arrange(p_lowf_S6, p_highf_S6, ncol=2)
ggsave("forest_S6.jpg",p_forest_S6, dpi=1000, width=16,height=6,unit="in")
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")

# Figure S7 #####
# Mediator analysis
p_preg_S7 <- ggplot() +
  geom_text(aes(x = 0.175, y = 16),
            label = 'Fertile', fontface = 'bold') +
  geom_text(aes(x = 0.45, y = 16),
            label = 'Subfertile', fontface = 'bold') +
  geom_text(aes(x = -0.22, y = 16),
            label = 'Outcomes', hjust = 'inward') +
  geom_text(aes(x = 0.31125, y = 16.6),
            label = 'Events / person years') +
  geom_text(aes(x = 1.71, y = 16),
            label = 'HR (95% CI)', hjust = 'inward') +
  # Stroke
  geom_text(aes(x = -0.22, y = 15,
                label = 'Stroke'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 15,
                label = paste(length(imp$data[imp$data$status_strok == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_strok[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Model 1
  geom_point(aes(x = strok_preg_f2[1,2], y = 14),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_preg_f2[1,7],
                     xmax = strok_preg_f2[1,8],
                     y = 14)) +
  geom_text(aes(x = -0.22, y = 14, label = 'Model 1'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 14,
                label = paste(sprintf('%.2f', strok_preg_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_preg_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_preg_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Model 2
  geom_point(aes(x = strok_preg_f3[1,2], y = 13),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = strok_preg_f3[1,7],
                     xmax = 1.4,
                     y = 13)) +
  geom_segment(aes(x = 1.35, y = 13, xend = 1.40, yend = 13),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 13, label = 'Model 2'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 13,
                label = paste(sprintf('%.2f', strok_preg_f3[1,2]),
                              paste(paste('(', sprintf('%.2f', strok_preg_f3[1,7]), sep = ""),
                                    paste(sprintf('%.2f', strok_preg_f3[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Coronary heart disease
  geom_text(aes(x = -0.22, y = 12,
                label = 'CHD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 12,
                label = paste(length(imp$data[imp$data$status_chd == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_chd[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Model 1
  geom_point(aes(x = chd_preg_f2[1,2], y = 11),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_preg_f2[1,7],
                     xmax = chd_preg_f2[1,8],
                     y = 11)) +
  geom_text(aes(x = -0.22, y = 11, label = 'Model 1'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 11,
                label = paste(sprintf('%.2f', chd_preg_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_preg_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_preg_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Model 2
  geom_point(aes(x = chd_preg_f3[1,2], y = 10),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = chd_preg_f3[1,7],
                     xmax = chd_preg_f3[1,8],
                     y = 10)) +
  geom_text(aes(x = -0.22, y = 10, label = 'Model 2'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 10,
                label = paste(sprintf('%.2f', chd_preg_f3[1,2]),
                              paste(paste('(', sprintf('%.2f', chd_preg_f3[1,7]), sep = ""),
                                    paste(sprintf('%.2f', chd_preg_f3[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Myocardial infarction
  geom_text(aes(x = -0.22, y = 9,
                label = 'MI'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 9,
                label = paste(length(imp$data[imp$data$status_myinf == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_myinf[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Model 1
  geom_point(aes(x = myinf_preg_f2[1,2], y = 8),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_preg_f2[1,7],
                     xmax = myinf_preg_f2[1,8],
                     y = 8)) +
  geom_text(aes(x = -0.22, y = 8, label = 'Model 1'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 8,
                label = paste(sprintf('%.2f', myinf_preg_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_preg_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_preg_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Model 2
  geom_point(aes(x = myinf_preg_f3[1,2], y = 7),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = myinf_preg_f3[1,7],
                     xmax = myinf_preg_f3[1,8],
                     y = 7)) +
  geom_text(aes(x = -0.22, y = 7, label = 'Model 2'),
            hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 7,
                label = paste(sprintf('%.2f', myinf_preg_f3[1,2]),
                              paste(paste('(', sprintf('%.2f', myinf_preg_f3[1,7]), sep = ""),
                                    paste(sprintf('%.2f', myinf_preg_f3[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Angina
  geom_text(aes(x = -0.22, y = 6,
                label = 'Angina'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 6,
                label = paste(length(imp$data[imp$data$status_ang == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_ang[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Model 1
  geom_point(aes(x = ang_preg_f2[1,2], y = 5),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_preg_f2[1,7],
                     xmax = 1.40,
                     y = 5)) +
  geom_segment(aes(x = 1.35, y = 5, xend = 1.40, yend = 5),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 5,
                label = 'Model 1'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 5,
                label = paste(sprintf('%.2f', ang_preg_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_preg_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_preg_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Model 2
  geom_point(aes(x = ang_preg_f3[1,2], y = 4),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = ang_preg_f3[1,7],
                     xmax = 1.40,
                     y = 4)) +
  geom_segment(aes(x = 1.35, y = 4, xend = 1.40, yend = 4),
               arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_text(aes(x = -0.22, y = 4,
                label = 'Model 2'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 4,
                label = paste(sprintf('%.2f', ang_preg_f3[1,2]),
                              paste(paste('(', sprintf('%.2f', ang_preg_f3[1,7]), sep = ""),
                                    paste(sprintf('%.2f', ang_preg_f3[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Any CVD outcome
  geom_text(aes(x = -0.22, y = 3,
                label = 'Any CVD'), hjust = 'inward', fontface = 'bold') +
  geom_text(aes(x = 0.175, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'no' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'no' & imp$data$sex == 'F'])), big.mark = ','), sep =' / '))) +
  geom_text(aes(x = 0.45, y = 3,
                label = paste(length(imp$data[imp$data$status_tot == 1 & imp$data$fert == 'yes' & imp$data$sex == 'F',1]),
                              prettyNum(sprintf("%1.0f",sum(imp$data$os_yrs_tot[imp$data$fert == 'yes' & imp$data$sex == 'F'])), big.mark = ','), sep = ' / '))) +
  # Model 1
  geom_point(aes(x = tot_preg_f2[1,2], y = 2),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_preg_f2[1,7],
                     xmax = tot_preg_f2[1,8],
                     y = 2)) +
  geom_text(aes(x = -0.22, y = 2,
                label = 'Model 1'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 2,
                label = paste(sprintf('%.2f', tot_preg_f2[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_preg_f2[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_preg_f2[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  # Model 2
  geom_point(aes(x = tot_preg_f3[1,2], y = 1),
             shape = 15, size = 3) +
  geom_linerange(aes(xmin = tot_preg_f3[1,7],
                     xmax = tot_preg_f3[1,8],
                     y = 1)) +
  geom_text(aes(x = -0.22, y = 1,
                label = 'Model 2'), hjust = 'inward') +
  geom_text(aes(x = 1.71, y = 1,
                label = paste(sprintf('%.2f', tot_preg_f3[1,2]),
                              paste(paste('(', sprintf('%.2f', tot_preg_f3[1,7]), sep = ""),
                                    paste(sprintf('%.2f', tot_preg_f3[1,8]), ')', sep = ""),
                                    sep = ', '))), hjust = 'inward') +
  
  # Visual
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 15.5), linetype = 'dashed') +
  geom_segment(aes(x = -0.24, xend = 1.73, y = 15.5, yend = 15.5), colour = 'grey') +
  xlab('Hazard ratio (HR)') +
  ggtitle('A) Women') +
  scale_x_continuous(limits = c(-0.24,1.73), breaks = seq(0.6,1.4,0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,17), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'))

# Save plot
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data/Export this")
ggsave("forest_S7.jpg",p_preg_S7, dpi=1000, width=8,height=6,unit="in")
setwd("N:/data/durable/projects/KASK_Infertility_CVD/Data")
