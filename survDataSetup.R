# Script to load, format and save survival extrapolation data 
# to be loaded into the dashboard

# Specify a set of intervals to take subset of K-M observations
monthIntervals <- data.frame(time2=seq(0,25,1/52)) %>% 
  mutate(matchid=row_number())

# For extrapolation and hazard data, timepoints are matched to nearest 
# specified interval

# Goodness of fit statistics
fitCprd_df = readRDS("./data/Goodness_of_fit_results_CPRD_Aurum.rds") %>%
  mutate(trisk=round(trisk,0),logLik=round(logLik,3),
         AIC=round(AIC,3),BIC=round(BIC,3)) %>%
  select(-Stratification,-GenderAge)

# Survival curves
SurvCprd_df = readRDS("./data/survival_estimates_CPRD_Aurum.rds") %>%
  mutate(Method=ifelse(Method=="Kaplan-Meier","Observed",Method),
         Age1=sub("_.*","",GenderAge),
         Gender1=sub(".*_","",GenderAge),
         Age=ifelse(!is.na(GenderAge),Age1,Age),
         Gender=ifelse(!is.na(GenderAge),Gender1,Gender)) %>%
  # Taking the subset of observations specified in monthIntervals
  rowwise() %>%
  mutate(matchid=which.min(abs(monthIntervals$time2-time))) %>%
  left_join(monthIntervals,by="matchid") %>%
  mutate(absDiff=abs(time2-time)) %>% group_by(Method,Cancer,Age,Gender,time2) %>%
  filter(absDiff==min(absDiff)) %>%
  mutate(time=round(time2,3),est=round(est,5),lcl=round(lcl,5),ucl=round(ucl,5)) %>%
  ungroup() %>%
  select(-Stratification,-GenderAge,-matchid,-time2,-absDiff) 

# Hazard rate over time
rateHazCprd_df = readRDS("./data/hazard_overtime_results_CPRD_Aurum.rds") %>%
  mutate(Method=ifelse(Method=="Kaplan-Meier","Observed",Method),
         Age1=sub("_.*","",GenderAge),
         Gender1=sub(".*_","",GenderAge),
         Age=ifelse(!is.na(GenderAge),Age1,Age),
         Gender=ifelse(!is.na(GenderAge),Gender1,Gender)) %>%
  # Taking the subset of observations specified in monthIntervals
  rowwise() %>%
  mutate(matchid=which.min(abs(monthIntervals$time2-time))) %>%
  left_join(monthIntervals,by="matchid") %>%
  mutate(absDiff=abs(time2-time)) %>% group_by(Method,Cancer,Age,Gender,time2) %>%
  filter(absDiff==min(absDiff)) %>%
  mutate(time=round(time2,3),est=round(est,5),lcl=round(lcl,5),ucl=round(ucl,5)) %>%
  ungroup() %>%
  select(-Stratification,-GenderAge,-matchid,-time2,-absDiff) 

# Mean/median survival estimates
survAvgCprd_df = readRDS("./data/median_survival_results_CPRD_Aurum.rds") %>%
  mutate(meanSurv=round(`*rmean`,3),medianSurv=round(median,3)) %>%
  select(Cancer,Age,Gender,meanSurv,medianSurv,Database)

# Number at risk summary table
riskTableCprd_df = readRDS("./data/risk_table_results_CPRD_Aurum.rds") %>%
  select(-Method,-Stratification,-GenderAge)
rownames(riskTableCprd_df) <- NULL

# Write all formatted csv data files
saveRDS(SurvCprd_df,"./data/survCprd.rds")
saveRDS(rateHazCprd_df,"./data/rateHazCprd.rds")
saveRDS(fitCprd_df,"./data/fitCprd.rds")
saveRDS(survAvgCprd_df,"./data/survAvgCprd.rds")
saveRDS(riskTableCprd_df,"./data/riskTableCprd.rds")