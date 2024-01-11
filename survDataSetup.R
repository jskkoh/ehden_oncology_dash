# Script to load, format and save survival extrapolation data 
# to be loaded into the dashboard

# Specify a set of intervals to take subset of K-M observations
monthIntervals <- data.frame(time2=seq(0,25,1/18)) %>% 
  mutate(matchid=row_number())

# For extrapolation and hazard data, timepoints are matched to nearest 
# specified interval

# Goodness of fit statistics
fitCprd_df = read.csv("./data/CPRD_GOLD_goodness_of_fit.csv") %>%
  filter((Stratification=="None" & Adjustment=="None") |
           (Stratification=="Sex" & Adjustment=="None") |
           (Stratification=="Age" & Adjustment=="None")) %>%
  mutate(trisk=round(trisk,0),logLik=round(logLik,3),
         AIC=round(AIC,3),BIC=round(BIC,3)) %>%
  select(-Stratification,-Adjustment)

# Survival curves
SurvCprd_df = read.csv("./data/CPRD_GOLD_survival_estimates.csv") %>%
  filter((Stratification=="None" & Adjustment=="None") |
           (Stratification=="Sex" & Adjustment=="None") |
           (Stratification=="Age" & Adjustment=="None")) %>%
  mutate(Method=ifelse(Method=="Kaplan-Meier","Observed",Method)) 
  #%>%
  ### Taking the subset of observations specified in monthIntervals
  #rowwise() %>%
  #mutate(matchid=which.min(abs(monthIntervals$time2-time))) %>%
  #left_join(monthIntervals,by="matchid") %>%
  #mutate(absDiff=abs(time2-time)) %>% group_by(Method,Cancer,Age,Sex,time2) %>%
  #filter(absDiff==min(absDiff)) %>%
  #mutate(time=round(time2,3),est=round(est,5),lcl=round(lcl,5),ucl=round(ucl,5)) %>%
  #ungroup() %>%
  #select(-Stratification,-Adjustment,-matchid,-time2,-absDiff) 

# Hazard rate over time
rateHazCprd_df = read.csv("./data/CPRD_GOLD_hazard_overtime.csv") %>%
  filter((Stratification=="None" & Adjustment=="None") |
           (Stratification=="Sex" & Adjustment=="None") |
           (Stratification=="Age" & Adjustment=="None")) %>%
  mutate(Method=ifelse(Method=="Kaplan-Meier","Observed",Method)) 
#%>%
  ### Taking the subset of observations specified in monthIntervals
  #rowwise() %>%
  #mutate(matchid=which.min(abs(monthIntervals$time2-time))) %>%
  #left_join(monthIntervals,by="matchid") %>%
  #mutate(absDiff=abs(time2-time)) %>% group_by(Method,Cancer,Age,Sex,time2) %>%
  #filter(absDiff==min(absDiff)) %>%
  #mutate(time=round(time2,3),est=round(est,5),lcl=round(lcl,5),ucl=round(ucl,5)) %>%
  #ungroup() %>%
  #select(-Stratification,-Adjustment,-matchid,-time2,-absDiff) 

# Mean/median survival estimates
survAvgCprd_df = read.csv("./data/CPRD_GOLD_median_mean_survprob_survival.csv") %>%
  filter(((Stratification=="None" & Adjustment=="None") |
           (Stratification=="Sex" & Adjustment=="None") |
           (Stratification=="Age" & Adjustment=="None")) &
           Method=="Kaplan-Meier") %>%
  mutate(meanSurv=round(`rmean`,3),medianSurv=round(median,3)) %>%
  select(Cancer,Age,Sex,meanSurv,medianSurv,Database)

# Number at risk summary table
riskTableCprd_df = read.csv("./data/CPRD_GOLD_risk_table.csv") %>%
  filter((Stratification=="None" & Adjustment=="None") |
           (Stratification=="Sex" & Adjustment=="None") |
           (Stratification=="Age" & Adjustment=="None")) %>%
  select(-Method,-Stratification,-Adjustment)
colnames(riskTableCprd_df)[2:23] <- c(0,0.5,seq(1,20,1))

# Write all formatted csv data files
write.csv(SurvCprd_df,"./data/survCprd.csv")
write.csv(rateHazCprd_df,"./data/rateHazCprd.csv")
write.csv(fitCprd_df,"./data/fitCprd.csv")
write.csv(survAvgCprd_df,"./data/survAvgCprd.csv")
write.csv(riskTableCprd_df,"./data/riskTableCprd.csv")