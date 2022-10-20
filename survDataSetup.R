# Script to load, format and save survival extrapolation data 
# to be loaded into the dashboard

monthIntervals <- data.frame(time2=seq(0,25,1/6)) %>% 
  mutate(matchid=row_number())

# For extrapolation and hazard data, timepoints are matched to nearest 
# specified interval

# Survival extrapolation data
survExCprd_df = read_excel("./data/cancer_extrapolation_results_ALL.xlsx",
                           "extrapolation_all") %>% rowwise() %>%
  mutate(matchid=which.min(abs(monthIntervals$time2-time))) %>%
  left_join(monthIntervals,by="matchid") %>%
  mutate(absDiff=abs(time2-time)) %>% group_by(Method,Cancer,Age,Gender,time2) %>%
  filter(absDiff==min(absDiff)) %>%
  mutate(time=round(time2,3),est=round(est,5),lcl=round(lcl,5),ucl=round(ucl,5))

# Hazard data
rateHazCprd_df = read_excel("./data/cancer_extrapolation_results_ALL.xlsx",
                           "hazardrate_all") %>% rowwise() %>%
  mutate(matchid=which.min(abs(monthIntervals$time2-time))) %>%
  left_join(monthIntervals,by="matchid") %>%
  mutate(absDiff=abs(time2-time)) %>% group_by(Method,Cancer,Age,Gender,time2) %>%
  filter(absDiff==min(absDiff)) %>%
  mutate(time=round(time2,3),est=round(est,5),lcl=round(lcl,5),ucl=round(ucl,5))

# Goodness of fit statistics
fitCprd_df = read_excel("./data/cancer_extrapolation_results_ALL.xlsx","GOF_all") %>%
  mutate(trisk=round(trisk,0),logLik=round(logLik,3),AIC=round(AIC,3),BIC=round(BIC,3))

# KM survival
kmCprd_df = read_excel("./data/cancer_KM_observed_results_ALL.xlsx","KM_observed_all") %>%
  mutate(time=round(time,3),est=round(est,5),lcl=round(lcl,5),ucl=round(ucl,5)) %>%
  arrange(time)

# Observed hazard rates
rateHazObsCprd_df = read_excel("./data/cancer_KM_observed_results_ALL.xlsx",
                               "KM_hazard_rate_all") %>%
  mutate(time=round(time,3),est=round(est,5),lcl=round(lcl,5),ucl=round(ucl,5)) %>%
  arrange(time)

# Mean and median survival
survAvgCprd_df = read_excel("./data/cancer_KM_observed_results_ALL.xlsx","KM_MedianSur_all") %>%
  mutate(meanSurv=round(`*rmean`,3),medianSurv=round(median,3)) %>%
  select(Cancer,Age,Gender,meanSurv,medianSurv)

# Risk table
riskTableCprd_df = read_excel("./data/cancer_KM_observed_results_ALL.xlsx","KM_risktable_all") %>%
  select(-Method)

# Write all formatted csv data files
write.csv(survExCprd_df,"./data/survExCprd.csv",row.names=F)
write.csv(rateHazCprd_df,"./data/rateHazCprd.csv",row.names=F)
write.csv(fitCprd_df,"./data/fitCprd.csv",row.names=F)
write.csv(kmCprd_df,"./data/kmCprd.csv",row.names=F)
write.csv(rateHazObsCprd_df,"./data/rateHazObsCprd.csv",row.names=F)
write.csv(survAvgCprd_df,"./data/survAvgCprd.csv",row.names=F)
write.csv(riskTableCprd_df,"./data/riskTableCprd.csv",row.names=F)