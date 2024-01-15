# Script to develop all Cancer/Age/Gender/Dataset permutations
library(tidyverse)

analysesRunList = read.csv("./data/CPRD_GOLD_analyses_run_summary.csv") %>%
  filter(Adjustment=="None") %>%
  select(-Adjustment) %>%
  mutate(Cancer=replace(Cancer,Cancer=="Head and Neck", "Head_and_neck")) 

prostate_df0 <- analysesRunList %>% filter(Cancer!="Prostate")
# recode prostate, males
prostate_df1 <- analysesRunList %>% filter(Cancer=="Prostate") %>%
  mutate(Run = replace(Run, Stratification=="Age", "No"))
# add prostate, females
prostate_df2 <- analysesRunList %>% filter(Cancer=="Prostate") %>%
  mutate(Sex="Female", Run="No")
# add prostate, both
prostate_df3 <- analysesRunList %>% filter(Cancer=="Prostate") %>%
  mutate(Sex="Both", Run="Yes")

analysesRunList <- rbind(prostate_df0,prostate_df1,prostate_df2,prostate_df3)
rm(prostate_df0,prostate_df1,prostate_df2,prostate_df3)


# Old code
#surv_df_all = read.csv("./data/survAvgCprd.csv")
#cancers <- unique(surv_df_all$Cancer)
#ages <- unique(surv_df_all$Age)
#genders <- unique(surv_df_all$Gender)
#datasets <- unique(surv_df_all$Database)
#plotCombinations <- expand.grid(Cancer=cancers,Age=ages,
#                                Gender=genders,Database=datasets) %>%
#  mutate(Render=1,
#         Render=ifelse(Cancer=="ProstateCancer" & Gender=="Female",0,Render))

