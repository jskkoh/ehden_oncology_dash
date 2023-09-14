# Script to develop all Cancer/Age/Gender/Dataset permutations
library(tidyverse)

surv_df_all = readRDS("./data/survCprd.rds") 
cancers <- unique(surv_df_all$Cancer)
ages <- unique(surv_df_all$Age)
genders <- unique(surv_df_all$Gender)
datasets <- unique(surv_df_all$Database)

plotCombinations <- expand.grid(Cancer=cancers,Age=ages,
                                Gender=genders,Database=datasets) %>%
  mutate(Render=1,
         Render=ifelse(Cancer=="ProstateCancer" & Gender=="Female",0,Render))

