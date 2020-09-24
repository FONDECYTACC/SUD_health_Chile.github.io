library(epitools)
epitab_early_event  <-epitab(xtabs(person_days ~ event+ mot_egres_latewdl, 
                                   data = CONS_C1_df_dup_SEP_2020_prev4),
                             method="rateratio",
                             rateratio = "wald",
                             pvalue= "chi2",
                             rev = "both")



library(cmprsk)
CI.4vs5 <-cuminc(ftime=CONS_C1_df_dup_SEP_2020_prev4$person_days,
                 fstatus=CONS_C1_df_dup_SEP_2020_prev4$event,group=CONS_C1_df_dup_SEP_2020_prev4$mot_egres_latewdl)
#CI.4vs5$Tests

library(epiR)
#xtabs(event ~ mot_egres_earlywdl + person_years, data = CONS_C1_df_dup_SEP_2020_prev4)
rval<-epiR::epi.2by2(xtabs(person_days ~ event+ mot_egres_earlywdl, data = CONS_C1_df_dup_SEP_2020_prev4), method = "cohort.count", conf.level = 0.95, outcome= "as.columns")
