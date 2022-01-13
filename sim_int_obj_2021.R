rbindlist(summary(total_fits_genf_obj, "rmst", 
                  newdata=expand.grid(motivodeegreso_mod_imp=levels(CONS_C1_df_dup_SEP_2020_irrs_health_survreg$motivodeegreso_mod_imp)[1:5], 
                                      sexo_2=levels(CONS_C1_df_dup_SEP_2020_irrs_health_survreg$sexo_2),
                                      edad_grupos=levels(CONS_C1_df_dup_SEP_2020_irrs_health_survreg$edad_grupos),
                                      sus_principal_mod=levels(CONS_C1_df_dup_SEP_2020_irrs_health_survreg$sus_principal_mod),
                                      sus_ini_mod=levels(CONS_C1_df_dup_SEP_2020_irrs_health_survreg$sus_ini_mod)), B=1e4, t=c(.25,1,3,5)), 
          idcol="Total") %>% 
  tidyr::separate(`Treatment Outcome`,into=c("Treatment Outcome","Sex", "Age","Primary Substance","Initial Substance"),sep=", ") %>% 
  dplyr::mutate(`Treatment Outcome`=factor(gsub("motivodeegreso_mod_imp\\=","",`Treatment Outcome`))) %>% 
  dplyr::mutate(Sex=factor(gsub("sexo_2\\=","",Sex))) %>% 
  dplyr::mutate(Age=factor(gsub("edad_grupos\\=","",Age))) %>% 
  dplyr::mutate(`Primary Substance`=factor(gsub("sus_principal_mod\\=","",`Primary Substance`))) %>% 
  dplyr::mutate(`Initial Substance`=factor(gsub("sus_ini_mod\\=","",`Initial Substance`))) %>% 