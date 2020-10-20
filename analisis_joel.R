rm(list=ls());gc()
load("G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/9.Rdata")


#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#¿cómo determinar qué tratamiento fue el más largo?
CONS_C1_JUN_2020_row_sig_row<-
  CONS_C1_df_dup_JUN_2020%>%
  #dplyr::filter(!is.na(diff_bet_treat))%>% #31,609, no hay NAs en derivación, los que son NA son 0.
  dplyr::mutate(filter_complex= dplyr::case_when(!is.na(diff_bet_treat) & diff_bet_treat<45 & as.character(motivoegreso_derivacion)=="Referral"~1,TRUE~0))%>%
  dplyr::arrange(hash_key)%>%
  dplyr::group_by(hash_key)%>%
  dplyr::mutate(sig_row=lag(row), sig_fech_egres_imp=lag(fech_egres_imp),sig_motivoegres_ref=lag(motivoegreso_derivacion),n_por_hash=n())%>%
  ungroup()%>%
  dplyr::filter(filter_complex==1)%>%
  # dplyr::select(row,hash_key,fech_ing,fech_egres_imp,motivoegreso_derivacion,obs_cambios,diff_bet_treat,sig_row,n_por_hash) %>% View() #0007678b8b35fa0961d1e8110fbf9620 
  dplyr::select(row,sig_row)  

CONS_C1_JUN_2020_cont_treat<-
  CONS_C1_df_dup_JUN_2020%>%
  dplyr::filter(row %in% unlist(c(CONS_C1_JUN_2020_row_sig_row$row,CONS_C1_JUN_2020_row_sig_row$sig_row)))%>%
  dplyr::arrange(hash_key,desc(fech_ing))%>%
  dplyr::mutate(filter_complex_anterior= dplyr::case_when(!is.na(lag(diff_bet_treat)) & lag(diff_bet_treat)<45 & as.character(lag(motivoegreso_derivacion))=="Referral"~1,TRUE~0))%>%
  dplyr::mutate(filter_complex= dplyr::case_when(!is.na(diff_bet_treat) & diff_bet_treat<45 & as.character(motivoegreso_derivacion)=="Referral"~1,TRUE~0))%>%
  dplyr::ungroup() %>% 
  dplyr::group_by(hash_key)%>%
  dplyr::slice_max(dias_trat) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(row,hash_key,dias_trat,cat_ocupacional,estatus_ocupacional) %>% 
  dplyr::mutate(row_larg_treat=row)

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

CONS_C1_JUN_2020_cont_treat %>%  janitor::tabyl(cat_ocupacional,estatus_ocupacional)    

#Ver qué row corresponde al tratamiento más largo
CONS_C1_df_dup_SEP_2020 %>% 
  #dplyr::mutate(row=as.character(row)) %>%
  dplyr::mutate(row_cont_entries2=row_cont_entries) %>% 
  tidyr::separate(row_cont_entries2,into=paste0("rows",1:7),sep="; ", convert=T) %>% #se supone que tiene 7 distintos
  dplyr::left_join(dplyr::select(CONS_C1,row,Condicion.Ocupacional),by="row")%>% 
  dplyr::rename("condicion_ocupacional_row_all"="Condicion.Ocupacional") %>% 
  dplyr::left_join(dplyr::select(CONS_C1,row,Condicion.Ocupacional),by=c("rows1"="row"))%>% 
  dplyr::rename("condicion_ocupacional_row1"="Condicion.Ocupacional") %>% 
  dplyr::left_join(dplyr::select(CONS_C1,row,Condicion.Ocupacional),by=c("rows2"="row"))%>% 
  dplyr::rename("condicion_ocupacional_row2"="Condicion.Ocupacional") %>% 
  dplyr::left_join(dplyr::select(CONS_C1,row,Condicion.Ocupacional),by=c("rows3"="row"))%>% 
  dplyr::rename("condicion_ocupacional_row3"="Condicion.Ocupacional") %>% 
  dplyr::left_join(dplyr::select(CONS_C1,row,Condicion.Ocupacional),by=c("rows4"="row"))%>% 
  dplyr::rename("condicion_ocupacional_row4"="Condicion.Ocupacional") %>% 
  dplyr::left_join(dplyr::select(CONS_C1,row,Condicion.Ocupacional),by=c("rows5"="row"))%>% 
  dplyr::rename("condicion_ocupacional_row5"="Condicion.Ocupacional") %>% 
  dplyr::left_join(dplyr::select(CONS_C1,row,Condicion.Ocupacional),by=c("rows6"="row"))%>% 
  dplyr::rename("condicion_ocupacional_row6"="Condicion.Ocupacional") %>% 
  dplyr::left_join(dplyr::select(CONS_C1,row,Condicion.Ocupacional),by=c("rows7"="row"))%>% 
  dplyr::rename("condicion_ocupacional_row7"="Condicion.Ocupacional") %>% 
  
  dplyr::left_join(CONS_C1_JUN_2020_cont_treat[,c("row","row_larg_treat")],by="row")%>% 
  dplyr::mutate(condicion_ocupacional_larg_treat=dplyr::case_when(row_larg_treat==rows1~condicion_ocupacional_row1,
                                                                  row_larg_treat==rows2~condicion_ocupacional_row2,
                                                                  row_larg_treat==rows3~condicion_ocupacional_row3,
                                                                  row_larg_treat==rows4~condicion_ocupacional_row4,
                                                                  row_larg_treat==rows5~condicion_ocupacional_row5,
                                                                  row_larg_treat==rows6~condicion_ocupacional_row6,
                                                                  row_larg_treat==rows7~condicion_ocupacional_row7)) %>% 
  dplyr::mutate(condicion_ocupacional_row_all2=ifelse(!is.na(condicion_ocupacional_larg_treat),
                                                     condicion_ocupacional_larg_treat,condicion_ocupacional_row_all)) %>% 
  dplyr::select(-condicion_ocupacional_row1,-condicion_ocupacional_row2,-condicion_ocupacional_row3,-condicion_ocupacional_row4,
                -condicion_ocupacional_row5,-condicion_ocupacional_row6,-condicion_ocupacional_row7,-rows1,-rows2,-rows3,-rows4,
                -rows5,-rows6,-rows7,-row_larg_treat) %>% 
  assign("CONS_C1_df_dup_SEP_2020_joel",., envir=.GlobalEnv)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
prueba<-
CONS_C1_df_dup_SEP_2020_joel %>% 
dplyr::group_by(hash_key) %>% 
  dplyr::mutate(menor_edad=dplyr::case_when(edad_al_ing<18~1,TRUE~0),menor_edad=sum(menor_edad,na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(edad_al_ing>=18) %>% 
  dplyr::group_by(hash_key) %>% 
  dplyr::mutate(dup2=row_number()) %>% 
  dplyr::mutate(duplicates_filtered2=n()) %>% 
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#Formatear días de tratamiento y diferencia en días excliyendo a los casos a los que le borramos un tratamiento menor a 18.
  dplyr::ungroup() %>% 
  dplyr::filter(dup2==1) %>%   #84,936 x 254
  dplyr::mutate(cum_dias_trat_sin_na_1= dplyr::case_when(menor_edad>0~cum_dias_trat_sin_na_2,TRUE~cum_dias_trat_sin_na_1)) %>% 
  dplyr::mutate(cum_dias_trat_sin_na_2= dplyr::case_when(menor_edad>0~cum_dias_trat_sin_na_3,TRUE~cum_dias_trat_sin_na_2)) %>% 
  dplyr::mutate(cum_dias_trat_sin_na_3= dplyr::case_when(menor_edad>0~cum_dias_trat_sin_na_4,TRUE~cum_dias_trat_sin_na_3)) %>% 
  dplyr::mutate(cum_dias_trat_sin_na_4= dplyr::case_when(menor_edad>0~cum_dias_trat_sin_na_5,TRUE~cum_dias_trat_sin_na_4)) %>% 
  dplyr::mutate(cum_dias_trat_sin_na_5= dplyr::case_when(menor_edad>0~cum_dias_trat_sin_na_6,TRUE~cum_dias_trat_sin_na_5)) %>% 
  dplyr::mutate(cum_dias_trat_sin_na_6= dplyr::case_when(menor_edad>0~cum_dias_trat_sin_na_7,TRUE~cum_dias_trat_sin_na_6)) %>% 
  dplyr::mutate(cum_dias_trat_sin_na_7= dplyr::case_when(menor_edad>0~cum_dias_trat_sin_na_8,TRUE~cum_dias_trat_sin_na_7)) %>% 
  dplyr::mutate(cum_dias_trat_sin_na_8= dplyr::case_when(menor_edad>0~cum_dias_trat_sin_na_9,TRUE~cum_dias_trat_sin_na_8)) %>% 
  dplyr::mutate(cum_dias_trat_sin_na_9= dplyr::case_when(menor_edad>0~cum_dias_trat_sin_na_10,TRUE~cum_dias_trat_sin_na_9)) %>% 
  dplyr::mutate(cum_dias_trat_sin_na_10= dplyr::case_when(menor_edad>0~NA_real_,TRUE~cum_dias_trat_sin_na_10)) %>% 
  
  dplyr::mutate(cum_diff_bet_treat_1= dplyr::case_when(menor_edad>0~cum_diff_bet_treat_2,TRUE~cum_diff_bet_treat_1)) %>% 
  dplyr::mutate(cum_diff_bet_treat_2= dplyr::case_when(menor_edad>0~cum_diff_bet_treat_3,TRUE~cum_diff_bet_treat_2)) %>% 
  dplyr::mutate(cum_diff_bet_treat_3= dplyr::case_when(menor_edad>0~cum_diff_bet_treat_4,TRUE~cum_diff_bet_treat_3)) %>% 
  dplyr::mutate(cum_diff_bet_treat_4= dplyr::case_when(menor_edad>0~cum_diff_bet_treat_5,TRUE~cum_diff_bet_treat_4)) %>% 
  dplyr::mutate(cum_diff_bet_treat_5= dplyr::case_when(menor_edad>0~cum_diff_bet_treat_6,TRUE~cum_diff_bet_treat_5)) %>% 
  dplyr::mutate(cum_diff_bet_treat_6= dplyr::case_when(menor_edad>0~cum_diff_bet_treat_7,TRUE~cum_diff_bet_treat_6)) %>% 
  dplyr::mutate(cum_diff_bet_treat_7= dplyr::case_when(menor_edad>0~cum_diff_bet_treat_8,TRUE~cum_diff_bet_treat_7)) %>% 
  dplyr::mutate(cum_diff_bet_treat_8= dplyr::case_when(menor_edad>0~cum_diff_bet_treat_9,TRUE~cum_diff_bet_treat_8)) %>% 
  dplyr::mutate(cum_diff_bet_treat_9= dplyr::case_when(menor_edad>0~cum_diff_bet_treat_10,TRUE~cum_diff_bet_treat_9)) %>% 
  dplyr::mutate(cum_diff_bet_treat_10= dplyr::case_when(menor_edad>0~NA_real_,TRUE~cum_diff_bet_treat_10)) %>%
  dplyr::mutate(tipo_de_plan_2_mod=dplyr::case_when(grepl("PAB",tipo_de_plan_2)~"PAB",
                                                    grepl("PAI",tipo_de_plan_2)~"PAI",
                                                    grepl("PR",tipo_de_plan_2)~"PR",
                                                    TRUE~NA_character_)) %>% 
  dplyr::mutate(tipo_de_plan_2_mod=factor(tipo_de_plan_2_mod)) %>% 
  dplyr::select(-menor_edad) %>% 
  dplyr::mutate(no_group=1)

#prueba %>% janitor::tabyl(condicion_ocupacional_row_all2,estatus_ocupacional)

#    
#   
library(compareGroups)
table3 <- compareGroups::compareGroups(no_group ~ edad_al_ing_grupos+ sexo_2+ escolaridad_rec+ estado_conyugal_2+ edad_ini_sus_prin+ edad_al_ing+
                                         sus_ini_mod+ sus_ini_mod_mvv+ freq_cons_sus_prin+ via_adm_sus_prin_act+ con_quien_vive_rec+ 
                                         tipo_de_plan_2+ cat_ocupacional+ abandono_temprano+ dg_cie_10_rec+ dias_treat_imp_sin_na+
                                         cnt_diagnostico_trs_fisico+ cnt_otros_probl_at_sm_or+ tipo_de_plan_2_mod+
                                         
                                         cum_dias_trat_sin_na_1+ cum_dias_trat_sin_na_2+ cum_dias_trat_sin_na_3+ cum_dias_trat_sin_na_4+ cum_dias_trat_sin_na_5+ cum_dias_trat_sin_na_6+ 
                                         cum_dias_trat_sin_na_7+ cum_dias_trat_sin_na_8+ cum_dias_trat_sin_na_9+ cum_dias_trat_sin_na_10+ cum_diff_bet_treat_1+ cum_diff_bet_treat_2+ cum_diff_bet_treat_3+ cum_diff_bet_treat_4+ cum_diff_bet_treat_5+ cum_diff_bet_treat_6+ 
                                         cum_diff_bet_treat_7+ cum_diff_bet_treat_8+ cum_diff_bet_treat_9+ cum_diff_bet_treat_10,
                                       method= c(edad_al_ing_grupos=3,
                                                 sexo_2=3,
                                                 escolaridad_rec=3,
                                                 estado_conyugal_2=3,
                                                 edad_ini_sus_prin=2,
                                                 edad_al_ing=2,
                                                 sus_ini_mod=3,
                                                 sus_ini_mod_mvv=3,
                                                 freq_cons_sus_prin=3,
                                                 via_adm_sus_prin_act=3,
                                                 con_quien_vive_rec=3,
                                                 tipo_de_plan_2=3,
                                                 cat_ocupacional=3,
                                                 abandono_temprano=3,
                                                 dg_cie_10_rec=3,
                                                 dias_treat_imp_sin_na=2,
                                                 cnt_mod_cie_10_or=2,
                                                 cnt_diagnostico_trs_fisico=2,
                                                 cnt_otros_probl_at_sm_or=2,
                                                 tipo_de_plan_2_mod=3,
                                                 cum_dias_trat_sin_na_1= 2,
                                                 cum_dias_trat_sin_na_2= 2, 
                                                 cum_dias_trat_sin_na_3= 2, 
                                                 cjaum_dias_trat_sin_na_4= 2, 
                                                 cum_dias_trat_sin_na_5= 2, 
                                                 cum_dias_trat_sin_na_6= 2, 
                                                 cum_dias_trat_sin_na_7= 2, 
                                                 cum_dias_trat_sin_na_8= 2, 
                                                 cum_dias_trat_sin_na_9= 2, 
                                                 cum_dias_trat_sin_na_10=2, 
                                                 cum_diff_bet_treat_1= 2, 
                                                 cum_diff_bet_treat_2= 2, 
                                                 cum_diff_bet_treat_3= 2, 
                                                 cum_diff_bet_treat_4= 2, 
                                                 cum_diff_bet_treat_5= 2, 
                                                 cum_diff_bet_treat_6= 2, 
                                                 cum_diff_bet_treat_7= 2, 
                                                 cum_diff_bet_treat_8= 2, 
                                                 cum_diff_bet_treat_9= 2, 
                                                 cum_diff_bet_treat_10= 2
                                       ),
                                       data = prueba,
                                       include.miss = T,
                                       var.equal=T,
                                       
)#cie_10 cat_ocupacional estatus_ocupacional


pvals <- getResults(table3)
#p.adjust(pvals, method = "BH")
restab3 <- createTable(table3,show.p.overall = F)
compareGroups::export2md(restab3, size=9, first.strip=T, hide.no="no", position="center",col.names=c("Variables","Total"),
                         format="html",caption= "Table 1. Summary descriptives table")%>%
  kableExtra::add_footnote(c("Note. Variables of C1 dataset had to be standardized before comparison;", "Continuous variables are presented as Medians and Percentiles 25 and 75 were shown;", "Categorical variables are presented as number (%)"), notation = "none")%>%
  kableExtra::scroll_box(width = "100%", height = "375px")


table4 <- compareGroups::compareGroups(sus_principal_mod ~ edad_al_ing_grupos+ sexo_2+ escolaridad_rec+ estado_conyugal_2+ edad_ini_sus_prin+ edad_al_ing+
                                         sus_ini_mod+ sus_ini_mod_mvv+ freq_cons_sus_prin+ via_adm_sus_prin_act+ con_quien_vive_rec+ 
                                         tipo_de_plan_2+ cat_ocupacional+ abandono_temprano+ dg_cie_10_rec+ dias_treat_imp_sin_na+
                                         cnt_diagnostico_trs_fisico+ cnt_otros_probl_at_sm_or+ tipo_de_plan_2_mod+

                                        cum_dias_trat_sin_na_1+ cum_dias_trat_sin_na_2+ cum_dias_trat_sin_na_3+ cum_dias_trat_sin_na_4+ cum_dias_trat_sin_na_5+ cum_dias_trat_sin_na_6+ 
                                         cum_dias_trat_sin_na_7+ cum_dias_trat_sin_na_8+ cum_dias_trat_sin_na_9+ cum_dias_trat_sin_na_10+ cum_diff_bet_treat_1+ cum_diff_bet_treat_2+ cum_diff_bet_treat_3+ cum_diff_bet_treat_4+ cum_diff_bet_treat_5+ cum_diff_bet_treat_6+ 
                                         cum_diff_bet_treat_7+ cum_diff_bet_treat_8+ cum_diff_bet_treat_9+ cum_diff_bet_treat_10,
                                       method= c(edad_al_ing_grupos=3,
                                                 sexo_2=3,
                                                 escolaridad_rec=3,
                                                 estado_conyugal_2=3,
                                                 edad_ini_sus_prin=2,
                                                 edad_al_ing=2,
                                                 sus_ini_mod=3,
                                                 sus_ini_mod_mvv=3,
                                                 freq_cons_sus_prin=3,
                                                 via_adm_sus_prin_act=3,
                                                 con_quien_vive_rec=3,
                                                 tipo_de_plan_2=3,
                                                 cat_ocupacional=3,
                                                 abandono_temprano=3,
                                                 dg_cie_10_rec=3,
                                                 dias_treat_imp_sin_na=2,
                                                 cnt_mod_cie_10_or=2,
                                                 cnt_diagnostico_trs_fisico=2,
                                                 cnt_otros_probl_at_sm_or=2,
                                                 tipo_de_plan_2_mod=3,
                                                 cum_dias_trat_sin_na_1= 2,
                                                 cum_dias_trat_sin_na_2= 2, 
                                                 cum_dias_trat_sin_na_3= 2, 
                                                 cjaum_dias_trat_sin_na_4= 2, 
                                                 cum_dias_trat_sin_na_5= 2, 
                                                 cum_dias_trat_sin_na_6= 2, 
                                                 cum_dias_trat_sin_na_7= 2, 
                                                 cum_dias_trat_sin_na_8= 2, 
                                                 cum_dias_trat_sin_na_9= 2, 
                                                 cum_dias_trat_sin_na_10=2, 
                                                 cum_diff_bet_treat_1= 2, 
                                                 cum_diff_bet_treat_2= 2, 
                                                 cum_diff_bet_treat_3= 2, 
                                                 cum_diff_bet_treat_4= 2, 
                                                 cum_diff_bet_treat_5= 2, 
                                                 cum_diff_bet_treat_6= 2, 
                                                 cum_diff_bet_treat_7= 2, 
                                                 cum_diff_bet_treat_8= 2, 
                                                 cum_diff_bet_treat_9= 2, 
                                                 cum_diff_bet_treat_10= 2
                                                 ),
                                       data = prueba,
                                       include.miss = T,
                                       var.equal=T
)#cie_10 cat_ocupacional estatus_ocupacional


pvals <- getResults(table4)
#p.adjust(pvals, method = "BH")
restab4 <- createTable(table4, show.p.overall = T)
compareGroups::export2md(restab4, size=9, first.strip=T, hide.no="no", position="center",
                         format="html",caption= "Table 2. Summary descriptives table by Primary Substance at Admission")%>%
  kableExtra::add_footnote(c("Note. Variables of C1 dataset had to be standardized before comparison;", "Continuous variables are presented as Medians and Percentiles 25 and 75 were shown;", "Categorical variables are presented as number (%)"), notation = "none")%>%
  kableExtra::scroll_box(width = "100%", height = "375px")