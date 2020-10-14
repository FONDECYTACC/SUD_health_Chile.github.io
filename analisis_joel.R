rm(list=ls());gc()
load("G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/9.Rdata")

prueba<-
  CONS_C1_df_dup_SEP_2020 %>%
  dplyr::filter(edad_al_ing>=18) %>% 
  dplyr::group_by(hash_key) %>% 
  dplyr::mutate(dup2=row_number()) %>% 
  dplyr::mutate(duplicates_filtered2=n()) %>% 
  
  dplyr::ungroup() %>% 
  dplyr::filter(dup2==1)  #84,936 x 254

library(compareGroups)
table2 <- compareGroups::compareGroups(sexo_2 ~ con_quien_vive_rec+ tipo_de_programa_2+ escolaridad_rec+ estado_conyugal_2+ numero_de_hijos_mod+ hijos_trat_res+ embarazo+ 
                                         sus_principal_mod+  otras_sus1_mod+ edad_ini_sus_prin_grupos+  freq_cons_sus_prin+  dg_cie_10_rec+  compromiso_biopsicosocial+  
                                         tipo_de_plan_2+  tipo_centro+  macrozona+  tenencia_de_la_vivienda_mod,
                                       method= c(edad_al_ing_grupos=3,
                                                 escolaridad_rec=3,
                                                 estado_conyugal_2=3,
                                                 numero_de_hijos_mod=2,
                                                 hijos_trat_res=3,
                                                 embarazo=3,
                                                 sus_principal_mod=3,
                                                 otras_sus1_mod=3,
                                                 edad_ini_sus_prin_grupos=3,
                                                 freq_cons_sus_prin=3,
                                                 dg_cie_10_rec=3,
                                                 compromiso_biopsicosocial=3,
                                                 tipo_de_plan_2=3,
                                                 tipo_centro=3,
                                                 macrozona=3,
                                                 tenencia_de_la_vivienda_mod=3),
                                       data = prueba,
                                       include.miss = T,
                                       var.equal=T
)#cie_10 cat_ocupacional estatus_ocupacional


pvals <- getResults(table2)
#p.adjust(pvals, method = "BH")
restab2 <- createTable(table2, show.p.overall = T)
compareGroups::export2md(restab2, size=9, first.strip=T, hide.no="no", position="center",
                         format="html",caption= "Table 1. Summary descriptives table by groups, between selected rows (=1) and records that did not matched, duplicate treatments and invalid ones (=0)")%>%
  kableExtra::add_footnote(c("Note. Variables of C1 dataset had to be standardized before comparison;", "Continuous variables are presented as Medians and Percentiles 25 and 75 were shown;", "Categorical variables are presented as number (%)"), notation = "none")%>%
  kableExtra::scroll_box(width = "100%", height = "375px")