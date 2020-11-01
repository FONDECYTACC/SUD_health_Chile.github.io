*final
use "G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/CONS_C1_df_dup_SEP_2020.dta", clear
cap label variable row "Numerador de los eventos presentes en la Base de Datos (Último registro)/Events in the Dataset (Last Entry)"
cap label variable row_cont_entries "Numerador de los eventos presentes en la Base de Datos(*)/Events in the Dataset(*)"
cap label variable hash_key "Codificación del RUT/Masked Identifier (RUT)"
cap label variable hash_rut_completo "HASH alternativo, en el escenario en que se asuma que el individuo al que se le codificó el RUT presente mayor edad/Alternative HASH-Key"
cap label variable id "Codigo Identificación de SENDA/SENDAs ID"
cap label variable id_mod "ID de SENDA para Presentación en Página Web (enmascara caracteres 5 y 6)/SENDAs ID (mask characters 5 & 6)"
cap label variable fech_ing "Fecha de Ingreso a Tratamiento (Primera Entrada)/Date of Admission to Treatment (First Entry)"
cap label variable fech_egres_imp "Fecha de Egreso (Imputados KNN & Lógico) del Último Registro(b)/Date of Discharge (Imputed KNN & Logic) of the Last Entry(b)"
cap label variable tipo_de_plan_2_largest_treat "Tipo de Plan del Registro Más Largo entre entradas intermedias(f)/Type of Plan of the Largest Entry Among Intermediate Entries(f)"
cap label variable tipo_de_plan_2_concat_a "Tipo de Plan(*)/Type of Plan(*)"
cap label variable tipo_de_programa_2 "Tipo de Programa del Registro Más Largo entre Entradas Intermedias/Type of Program of the Largest Entry Among Intermediate Entries"
cap label variable id_centro "ID de Centro(b)/Center ID(b)"
cap label variable nombre_centro "Nombre del Centro de Tratamiento(*)/Treatment Center(*)"
cap label variable id_centro_concat_a "ID de Centro(*)/Center ID(*)"
cap label variable tipo_centro "Tipo de Centro del Último Registro/Type of Center of the Last Entry"
cap label variable servicio_de_salud "Servicio de Salud(*)/Health Service(*)"
cap label variable senda "SENDA del Último Registro/SENDA of the Last Entry"
cap label variable numero_de_hijos_mod "Número de Hijos (Valor Max.)/Number of Children (Max. Value)"
cap label variable num_hijos_trat_res_mod "Número de Hijos para Ingreso a Tratamiento Residencial del Último Registro/Number of Children to Residential Treatment of the Last Entry"
cap label variable tipo_centro_derivacion "Tipo de Centro al que el Usuario es Derivado del Último Registro(b)/Type of Center of Derivation of the Last Entry(b)"
cap label variable macrozona "Macrozona del Centro del Último Registro(b)/Macrozones of the Center of the Last Entry(b)"
cap label variable nombre_region "Región del Centro del Último Registro(b)/Chilean Region of the Center of the Last Entry(b)"
cap label variable comuna_residencia_cod "Comuna de Residencia del Último Registro(b)/Municipality or District of Residence of the Last Entry(b)"
cap label variable fecha_ingreso_a_convenio_senda "Fecha de Ingreso a Convenio SENDA (aún no formateada como fecha) (Primera Entrada)/Date of Admission to SENDA Agreement (First Entry)"
cap label variable identidad_de_genero "Identidad de Género (Último Registro)(b)/Gender Identity (Last Entry)(b)"
cap label variable edad_al_ing "Edad a la Fecha de Ingreso a Tratamiento (numérico continuo) (Primera Entrada)/Age at Admission to Treatment (First Entry)"
cap label variable origen_ingreso_mod "Origen de Ingreso (Primera Entrada)/Motive of Admission to Treatment (First Entry)"
cap label variable x_se_trata_mujer_emb "Mujer Embarazada al Ingreso (d)/Pregnant at Admission (d)"
cap label variable compromiso_biopsicosocial "Compromiso Biopsicosocial(d)/Biopsychosocial Involvement(d)"
cap label variable dg_global_nec_int_soc_or "Diagnóstico Global de Necesidades de Integración Social (Al Ingreso)(d)/Global Diagnosis of Social Integration (At Admission)(d)"
cap label variable dg_nec_int_soc_cap_hum_or "Diagnóstico de Necesidades de Integración Social en Capital Humano (Al Ingreso)(d)/Global Diagnosis of Social Integration in Human Capital (At Admission)(d)"
cap label variable dg_nec_int_soc_cap_fis_or "Diagnóstico de Necesidades de Integración Social en Capital Físico (Al Ingreso)(d)/Global Diagnosis of Social Integration in Physical Capital (At Admission)(d)"
cap label variable dg_nec_int_soc_cap_soc_or "Diagnóstico de Necesidades de Integración Social en Capital Social (Al Ingreso)(d)/Global Diagnosis of Social Integration in Social Capital (At Admission)(d)"
cap label variable usuario_tribunal_trat_droga "Usuario de modalidad Tribunales de Tratamiento de Drogas(d)/User of Drug Treatment Courts Modality(d)"
cap label variable evaluacindelprocesoteraputico "Evaluación del Proceso Terapéutico(d)/Evaluation of the Therapeutic Process(d)"
cap label variable eva_consumo "Evaluación al Egreso Respecto al Patrón de consumo(d)/Evaluation at Discharge regarding to Consumption Pattern(d)"
cap label variable eva_fam "Evaluación al Egreso Respecto a Situación Familiar(d)/Evaluation at Discharge regarding to Family Situation(d)"
cap label variable eva_relinterp "Evaluación al Egreso Respecto a Relaciones Interpersonales(d)/Evaluation at Discharge regarding to Interpersonal Relations(d)"
cap label variable eva_ocupacion "Evaluación al Egreso Respecto a Situación Ocupacional(d)/Evaluation at Discharge regarding to Occupational Status(d)"
cap label variable eva_sm "Evaluación al Egreso Respecto a Salud Mental(d)/Evaluation at Discharge regarding to Mental Health(d)"
cap label variable eva_fisica "Evaluación al Egreso Respecto a Salud Física(d)/Evaluation at Discharge regarding to Physical Health(d)"
cap label variable eva_transgnorma "Evaluación al Egreso Respecto a Trasgresión a la Norma Social(d)/Evaluation at Discharge regarding to Transgression to the Norm(d)"
cap label variable dg_global_nec_int_soc_or_1 "Diagnóstico Global de Necesidades de Integración Social (Al Egreso)(d)/Global Diagnosis of Social Integration (At Discharge)(d)"
cap label variable dg_nec_int_soc_cap_hum_or_1 "Diagnóstico de Necesidades de Integración Social en Capital Humano (Al Egreso)(d)/Global Diagnosis of Social Integration in Human Capital (At Discharge)(d)"
cap label variable dg_nec_int_soc_cap_fis_or_1 "Diagnóstico de Necesidades de Integración Social en Capital Físico (Al Egreso)(d)/Global Diagnosis of Social Integration in Physical Capital (At Discharge)(d)"
cap label variable dg_nec_int_soc_cap_soc_or_1 "Diagnóstico de Necesidades de Integración Social en Capital Social (Al Egreso)(d)/Global Diagnosis of Social Integration in Social Capital (At Discharge)(d)"
cap label variable tiene_menores_de_edad_a_cargo "Menores de Edad A Cargo(d)/Minor Dependants(d)"
cap label variable ha_estado_embarazada_egreso "¿Ha estado embarazada? (al Egreso)(d)/Have you been Pregnant (at Discharge)(d)"
cap label variable discapacidad "Presenta Discapacidad(d)/Disability(d)"
cap label variable opcion_discapacidad "Origen de Discapacidad(d)/Cause of Disability(d)"
cap label variable escolaridad "Escolaridad: Nivel Eduacional(d)/Educational Attainment(d)"
cap label variable edad_al_ing_grupos "Edad a la Fecha de Ingreso a Tratamiento en Grupos(c)/Age at Admission to Treatment In Groups(c)"
cap label variable nacionalidad "Nacionalidad/Nationality"
cap label variable sexo_2 "Sexo Usuario/Sex of User"
cap label variable embarazo "Embarazo al Ingreso(c)/Pregnant at Admission(c)"
cap label variable fech_nac "Fecha de Nacimiento/Date of Birth"
cap label variable edad_ini_cons "Edad de Inicio de Consumo/Age of Onset of Drug Use"
cap label variable edad_ini_sus_prin "Edad de Inicio de Consumo Sustancia Principal/Age of Onset of Drug Use of Primary Substance"
cap label variable estado_conyugal_2 "Estado Conyugal/Marital Status"
cap label variable edad_grupos "Edad agrupada/Age in groups"
cap label variable freq_cons_sus_prin "Frecuencia de Consumo de la Sustancia Principal (30 días previos a la admisión)(f)/Frequency of Consumption of the Primary or Main Substance (30 days previous to admission)(f)"
cap label variable via_adm_sus_prin_act "Vía de Administración de la Sustancia Principal (Se aplicaron criterios de limpieza)(f)/Route of Administration of the Primary or Main Substance (Tidy)(f)"
cap label variable etnia_cor "Etnia/Ethnic Group"
cap label variable nacionalidad_2 "Segunda Nacionalidad/Second Nationality"
cap label variable etnia_cor_2 "Etnia (2)/Second Ethnic Group"
cap label variable sus_ini_2_mod "Segunda Sustancia de Inicio(Sólo más frecuentes)/Second Starting Substance"
cap label variable sus_ini_3_mod "Tercera Sustancia de Inicio(Sólo más frecuentes)/Third Starting Substance"
cap label variable sus_ini_mod "Sustancia de Inicio (Sólo más frecuentes)/Starting Substance (Only more frequent)"
cap label variable con_quien_vive "Persona con la que vive el Usuario(f)/People that Share Household with the User (Cohabitation Status)(f)"
cap label variable estatus_ocupacional "Condición Ocupacional(f)/Occupational Status(f)"
cap label variable cat_ocupacional "Categoría Ocupacional(f)/Occupational Category(f)"
cap label variable sus_principal_mod "Sustancia Principal de Consumo (Sólo más frecuentes)(f)/Primary or Main Substance of Consumption at Admission (Only more frequent)(f)"
cap label variable tipo_de_vivienda_mod "Tipo de Vivienda(f)/Type of Housing(f)"
cap label variable tenencia_de_la_vivienda_mod "Tenencia de la Vivienda(f)/Tenure status of Households(f)"
cap label variable rubro_trabaja_mod "Rubro de Trabajo(f)/Area of Work(f)"
cap label variable otras_sus1_mod "Otras Sustancias (1)(Sólo más frecuentes)(f)/Other Substances (1)(Only more frequent)(f)"
cap label variable otras_sus2_mod "Otras Sustancias (2)(Sólo más frecuentes)(f)/Other Substances (2)(Only more frequent)(f)"
cap label variable otras_sus3_mod "Otras Sustancias (3)(Sólo más frecuentes)(f)/Other Substances (3)(Only more frequent)(f)"
cap label variable dg_trs_cons_sus_or "Diagnósico de Trastorno por Consumo de Sustancias(d)/Diagnosed of Substance Use Disorder(d)"
cap label variable diagnostico_trs_fisico "Diagnóstico de Trastorno Físico(g)/Diagnosis of Physical Disorder(g)"
cap label variable otros_probl_at_sm_or "Otros Problemas de Atención Vinculados a Salud Mental(g)/Other problems linked to Mental Health(g)"
cap label variable ano_bd_first "Año de la Base de Datos(c)/Year of the Dataset (Source)(c)"
cap label variable ano_bd_last "Año de la Base de Datos(b)/Year of the Dataset (Source)(b)"
cap label variable obs "Observaciones al Proceso de Limpieza y Estandarización de Casos(e)/Observations to the Process of Data Tidying & Standardization(e)"
cap label variable obs_concat_a "Observaciones al Proceso de Limpieza y Estandarización de Casos(*)/Observations to the Process of Data Tidying & Standardization(*)"
cap label variable rn_common_treats2 "Cuenta de Entradas Comunes(b)/Count of Common Entries(b)"
cap label variable concat_hash_id_treatments "Combination of User & Distint Entries"
cap label variable at_least_one_cont_entry "Casos de Usuarios con más de una entrada después de otra/Cases of users with more than one entry after another one"
cap label variable senda_concat_a "SENDA(*)/SENDA(*)"
cap label variable tipo_centro_concat_a "Tipo de Centro(*)/Type of Center(*)"
cap label variable fech_ing_num "Fecha de Ingreso a Tratamiento (Numérico)(c)/Date of Admission to Treatment (Numeric)(c)"
cap label variable fech_egres_num "Fecha de Egreso (Imputados KNN & Lógico)(Numérico)(b)/Date of Discharge (Imputed KNN & Logic)(Numeric)(b) of the Next Treatment"
cap label variable fech_ing_next_treat "Fecha de Ingreso a Tratamiento (Numérico)(c) del Tratamiento Posterior/Date of Admission to Treatment (Numeric)(c)"
cap label variable id_centro_sig_trat "ID del Centro del Tratamiento Posterior/Center ID of the Next Treatment"
cap label variable tipo_plan_sig_trat "Tipo de Plan del Tratamiento Posterior/Type of Plan of the Next Treatment"
cap label variable tipo_programa_sig_trat "Tipo de Programa del Tratamiento Posterior/Type of Program of the Next Treatment"
cap label variable senda_sig_trat "SENDA del Tratamiento Posterior/SENDA of the Next Treatment"
cap label variable menor_60_dias_diff "Menor a 60 días de diferencia con el Tratamiento Posterior/Menor a 60 days of difference between the Next Treatment"
cap label variable menor_45_dias_diff "Menor a 45 días de diferencia con el Tratamiento Posterior/Less than 45 days of difference between the Next Treatment"
cap label variable motivoegreso_derivacion "Motivo de Egreso= Derivación(b)/Cause of Discharge= Derivación(b)"
cap label variable abandono_temprano "Abandono temprano(<3 meses)/ Early Drop-out(<3 months)"
cap label variable obs_cambios "Cambios del tratamiento en comparación al Tratamiento Posterior/Changes in treatment compared to the Next Treatment"
cap label variable obs_cambios_ninguno "Sin cambios del tratamiento en comparación al Tratamiento Posterior/No changes in treatment compared to the Next Treatment"
cap label variable obs_cambios_num "Recuento de cambios del tratamiento en comparación al Tratamiento Posterior/Count of changes in treatment compared to the Next Treatment"
cap label variable obs_cambios_fac "Recuento de cambios del tratamiento en comparación al Tratamiento Posterior(factor)/Count of changes in treatment compared to the Next Treatment(factor)"
cap label variable edad_ini_sus_prin_grupos "Edad de Inicio de Consumo Sustancia Principal (en Grupos)/Age of Onset of Drug Use of Primary Substance (in Groups)"
cap label variable hash_key_sex_program "Usuarios a los que se le ha cambiado el sexo de acuerdo al tipo de plan/Users that changed of sex considering the types of plan"
cap label variable centro_muj "ID de centro que alude a un centro específico para mujeres/Center ID aludes to a women-specific center"
cap label variable cie_10 "Diagnóstico CIE-10 (1 o más)/Psychiatric Diagnoses (ICD-10)(one or more)"
cap label variable dsm_iv "Diagnóstico DSM-IV (1 o más)/Psychiatric Diagnoses (DSM-IV)(one or more)"
cap label variable con_quien_vive_rec "Persona con la que vive el Usuario (Recodificada)(f)/People that Share Household with the User (Cohabitation Status)(Recoded)(f)"
cap label variable cnt_mod_dsm_iv_or "Recuento de Diagnóstico DSM-IV/Count of Psychiatric Diagnoses (DSM-IV)"
cap label variable cnt_mod_cie_10_or "Recuento de Diagnóstico CIE-10/Count of Psychiatric Diagnoses (ICD-10)"
cap label variable cnt_diagnostico_trs_fisico "Recuento de Diagnóstico de Trastorno Físico/Count of Physical Disorder"
cap label variable dg_trs_psiq_sub_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (Subclasificacion)(g)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (sub-classification)(g)"
cap label variable dg_trs_psiq_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV(g)/Diagnosis of Psychiatric Disorders, DSM-IV criteria(g)"
cap label variable dg_trs_psiq_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10(g)/Diagnosis of Psychiatric Disorders, CIE-10 criteria(g)"
cap label variable x2_dg_trs_psiq_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (2)(g)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (2)(g)"
cap label variable x3_dg_trs_psiq_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (3)(g)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (3)(g)"
cap label variable x4_dg_trs_psiq_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (4)(g)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (4)(g)"
cap label variable x5_dg_trs_psiq_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (5)(g)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (5)(g)"
cap label variable x2_dg_trs_psiq_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (2)(g)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (2)(g)"
cap label variable x3_dg_trs_psiq_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (3)(g)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (3)(g)"
cap label variable x4_dg_trs_psiq_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (4)(g)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (4)(g)"
cap label variable dg_trs_psiq_sub_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (Subclasificacion)(g)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (subclassification)(g)"
cap label variable x2_dg_trs_psiq_sub_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (Subclasificacion) (2)(g)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (subclassification) (2)(g)"
cap label variable x3_dg_trs_psiq_sub_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (Subclasificacion) (3)(g)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (subclassification) (3)(g)"
cap label variable x4_dg_trs_psiq_sub_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (Subclasificacion)(4)(g)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (subclassification)(4)(g)"
cap label variable x2_dg_trs_psiq_sub_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (Subclasificacion) (2)(g)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (sub-classification) (2)(g)"
cap label variable x3_dg_trs_psiq_sub_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (Subclasificacion) (3)(g)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (sub-classification) (3)(g)"
cap label variable x4_dg_trs_psiq_sub_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (Subclasificacion)(4)(g)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (sub-classification)(4)(g)"
cap label variable cnt_otros_probl_at_sm_or "Recuento de Otros Problemas de Atención Vinculados a Salud Mental/Count of Other problems linked to Mental Health"
cap label variable escolaridad_rec "Escolaridad: Nivel Eduacional(d) Normalizado a Progresión de Tratamientos/Educational Attainment(d) & Normalized Following Progression of Treatments"
cap label variable tipo_de_plan_2 "Tipo de Plan del Último Registro/Type of Plan of the Last Entry"
cap label variable motivodeegreso_mod_imp "Motivo de Egreso (con abandono temprano y tardío)(Imputados KNN & Lógico) del Último Registro(b)/Cause of Discharge (with late and early withdrawal)(Imputed KNN & Logic) of the Last Entry(b)"
cap label variable rn_hash "Número de Tratamientos por Usuario (menor, tratamiento más antiguo)/Number of Treatments by User (less, older treatment)"
cap label variable n_hash "Número total de Tratamientos por Usuario/Total Number of Treatments by User"
cap label variable cum_dias_trat_sin_na "Suma acumulada de Días de Tratamiento por Usuario/Cumulative Days of Treatment by User"
cap label variable mean_cum_dias_trat_sin_na "Promedio acumulado de Días de Tratamiento por Usuario/Cumulative Average Days of Treatment by User"
cap label variable cum_diff_bet_treat "Suma acumulada de Diferencia en Días con Tratamiento Siguiente por Usuario/Cumulative sum of Days of difference between the Next Treatment by User"
cap label variable mean_cum_diff_bet_treat "Promedio acumulado de Diferencia en Días entre Tratamientos por Usuario/Cumulative Average Days of Differences Between Treatments By User"
cap label variable tipo_de_plan_2_1 ""
cap label variable tipo_de_plan_2_2 ""
cap label variable tipo_de_plan_2_3 ""
cap label variable tipo_de_plan_2_4 ""
cap label variable tipo_de_plan_2_5 ""
cap label variable tipo_de_plan_2_6 ""
cap label variable tipo_de_plan_2_7 ""
cap label variable tipo_de_plan_2_8 ""
cap label variable tipo_de_plan_2_9 ""
cap label variable tipo_de_plan_2_10 ""
cap label variable motivodeegreso_mod_imp_1 ""
cap label variable motivodeegreso_mod_imp_2 ""
cap label variable motivodeegreso_mod_imp_3 ""
cap label variable motivodeegreso_mod_imp_4 ""
cap label variable motivodeegreso_mod_imp_5 ""
cap label variable motivodeegreso_mod_imp_6 ""
cap label variable motivodeegreso_mod_imp_7 ""
cap label variable motivodeegreso_mod_imp_8 ""
cap label variable motivodeegreso_mod_imp_9 ""
cap label variable motivodeegreso_mod_imp_10 ""
cap label variable dias_treat_imp_sin_na_1 ""
cap label variable dias_treat_imp_sin_na_2 ""
cap label variable dias_treat_imp_sin_na_3 ""
cap label variable dias_treat_imp_sin_na_4 ""
cap label variable dias_treat_imp_sin_na_5 ""
cap label variable dias_treat_imp_sin_na_6 ""
cap label variable dias_treat_imp_sin_na_7 ""
cap label variable dias_treat_imp_sin_na_8 ""
cap label variable dias_treat_imp_sin_na_9 ""
cap label variable dias_treat_imp_sin_na_10 ""
cap label variable diff_bet_treat_1 ""
cap label variable diff_bet_treat_2 ""
cap label variable diff_bet_treat_3 ""
cap label variable diff_bet_treat_4 ""
cap label variable diff_bet_treat_5 ""
cap label variable diff_bet_treat_6 ""
cap label variable diff_bet_treat_7 ""
cap label variable diff_bet_treat_8 ""
cap label variable diff_bet_treat_9 ""
cap label variable diff_bet_treat_10 ""
cap label variable cum_dias_trat_sin_na_1 ""
cap label variable cum_dias_trat_sin_na_2 ""
cap label variable cum_dias_trat_sin_na_3 ""
cap label variable cum_dias_trat_sin_na_4 ""
cap label variable cum_dias_trat_sin_na_5 ""
cap label variable cum_dias_trat_sin_na_6 ""
cap label variable cum_dias_trat_sin_na_7 ""
cap label variable cum_dias_trat_sin_na_8 ""
cap label variable cum_dias_trat_sin_na_9 ""
cap label variable cum_dias_trat_sin_na_10 ""
cap label variable mean_cum_dias_trat_sin_na_1 ""
cap label variable mean_cum_dias_trat_sin_na_2 ""
cap label variable mean_cum_dias_trat_sin_na_3 ""
cap label variable mean_cum_dias_trat_sin_na_4 ""
cap label variable mean_cum_dias_trat_sin_na_5 ""
cap label variable mean_cum_dias_trat_sin_na_6 ""
cap label variable mean_cum_dias_trat_sin_na_7 ""
cap label variable mean_cum_dias_trat_sin_na_8 ""
cap label variable mean_cum_dias_trat_sin_na_9 ""
cap label variable mean_cum_dias_trat_sin_na_10 ""
cap label variable cum_diff_bet_treat_1 ""
cap label variable cum_diff_bet_treat_2 ""
cap label variable cum_diff_bet_treat_3 ""
cap label variable cum_diff_bet_treat_4 ""
cap label variable cum_diff_bet_treat_5 ""
cap label variable cum_diff_bet_treat_6 ""
cap label variable cum_diff_bet_treat_7 ""
cap label variable cum_diff_bet_treat_8 ""
cap label variable cum_diff_bet_treat_9 ""
cap label variable cum_diff_bet_treat_10 ""
cap label variable mean_cum_diff_bet_treat_1 ""
cap label variable mean_cum_diff_bet_treat_2 ""
cap label variable mean_cum_diff_bet_treat_3 ""
cap label variable mean_cum_diff_bet_treat_4 ""
cap label variable mean_cum_diff_bet_treat_5 ""
cap label variable mean_cum_diff_bet_treat_6 ""
cap label variable mean_cum_diff_bet_treat_7 ""
cap label variable mean_cum_diff_bet_treat_8 ""
cap label variable mean_cum_diff_bet_treat_9 ""
cap label variable mean_cum_diff_bet_treat_10 ""
cap label variable diff_bet_treat "Días de diferencia con el Tratamiento Posterior/Days of difference between the Next Treatment"
cap label variable dias_treat_imp_sin_na "Días de Tratamiento (valores perdidos en la fecha de egreso se reemplazaron por la diferencia con 2019-11-13)/Days of Treatment (missing dates of discharge were replaced with difference from 2019-11-13)"
cap label variable dup "Número de Tratamientos por HASH (posición a medida que aparecen, menor más antiguo)/Number of Treatments by User"
cap label variable duplicates_filtered "Número de Tratamientos por HASH (Total)/Number of Treatments by User (Total)"
cap label variable startdate "Fecha de Ingreso Primer Tratamiento (x HASH)/Date of Admission to the First Treatment (By User)"
cap label variable enddate "Fecha de Egreso del Último Tratamiento (x HASH)/Date of Discharge of the Last Treatment (By User)"
cap label variable person_days "Días en los que estado un usuario en el sistema para el estudio/User's Days available in the system for the study"
cap label variable a_botar "Descarta tratamientos posteriores por usuario(Perdidos= Trat Posteriores)/Discard posterior treatments by each user (Missing= Posterior Treatments)"
cap label variable event "Usuarios con Tratamiento Posterior (1=Reingreso x HASH)/Users with Posterior Treatments (1=Readmission)"
cap label variable person_years "Años en los que estado un usuario en el sistema para el estudio/User's Years available in the system for the study"
cap label variable dg_total_cie_10 "Conteo de Diagnósticos CIE-10(sólo diagnósticos)/Count of ICD-10 Diagnostics(only diagnoses)"
cap label variable dg_cie_10_rec "Diagnóstico CIE-10 (1 o más)(Recodificado)/Psychiatric Diagnoses (ICD-10)(one or more)(Recoded)"
cap label variable dg_total_dsm_iv "Conteo de Diagnósticos DSM-IV(sólo diagnósticos)/Count of DSM-IV Diagnostics(only diagnoses)"
cap label variable dg_dsm_iv_rec "Diagnóstico DSM-IV (1 o más)(Recodificado)/Psychiatric Diagnoses (DSM-IV)(one or more)(Recoded)"
cap label variable sus_ini_mod_mvv "Sustancia de Inicio (Valor más vulnerable)/Starting Substance (Most vulnerable value)"
cap label variable estatus_ocupacional_rec "Condición Ocupacional, corregido por categoría(f)/Occupational Status, corrected for categories(f)"
cap label variable hijos_trat_res "Tiene Hijos en Ingreso a Tratamiento Residencial del Último Registro/Have Children in Residential Treatment of the Last Entry"
cap label variable dg_fis_anemia "Dignóstico Físico, Anemia/Physical Dg. Anemia"
cap label variable dg_fis_card "Dignóstico Físico, Enfermedad Cardiaca/Physical Dg. Heart Disease"
cap label variable dg_fis_in_study "Dignóstico Físico, En estudio/Physical Dg. Under Study"
cap label variable dg_fis_enf_som "Dignóstico Físico, /Physical Dg. Somatic illnesses"
cap label variable dg_fis_ets "Dignóstico Físico, ETSs/Physical Dg. STDs"
cap label variable dg_fis_hep_alc "Dignóstico Físico, Hepatitis Alcohólica Subaguda/Physical Dg. Alcoholic hepatitis"
cap label variable dg_fis_hep_b "Dignóstico Físico, Hepatitis B, C, D/Physical Dg. Hepatitis B, C, D"
cap label variable dg_fis_hep_cro "Dignóstico Físico, Hepatitis Crónica/Physical Dg. Chronic hepatitis"
cap label variable dg_fis_inf "Dignóstico Físico, Enfermedades Infeccisas Rel. con Consumo/Physical Dg. Infectuous diseases related to SUDs"
cap label variable dg_fis_otr_cond_fis_ries_vit "Dignóstico Físico, Otras condiciones de riesgo vital/Physical Dg. Other phsysical conditions of vital risk"
cap label variable dg_fis_otr_cond_fis "Dignóstico Físico, Otras condiciones limitantes/Physical Dg. Other phsysical limitations"
cap label variable dg_fis_pat_buc "Dignóstico Físico, Patología Bucal/Physical Dg. Oral-care pathologies"
cap label variable dg_fis_pat_ges_intrau "Dignóstico Físico, Patología de la gestión y del niño intrauterino/Physical Dg. Development or Intrauterine"
cap label variable dg_fis_trau_sec "Dignóstico Físico, Traumatismos y secuelas secundarias/Physical Dg. Traumatisms and disabling sequelae"
cap label variable otros_pr_sm_abu_sex "Otros problemas de Atención de Salud Mental, Abuso Sexual/Other mental health problems, Sexual abuse"
cap label variable otros_pr_sm_exp_com_sex "Otros problemas de Atención de Salud Mental, Explotación Sexual Comercial/Other mental health problems, Commercial sexual exploitation"
cap label variable otros_pr_sm_otros "Otros problemas de Atención de Salud Mental, Otros/Other mental health problems, Other"
cap label variable otros_pr_sm_vif "Otros problemas de Atención de Salud Mental, Violencia Intrafamiliar/Other mental health problems, Domestic violence"
cap save "G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/CONS_C1_df_dup_SEP_2020.dta", replace
cap drop id id_mod nombre_centro consentimiento_informado
cap save "G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/CONS_C1_df_dup_SEP_2020_exp.dta", replace
