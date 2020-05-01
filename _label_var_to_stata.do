use "G:\Mi unidad\Alvacast\SISTRAT 2019 (github)\CONS_C1_df_dup_FEB_2020.dta", clear 
*="cap label variable "&A2&" """&B2&""""
cap label variable row "Numerador de los eventos presentes en la Base de Datos/Events in the Dataset"
cap label variable table "Origen de los Datos (de los archivos por año)/Source of Data (of files per year)"
cap label variable hash_key "Codificación del RUT/Masked Identifier (RUT)"
cap label variable ano_bd "Año de la Base de Datos/Year of the Dataset (Source)"
cap label variable id "Codigo Identificación de SENDA/SENDAs ID"
cap label variable nombre_centro "Nombre del Centro de Tratamiento/Treatment Center"
cap label variable tipo_centro "Tipo de Centro/Type of Center"
cap label variable region_del_centro "Región del Centro/Chilean Region of the Center"
cap label variable servicio_de_salud "Servicio de Salud/Health Service"
cap label variable tipo_de_programa "(original, Recodificado en tipo_de_programa_2)/"
cap label variable tipo_de_plan "(original, Recodificado en tipo_de_plan_2)/"
cap label variable senda "SENDA/SENDA"
cap label variable dias_trat "Días de Tratamiento/Days of Treatment"
cap label variable nmesesentratamiento "Número de Meses en Tratamiento/Number of Months in Treatment"
cap label variable dias_en_senda "Días en SENDA/Days in SENDA"
cap label variable n_meses_en_senda "Número de Meses en SENDA/Number of Months in SENDA"
cap label variable sexo "(original, Recodificado en sexo_2)/"
cap label variable edad "Edad (número entero)/Age (Discrete Number)"
cap label variable nombre_usuario "Nombre del Usuario (OCULTO y no accesible)/Name of the User (Not Accessible)"
cap label variable comuna_residencia "Comuna de Residencia/Municipality or District of Residence"
cap label variable origen_de_ingreso "(original, Recodificado en origen_ingreso)/"
cap label variable pais_nacimiento "País de Nacimiento/Country of Birth"
cap label variable nacionalidad "Nacionalidad/Nationality"
cap label variable etnia "Etnia/Ethnicity"
cap label variable estado_conyugal "(original, Recodificado en estado_conyugal_2)/"
cap label variable numero_de_hijos "Número de Hijos/Number of Children"
cap label variable num_hijos_ing_trat_res "Número de Hijos para Ingreso a Tratamiento Residencial/Number of Children to Residential Treatment"
cap label variable parentesco_con_el_jefe_de_hogar "(Sólo presenta valores perdidos)/"
cap label variable num_trat_ant "Número de Tratamientos Anteriores/Number of Previous Treatments"
cap label variable fecha_ultimo_tratamiento "Fecha del Último Tratamiento (aún no formateada como fecha)/Date of the Last Treatment"
cap label variable sustancia_de_inicio "(original, Recodificado en sus_ini)/"
cap label variable edad_inicio_consumo "(original, Recodificado en edad_ini_cons)/"
cap label variable x_se_trata_mujer_emb "Mujer Embarazada al Ingreso/Pregnant at Admission"
cap label variable escolaridad_ultimo_ano_cursado "(original, Recodificado en escolaridad)/"
cap label variable condicion_ocupacional "(original, Recodificado en estatus_ocupacional)/"
cap label variable categoria_ocupacional "(original, Recodificado en cat_ocupacional)/"
cap label variable rubro_trabaja "Rubro de Trabajo/Area of Work"
cap label variable con_quien_vive "Persona con la que vive el Usuario/People that Share Household with the User"
cap label variable tipo_de_vivienda "Tipo de Vivienda/Type of Housing"
cap label variable tenencia_de_la_vivienda "Tenencia de la Vivienda/Tenure status of Households"
cap label variable sustancia_principal "(original, Recodificado en sus_principal)/"
cap label variable otras_sustancias_nº1 "(original, Recodificado en otras_sus1)/"
cap label variable otras_sustancias_nº2 "(original, Recodificado en otras_sus2)/"
cap label variable otras_sustancias_nº3 "(original, Recodificado en otras_sus3)/"
cap label variable freq_cons_sus_prin_original "(original, Recodificado en freq_cons_sus_prin)/"
cap label variable edad_inicio_sustancia_principal "(original, Recodificado en edad_ini_sus_prin)/"
cap label variable via_adm_sus_prin_original "(original, Recodificado en via_adm_sus_prin)/"
cap label variable dg_trs_cons_sus_or "Diagnósico de Trastorno por Consumo de Sustancias/Diagnosed of Substance Use Disorder"
cap label variable dg_trs_psiq_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV/Diagnosis of Psychiatric Disorders, DSM-IV criteria"
cap label variable dg_trs_psiq_sub_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (Subclasificacion)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (sub-classification)"
cap label variable x2_dg_trs_psiq_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (2)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (2)"
cap label variable x2_dg_trs_psiq_sub_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (Subclasificacion) (2)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (sub-classification) (2)"
cap label variable x3_dg_trs_psiq_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (3)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (3)"
cap label variable x3_dg_trs_psiq_sub_dsm_iv_or "Diagnóstico de Trastorno Psiquiátrico, Criterios DSM IV (Subclasificacion) (3)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (sub-classification) (3)"
cap label variable dg_trs_psiq_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10/Diagnosis of Psychiatric Disorders, CIE-10 criteria"
cap label variable dg_trs_psiq_sub_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (Subclasificacion)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (subclassification)"
cap label variable x2_dg_trs_psiq_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (2)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (2)"
cap label variable x2_dg_trs_psiq_sub_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (Subclasificacion) (2)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (subclassification) (2)"
cap label variable x3_dg_trs_psiq_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (3)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (3)"
cap label variable x3_dg_trs_psiq_sub_cie_10_or "Diagnóstico de Trastorno Psiquiátrico, Criterios CIE-10 (Subclasificacion) (3)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (subclassification) (3)"
cap label variable diagnostico_trs_fisico "Diagnóstico de Trastorno Físico/Diagnosis of Physical Disorder"
cap label variable otros_probl_at_sm_or "Otros Problemas de Atención Vinculados a Salud Mental/Other problems linked to Mental Health"
cap label variable compromiso_biopsicosocial "Compromiso Biopsicosocial/Biopsychosocial Involvement"
cap label variable dg_global_nec_int_soc_or "Diagnóstico Global de Necesidades de Integración Social/Global Diagnosis of Social Integration"
cap label variable dg_nec_int_soc_cap_hum_or "Diagnóstico de Necesidades de Integración Social en Capital Humano/Global Diagnosis of Social Integration in Human Capital"
cap label variable dg_nec_int_soc_cap_fis_or "Diagnóstico de Necesidades de Integración Social en Capital Físico/Global Diagnosis of Social Integration in Physical Capital"
cap label variable dg_nec_int_soc_cap_soc_or "Diagnóstico de Necesidades de Integración Social en Capital Social/Global Diagnosis of Social Integration in Social Capital"
cap label variable fech_ing "Fecha de Ingreso a Tratamiento/Date of Admission to Treatment"
cap label variable fecha_ingreso_a_convenio_senda "Fecha de Ingreso a Convenio SENDA (aún no formateada como fecha)/Date of Admission to SENDA Agreement"
cap label variable usuario_tribunal_trat_droga "Usuario de modalidad Tribunales de Tratamiento de Drogas/User of Drug Treatment Courts Modality"
cap label variable consentimiento_informado "Consentimiento Informado/Informed Consent"
cap label variable fech_egres "Fecha de Egreso de Tratamiento/Date of Discharge from Treatment"
cap label variable motivodeegreso "Motivo de Egreso/Cause of Discharge"
cap label variable tipo_centro_derivacion "Tipo de Centro al que el Usuario es Derivado/Type of Center of Derivation"
cap label variable evaluacindelprocesoteraputico "Evaluación del Proceso Terapéutico/Evaluation of the Therapeutic Process"
cap label variable eva_consumo "Evaluación al Egreso Respecto al Patrón de consumo/Evaluation at Discharge regarding to Consumption Pattern"
cap label variable eva_fam "Evaluación al Egreso Respecto a Situación Familiar/Evaluation at Discharge regarding to Family Situation"
cap label variable eva_relinterp "Evaluación al Egreso Respecto a Relaciones Interpersonales/Evaluation at Discharge regarding to Interpersonal Relations"
cap label variable eva_ocupacion "Evaluación al Egreso Respecto a Situación Ocupacional/Evaluation at Discharge regarding to Occupational Status"
cap label variable eva_sm "Evaluación al Egreso Respecto a Salud Mental/Evaluation at Discharge regarding to Mental Health"
cap label variable eva_fisica "Evaluación al Egreso Respecto a Salud Física/Evaluation at Discharge regarding to Physical Health"
cap label variable eva_transgnorma "Evaluación al Egreso Respecto a Trasgresión a la Norma Social/Evaluation at Discharge regarding to Transgression to the Norm"
cap label variable dg_trs_psiq_cie_10_egres_or "(Sólo presenta valores perdidos)/"
cap label variable dg_global_nec_int_soc_or_1 "Diagnóstico Global de Necesidades de Integración Social (1)/Global Diagnosis of Social Integration (1)"
cap label variable dg_nec_int_soc_cap_hum_or_1 "Diagnóstico de Necesidades de Integración Social en Capital Humano (1)/Global Diagnosis of Social Integration in Human Capital (1)"
cap label variable dg_nec_int_soc_cap_fis_or_1 "Diagnóstico de Necesidades de Integración Social en Capital Físico (1)/Global Diagnosis of Social Integration in Physical Capital (1)"
cap label variable dg_nec_int_soc_cap_soc_or_1 "Diagnóstico de Necesidades de Integración Social en Capital Social (1)/Global Diagnosis of Social Integration in Social Capital (1)"
cap label variable tiene_menores_de_edad_a_cargo "Menores de Edad A Cargo/Minor Dependants"
cap label variable mot_egres_alt_adm_or "Motivo de Egreso Alta Administrativa/Cause of Administrative Discharge"
cap label variable consorcio "Sociedades de Tratamiento, Servicios de Salud, Fundaciones, entre otras entidades encargadas de los centros/Consortium"
cap label variable id_centro "ID de Centro/Center ID"
cap label variable ha_estado_embarazada_egreso "¿Ha estado embarazada? (al Egreso)/Have you been Pregnant (at Discharge)"
cap label variable identidad_de_genero "Identidad de Género/Gender Identity"
cap label variable discapacidad "Presenta Discapacidad/Disability"
cap label variable hash_rut_completo "HASH alternativo, en el escenario en que se asuma que el individuo al que se le codificó el RUT presente mayor edad/Alternative HASH-Key"
cap label variable opcion_discapacidad "Origen de Discapacidad/Cause of Disability"
cap label variable sexo_2 "Sexo Usuario/Sex of User"
cap label variable embarazo "Embarazo/Pregnant"
cap label variable tipo_de_plan_2 "Tipo de Plan/Type of Plan"
cap label variable tipo_de_programa_2 "Tipo de Programa de Tratamiento/Type of Program"
cap label variable fech_egres_sin_fmt "Fecha de Egreso de Tratamiento (Sin Formato de Fecha)/Date of Discharge"
cap label variable id_mod "ID de SENDA para Presentación en Página Web (enmascara caracteres 5 y 6)/SENDAs ID (mask characters 5 & 6)"
cap label variable ano_nac "Año de Nacimiento (numérico)/Year of Birth (numeric)"
cap label variable fech_ing_ano "Año de Ingreso (numérico)/Year of Admission (numeric)"
cap label variable fech_ing_mes "Mes de Ingreso (numérico)/Month of Admission (numeric)"
cap label variable fech_ing_dia "Día de Ingreso (numérico)/Day of Admission (numeric)"
cap label variable concat "ID de SENDA y HASH Concatenado (permite discriminar más de un HASH en un mismo ID)/Combination of SENDAs ID & HASH"
cap label variable dup_todo ""
cap label variable dias_trat_inv "Días de Tratamiento Invertidos (fecha más reciente, menor valor numérico)/Treatment Days (Reversed)"
cap label variable fech_nac "Fecha de Nacimiento/Date of Birth"
cap label variable edad_al_ing "Edad a la Fecha de Ingreso a Tratamiento (numérico continuo)/Age at Admission to Treatment"
cap label variable edad_ini_cons "Edad de Inicio de Consumo/ Age of Onset of Drug Use"
cap label variable edad_ini_sus_prin "Edad de Inicio de Consumo Sustancia Principal/ Age of Onset of Drug Use Primary or Main Substance at Admission"
cap label variable dias_trat_alta_temprana "Días de tratamiento (<90)/ Less than 90 days in treatment"
cap label variable motivodeegreso_mod "Motivo de Egreso (con abandono temprano y tardío)/Cause of Discharge (with late and early withdrawal)"
cap label variable sus_principal "Sustancia Principal de Consumo/Primary or Main Substance of Consumption"
cap label variable otras_sus1 "Otras Sustancias (1)/Other Substances (1)"
cap label variable otras_sus2 "Otras Sustancias (2)/Other Substances (2)"
cap label variable otras_sus3 "Otras Sustancias (3)/Other Substances (3)"
cap label variable sus_ini "Sustancia de Inicio/Starting Substance"
cap label variable estado_conyugal_2 "Estado Conyugal/Marital Status"
cap label variable estatus_ocupacional "Condición Ocupacional/Occupational Status"
cap label variable cat_ocupacional "Categoría Ocupacional/Occupational Category"
cap label variable edad_grupos "Edad agrupada/Age in groups"
cap label variable origen_ingreso "Origen de Ingreso/Motive of Admission to Treatment"
cap label variable escolaridad "Escolaridad: Nivel Eduacional/Educational Attainment"
cap label variable via_adm_sus_prin "Vía de Administración de la Sustancia Principal/Route of Administration of the Primary or Main Substance"
cap label variable freq_cons_sus_prin "Frecuencia de Consumo de la Sustancia Principal/Frequency of Consumption of the Primary or Main Substance"
cap label variable dias_trat_knn_imp "Días de Tratamiento (Imputados KNN)/Days of Treatment (Imputed KNN)"
cap label variable fech_egres_knn_imp "Fecha de Egreso (Imputados KNN)/Date of Discharge (Imputed KNN)"
cap label variable dias_trat_alta_temprana_knn_imp "Días de Tratamiento con Alta Temprana (<90) (Imputados KNN)/Days of Treatment w Early Withdrawal (Imputed KNN)"
cap label variable fech_egres_imp "Fecha de Egreso (Imputados KNN & Lógico)/Date of Discharge (Imputed KNN & Logic)"
cap label variable motivodeegreso_imp "Motivo de Egreso(Imputados KNN & Lógico)/Cause of Discharge (Imputed KNN & Logic)"
cap label variable motivodeegreso_mod_imp "Motivo de Egreso (con abandono temprano y tardío)(Imputados KNN & Lógico)/Cause of Discharge (with late and early withdrawal)(Imputed KNN & Logic)"
cap label variable dias_trat_imp "Días de Tratamiento (Imputados KNN & Lógico)/Days of Treatment (Imputed KNN & Logic)"
cap label variable dias_trat_alta_temprana_imp "Días de Tratamiento con Alta Temprana (<90) (Imputados KNN & Lógico)/Days of Treatment w Early Withdrawal (Imputed KNN & Logic)"
cap label variable via_adm_sus_prin_act "Vía de Administración de la Sustancia Principal (Se aplicaron criterios de limpieza)/Route of Administration of the Primary or Main Substance (Tidy)"
cap label variable nacionalidad_2 "Segunda Nacionalidad/Second Nationality"
cap label variable etnia_2 "Etnia/Second Ethnic Group"
cap label variable sus_ini_2 "Segunda Sustancia de Inicio/Second Starting Substance"
cap save "G:\Mi unidad\Alvacast\SISTRAT 2019 (github)\CONS_C1_df_dup_FEB_2020.dta", replace
cap drop id id_mod nombre_centro
cap save "G:\Mi unidad\Alvacast\SISTRAT 2019 (github)\CONS_C1_df_dup_FEB_2020_exp.dta", replace
