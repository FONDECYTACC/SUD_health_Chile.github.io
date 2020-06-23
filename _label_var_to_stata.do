*final
use "G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/CONS_C1_df_dup_JUN_2020.dta", clear
cap label variable row = "Numerador de los eventos presentes en la Base de Datos/Events in the Dataset"
cap label variable table = "Origen de los Datos (de los archivos por a�o)/Source of Data (of files per year)"
cap label variable hash_key = "Codificaci�n del RUT/Masked Identifier (RUT)"
cap label variable ano_bd = "A�o de la Base de Datos/Year of the Dataset (Source)"
cap label variable id = "Codigo Identificaci�n de SENDA/SENDAs ID"
cap label variable nombre_centro = "Nombre del Centro de Tratamiento/Treatment Center"
cap label variable tipo_centro = "Tipo de Centro/Type of Center"
cap label variable region_del_centro = "(original, Recodificado en nombre_region)/"
cap label variable servicio_de_salud = "Servicio de Salud/Health Service"
cap label variable tipo_de_programa = "(original, Recodificado en tipo_de_programa_2)/"
cap label variable tipo_de_plan = "(original, Recodificado en tipo_de_plan_2)/"
cap label variable senda = "SENDA/SENDA"
cap label variable dias_trat = "D�as de Tratamiento/Days of Treatment"
cap label variable nmesesentratamiento = "N�mero de Meses en Tratamiento/Number of Months in Treatment"
cap label variable dias_en_senda = "D�as en SENDA/Days in SENDA"
cap label variable n_meses_en_senda = "N�mero de Meses en SENDA/Number of Months in SENDA"
cap label variable sexo = "(original, Recodificado en sexo_2)/"
cap label variable edad = "Edad (n�mero entero)/Age (In years, Discrete Number)"
cap label variable nombre_usuario = "Nombre del Usuario (OCULTO y no accesible)/Name of the User (Not Accessible)"
cap label variable comuna_residencia = "(original, Recodificado en comuna_residencia_cod)/"
cap label variable origen_de_ingreso = "(original, Recodificado en origen_ingreso)/"
cap label variable pais_nacimiento = "Pa�s de Nacimiento/Country of Birth"
cap label variable nacionalidad = "Nacionalidad/Nationality"
cap label variable etnia = "(original, recodificado en etnia_cor)/"
cap label variable estado_conyugal = "(original, Recodificado en estado_conyugal_2)/"
cap label variable numero_de_hijos = "N�mero de Hijos/Number of Children"
cap label variable num_hijos_ing_trat_res = "N�mero de Hijos para Ingreso a Tratamiento Residencial/Number of Children to Residential Treatment"
cap label variable parentesco_con_el_jefe_de_hogar = "(S�lo presenta valores perdidos)/"
cap label variable num_trat_ant = "N�mero de Tratamientos Anteriores/Number of Previous Treatments"
cap label variable fecha_ultimo_tratamiento = "Fecha del �ltimo Tratamiento/Date of the Last Treatment"
cap label variable sustancia_de_inicio = "(original, Recodificado en sus_ini)/"
cap label variable edad_inicio_consumo = "(original, Recodificado en edad_ini_cons)/"
cap label variable x_se_trata_mujer_emb = "Mujer Embarazada al Ingreso/Pregnant at Admission"
cap label variable escolaridad_ultimo_ano_cursado = "(original, Recodificado en escolaridad)/"
cap label variable condicion_ocupacional = "(original, Recodificado en estatus_ocupacional)/"
cap label variable categoria_ocupacional = "(original, Recodificado en cat_ocupacional)/"
cap label variable rubro_trabaja = "Rubro de Trabajo/Area of Work"
cap label variable con_quien_vive = "Persona con la que vive el Usuario/People that Share Household with the User"
cap label variable tipo_de_vivienda = "Tipo de Vivienda/Type of Housing"
cap label variable tenencia_de_la_vivienda = "Tenencia de la Vivienda/Tenure status of Households"
cap label variable sustancia_principal = "(original, Recodificado en sus_principal)/"
cap label variable otras_sustancias_n�1 = "(original, Recodificado en otras_sus1)/"
cap label variable otras_sustancias_n�2 = "(original, Recodificado en otras_sus2)/"
cap label variable otras_sustancias_n�3 = "(original, Recodificado en otras_sus3)/"
cap label variable freq_cons_sus_prin_original = "(original, Recodificado en freq_cons_sus_prin)/"
cap label variable edad_inicio_sustancia_principal = "(original, Recodificado en edad_ini_sus_prin)/"
cap label variable via_adm_sus_prin_original = "(original, Recodificado en via_adm_sus_prin)/"
cap label variable dg_trs_cons_sus_or = "Diagn�sico de Trastorno por Consumo de Sustancias/Diagnosed of Substance Use Disorder"
cap label variable dg_trs_psiq_dsm_iv_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios DSM IV/Diagnosis of Psychiatric Disorders, DSM-IV criteria"
cap label variable dg_trs_psiq_sub_dsm_iv_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios DSM IV (Subclasificacion)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (sub-classification)"
cap label variable x2_dg_trs_psiq_dsm_iv_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios DSM IV (2)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (2)"
cap label variable x2_dg_trs_psiq_sub_dsm_iv_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios DSM IV (Subclasificacion) (2)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (sub-classification) (2)"
cap label variable x3_dg_trs_psiq_dsm_iv_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios DSM IV (3)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (3)"
cap label variable x3_dg_trs_psiq_sub_dsm_iv_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios DSM IV (Subclasificacion) (3)/Diagnosis of Psychiatric Disorders, DSM-IV criteria (sub-classification) (3)"
cap label variable dg_trs_psiq_cie_10_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios CIE-10/Diagnosis of Psychiatric Disorders, CIE-10 criteria"
cap label variable dg_trs_psiq_sub_cie_10_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios CIE-10 (Subclasificacion)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (subclassification)"
cap label variable x2_dg_trs_psiq_cie_10_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios CIE-10 (2)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (2)"
cap label variable x2_dg_trs_psiq_sub_cie_10_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios CIE-10 (Subclasificacion) (2)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (subclassification) (2)"
cap label variable x3_dg_trs_psiq_cie_10_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios CIE-10 (3)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (3)"
cap label variable x3_dg_trs_psiq_sub_cie_10_or = "Diagn�stico de Trastorno Psiqui�trico, Criterios CIE-10 (Subclasificacion) (3)/Diagnosis of Psychiatric Disorders, CIE-10 criteria (subclassification) (3)"
cap label variable diagnostico_trs_fisico = "Diagn�stico de Trastorno F�sico/Diagnosis of Physical Disorder"
cap label variable otros_probl_at_sm_or = "Otros Problemas de Atenci�n Vinculados a Salud Mental/Other problems linked to Mental Health"
cap label variable compromiso_biopsicosocial = "Compromiso Biopsicosocial/Biopsychosocial Involvement"
cap label variable dg_global_nec_int_soc_or = "Diagn�stico Global de Necesidades de Integraci�n Social (Al Ingreso)/Global Diagnosis of Social Integration (At Admission)"
cap label variable dg_nec_int_soc_cap_hum_or = "Diagn�stico de Necesidades de Integraci�n Social en Capital Humano (Al Ingreso)/Global Diagnosis of Social Integration in Human Capital (At Admission)"
cap label variable dg_nec_int_soc_cap_fis_or = "Diagn�stico de Necesidades de Integraci�n Social en Capital F�sico (Al Ingreso)/Global Diagnosis of Social Integration in Physical Capital (At Admission)"
cap label variable dg_nec_int_soc_cap_soc_or = "Diagn�stico de Necesidades de Integraci�n Social en Capital Social (Al Ingreso)/Global Diagnosis of Social Integration in Social Capital (At Admission)"
cap label variable fech_ing = "Fecha de Ingreso a Tratamiento/Date of Admission to Treatment"
cap label variable fecha_ingreso_a_convenio_senda = "Fecha de Ingreso a Convenio SENDA (a�n no formateada como fecha)/Date of Admission to SENDA Agreement"
cap label variable usuario_tribunal_trat_droga = "Usuario de modalidad Tribunales de Tratamiento de Drogas/User of Drug Treatment Courts Modality"
cap label variable consentimiento_informado = "Consentimiento Informado/Informed Consent"
cap label variable fech_egres = "Fecha de Egreso de Tratamiento/Date of Discharge from Treatment"
cap label variable motivodeegreso = "Motivo de Egreso/Cause of Discharge"
cap label variable tipo_centro_derivacion = "Tipo de Centro al que el Usuario es Derivado/Type of Center of Derivation"
cap label variable evaluacindelprocesoteraputico = "Evaluaci�n del Proceso Terap�utico/Evaluation of the Therapeutic Process"
cap label variable eva_consumo = "Evaluaci�n al Egreso Respecto al Patr�n de consumo/Evaluation at Discharge regarding to Consumption Pattern"
cap label variable eva_fam = "Evaluaci�n al Egreso Respecto a Situaci�n Familiar/Evaluation at Discharge regarding to Family Situation"
cap label variable eva_relinterp = "Evaluaci�n al Egreso Respecto a Relaciones Interpersonales/Evaluation at Discharge regarding to Interpersonal Relations"
cap label variable eva_ocupacion = "Evaluaci�n al Egreso Respecto a Situaci�n Ocupacional/Evaluation at Discharge regarding to Occupational Status"
cap label variable eva_sm = "Evaluaci�n al Egreso Respecto a Salud Mental/Evaluation at Discharge regarding to Mental Health"
cap label variable eva_fisica = "Evaluaci�n al Egreso Respecto a Salud F�sica/Evaluation at Discharge regarding to Physical Health"
cap label variable eva_transgnorma = "Evaluaci�n al Egreso Respecto a Trasgresi�n a la Norma Social/Evaluation at Discharge regarding to Transgression to the Norm"
cap label variable dg_trs_psiq_cie_10_egres_or = "(S�lo presenta valores perdidos)/"
cap label variable dg_global_nec_int_soc_or_1 = "Diagn�stico Global de Necesidades de Integraci�n Social (Al Egreso)/Global Diagnosis of Social Integration (At Discharge)"
cap label variable dg_nec_int_soc_cap_hum_or_1 = "Diagn�stico de Necesidades de Integraci�n Social en Capital Humano (Al Egreso)/Global Diagnosis of Social Integration in Human Capital (At Discharge)"
cap label variable dg_nec_int_soc_cap_fis_or_1 = "Diagn�stico de Necesidades de Integraci�n Social en Capital F�sico (Al Egreso)/Global Diagnosis of Social Integration in Physical Capital (At Discharge)"
cap label variable dg_nec_int_soc_cap_soc_or_1 = "Diagn�stico de Necesidades de Integraci�n Social en Capital Social (Al Egreso)/Global Diagnosis of Social Integration in Social Capital (At Discharge)"
cap label variable tiene_menores_de_edad_a_cargo = "Menores de Edad A Cargo/Minor Dependants"
cap label variable mot_egres_alt_adm_or = "Motivo de Egreso Alta Administrativa/Cause of Administrative Discharge"
cap label variable consorcio = "Sociedades de Tratamiento Servicios de Salud- Fundaciones- entre otras entidades encargadas de los centros/Consortium"
cap label variable id_centro = "ID de Centro/Center ID"
cap label variable ha_estado_embarazada_egreso = "�Ha estado embarazada? (al Egreso)/Have you been Pregnant (at Discharge)"
cap label variable identidad_de_genero = "Identidad de G�nero/Gender Identity"
cap label variable discapacidad = "Presenta Discapacidad/Disability"
cap label variable hash_rut_completo = "HASH alternativo, en el escenario en que se asuma que el individuo al que se le codific� el RUT presente mayor edad/Alternative HASH-Key"
cap label variable opcion_discapacidad = "Origen de Discapacidad/Cause of Disability"
cap label variable sexo_2 = "Sexo Usuario/Sex of User"
cap label variable embarazo = "Embarazo/Pregnant"
cap label variable tipo_de_plan_2 = "Tipo de Plan/Type of Plan"
cap label variable tipo_de_programa_2 = "Tipo de Programa de Tratamiento/Type of Program"
cap label variable fech_egres_sin_fmt = "Fecha de Egreso de Tratamiento (Sin Formato de Fecha)/Date of Discharge"
cap label variable id_mod = "ID de SENDA para Presentaci�n en P�gina Web (enmascara caracteres 5 y 6)/SENDAs ID (mask characters 5 & 6)"
cap label variable ano_nac = "A�o de Nacimiento (num�rico)/Year of Birth (numeric)"
cap label variable fech_ing_ano = "A�o de Ingreso (num�rico)/Year of Admission (numeric)"
cap label variable fech_ing_mes = "Mes de Ingreso (num�rico)/Month of Admission (numeric)"
cap label variable fech_ing_dia = "D�a de Ingreso (num�rico)/Day of Admission (numeric)"
cap label variable concat = "ID de SENDA y HASH Concatenado (permite discriminar m�s de un HASH en un mismo ID)/Combination of SENDAs ID & HASH"
cap label variable obs = "Observaciones al Proceso de Limpieza y Estandarizaci�n de Casos/Observations to the Process of Data Tidying & Standardization"
cap label variable dias_trat_inv = "D�as de Tratamiento Invertidos (fecha m�s reciente, menor valor num�rico)/Treatment Days (Reversed)"
cap label variable fech_nac = "Fecha de Nacimiento/Date of Birth"
cap label variable edad_al_ing = "Edad a la Fecha de Ingreso a Tratamiento (num�rico continuo)/Age at Admission to Treatment"
cap label variable edad_ini_cons = "Edad de Inicio de Consumo/Age of Onset of Drug Use"
cap label variable edad_ini_sus_prin = "Edad de Inicio de Consumo Sustancia Principal/Age of Onset of Drug Use of Primary Substance"
cap label variable dias_trat_alta_temprana = "D�as de tratamiento (<90)/Less than 90 days in treatment"
cap label variable motivodeegreso_mod = "Motivo de Egreso (con abandono temprano y tard�o)/Cause of Discharge (with late and early withdrawal)"
cap label variable sus_principal = "Sustancia Principal de Consumo/Primary or Main Substance of Consumption at Admission"
cap label variable otras_sus1 = "Otras Sustancias (1)/Other Substances (1)"
cap label variable otras_sus2 = "Otras Sustancias (2)/Other Substances (2)"
cap label variable otras_sus3 = "Otras Sustancias (3)/Other Substances (3)"
cap label variable sus_ini = "Sustancia de Inicio/Starting Substance"
cap label variable estado_conyugal_2 = "Estado Conyugal/Marital Status"
cap label variable estatus_ocupacional = "Condici�n Ocupacional/Occupational Status"
cap label variable cat_ocupacional = "Categor�a Ocupacional/Occupational Category"
cap label variable edad_grupos = "Edad agrupada/Age in groups"
cap label variable origen_ingreso = "Origen de Ingreso/Motive of Admission to Treatment"
cap label variable escolaridad = "Escolaridad: Nivel Eduacional/Educational Attainment"
cap label variable via_adm_sus_prin = "V�a de Administraci�n de la Sustancia Principal/Route of Administration of the Primary or Main Substance"
cap label variable freq_cons_sus_prin = "Frecuencia de Consumo de la Sustancia Principal/Frequency of Consumption of the Primary or Main Substance"
cap label variable dias_trat_knn_imp = "D�as de Tratamiento (Imputados KNN)/Days of Treatment (Imputed KNN)"
cap label variable fech_egres_knn_imp = "Fecha de Egreso (Imputados KNN)/Date of Discharge (Imputed KNN)"
cap label variable dias_trat_alta_temprana_knn_imp = "D�as de Tratamiento con Alta Temprana (<90) (Imputados KNN)/Days of Treatment w Early Withdrawal (Imputed KNN)"
cap label variable fech_egres_imp = "Fecha de Egreso (Imputados KNN & L�gico)/Date of Discharge (Imputed KNN & Logic)"
cap label variable motivodeegreso_imp = "Motivo de Egreso(Imputados KNN & L�gico)/Cause of Discharge (Imputed KNN & Logic)"
cap label variable motivodeegreso_mod_imp = "Motivo de Egreso (con abandono temprano y tard�o)(Imputados KNN & L�gico)/Cause of Discharge (with late and early withdrawal)(Imputed KNN & Logic)"
cap label variable dias_trat_imp = "D�as de Tratamiento (Imputados KNN & L�gico)/Days of Treatment (Imputed KNN & Logic)"
cap label variable dias_trat_alta_temprana_imp = "D�as de Tratamiento con Alta Temprana (<90) (Imputados KNN & L�gico)/Days of Treatment w Early Withdrawal (Imputed KNN & Logic)"
cap label variable via_adm_sus_prin_act = "V�a de Administraci�n de la Sustancia Principal (Se aplicaron criterios de limpieza)/Route of Administration of the Primary or Main Substance (Tidy)"
cap label variable etnia_cor = "Etnia/Ethnic Group"
cap label variable nacionalidad_2 = "Segunda Nacionalidad/Second Nationality"
cap label variable etnia_cor_2 = "Etnia (2)/Second Ethnic Group"
cap label variable sus_ini_2 = "Segunda Sustancia de Inicio/Second Starting Substance"
cap label variable sus_ini_3 = "Tercera Sustancia de Inicio/Third Starting Substance"
cap label variable concat_hash_sus_prin = "Combination of User & Primary Substance"
cap label variable macrozona = "Macrozona/Macrozones"
cap label variable nombre_region = " Regi�n del Centro/Chilean Region of the Center"
cap label variable comuna_residencia_cod = "Comuna de Residencia/Municipality or District of Residence"
cap label variable sus_ini_mod = "Sustancia de Inicio (S�lo m�s frecuentes)/Starting Substance (Only more frequent)"
cap label variable sus_principal_mod = "Sustancia Principal de Consumo (S�lo m�s frecuentes)/Primary or Main Substance of Consumption at Admission (Only more frequent)"
cap label variable origen_ingreso_mod = "Origen de Ingreso/Motive of Admission to Treatment"
cap label variable menor_60_dias_diff = "Menor o igual a 60 d�as de diferencia con el registro posterior/Less or equal than 60 days of difference between the next entry"
cap label variable diff_bet_treat = "D�as de diferencia con el registro posterior/Days of difference between the next entry"
cap label variable id_centro_sig_trat = "ID del Centro del registro posterior/Center ID of the Next Treatment"
cap label variable tipo_plan_sig_trat = "Tipo de Plan del registro posterior/Type of Plan of the Next Entry"
cap label variable tipo_programa_sig_trat = "Tipo de Programa del registro posterior/Type of Program of the Next Entry"
cap label variable senda_sig_trat = "SENDA del registro posterior/SENDA of the Next Entry"
cap label variable motivoegreso_derivacion = "Motivo de Egreso del registro posterior/Cause of Discharge of the Next Entry"
cap label variable obs_cambios = "Cambios del tratamiento en comparaci�n al registro posterior/Changes in treatment compared to the Next Entry"
cap label variable obs_cambios_ninguno = "Sin cambios del tratamiento en comparaci�n al registro posterior/No changes in treatment compared to the Next Entry"
cap label variable obs_cambios_num = "Recuento de cambios del tratamiento en comparaci�n al registro posterior/Count of changes in treatment compared to the Next Entry"
cap label variable obs_cambios_fac = "Recuento de cambios del tratamiento en comparaci�n al registro posterior(factor)/Count of changes in treatment compared to the Next Entry(factor)"
cap save "G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/CONS_C1_df_dup_JUN_2020.dta", replace
cap drop id id_mod nombre_centro
cap save "G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/CONS_C1_df_dup_JUN_2020_exp.dta", replace
