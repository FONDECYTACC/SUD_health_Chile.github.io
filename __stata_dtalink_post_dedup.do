*final
clear all
ssc install dtalink
use "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2019 (github)\CONS_C1_df_dup_FEB_2020_prev.dta"
cap gen date_in = mdy(real_fech_ing_mes, real_fech_ing_dia, real_fech_ing_ano)
cap gen date_in = mdy(fech_ing_mes, fech_ing_dia, fech_ing_ano)
generate id_match = _n
cap drop _id
dtalink hash_key 25 -25 id_centro 10 0 date_in 30 -30 5 tipo_de_plan 7 -7 escolaridad 8 -8 sexo 10 -10 id 10 -10, block(edad) cutoff(70)
drop if missing(_score)
cap qui save "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2019 (github)\_CONS_C1_df_match70_2020_02_29.dta"
cap qui save "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2019 (github)\_CONS_C1_df_match70_2020_02_29.dta", replace
