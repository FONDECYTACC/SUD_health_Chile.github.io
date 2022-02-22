*final
clear all
ssc install dtalink
import delimited "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2019 (github)\CONS_C1_df_dup_ENE_2020.csv"
cap gen date_in = mdy(real_fech_ing_mes, real_fech_ing_dia, real_fech_ing_ano)
cap gen date_in = mdy(fech_ing_mes, fech_ing_dia, fech_ing_ano)
generate id_match = _n
cap drop _id
dtalink hash_key 25 -25 id 25 -25 sexo 10 0 idcentro 10 0 date_in 30 -30 5, block(edad) cutoff(70)
drop if missing(_score)
qui save "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2019 (github)\_CONS_C1_df_match70_2020_02_11.dta", replace
