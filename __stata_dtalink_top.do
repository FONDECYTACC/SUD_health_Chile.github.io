*final
clear all
ssc install dtalink
import delimited "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2019 (github)\CONS_TOP_df_dup_ENE_2020.csv"
qui generate id_match = _n
cap drop _id _matchID _matchflag _score
gen fech_ap_top_mod = date(fech_ap_top, "YMD")
dtalink hash_key 125 -125 fech_ap_top_mod 125 -125 5 tipocentro 5 -5  sustanciaprincipal1 5 -5 sustanciaprincipal2 5 -5 sustanciaprincipal3 5 -5 totaloh 5 -5 dosisoh 5 -5 totalthc 5 -5 dosisthc 5 -5 totalpbc 5 -5 dosispbc 5 -5 totalcoc 5 -5 dosiscoc 5 -5 totalbzd 5 -5 dosisbzd 5 -5 totalotra 5 -5 dosisotra 5 -5 hurto 5 -5 robo 5 -5 ventadrogas 5 -5 rina 5 -5 totalvif 5 -5 otro 5 -5 totaltransgresion 5 -5 saludpsicologica 5 -5 totaltrabajo 5 -5 totaleducacion 5 -5 saludfisica 5 -5 lugarvivir 5 -5 vivienda 5 -5 calidadvida 5 -5, block(edad) cutoff(300)
drop if missing(_score)
qui save "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2019 (github)\_CONS_TOP_df_match75_05_02_2020.dta", replace
