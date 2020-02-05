qui use "G:\Mi unidad\Alvacast\SISTRAT 2019 (github)\Stata SER 2020\_CONS_C1_df_csv_pedido_SER2020_avance6(ENE 2020).dta", clear
* cases with only one event are  keeped
* dup= count how many times the hash has repeated (eg. 1, 2,  3)
* duplicates_filtered= count the total times each hash has repeated, different than duplicated_hash because this may be considering deleted cases. 
* get the first case if there are no other among them
qui gen a_botar=.
qui recode a_botar .=1 if dup==0 & duplicates_filtered==1
qui recode a_botar .=1 if dup==1 & duplicates_filtered>1
qui drop if missing(a_botar)
qui gen event=0
qui recode event 0=1 if dup>0
qui stset person_days, failure(event==1) scale(365.25) id(row)
stdescribe, weight
*always get the first treatment, if it has more than one: 
*get the first case if there are no other among them
******browse hash_key a_botar dup duplicates_filtered if dup==0 & duplicates_filtered ==1
******browse hash_key a_botar dup duplicates_filtered if duplicates_filtered >1
****drop unique cases
drop if missing(a_botar)
****generate event if each hash has more than one admission
