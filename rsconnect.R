load("G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/5.Rdata")

ExPanD(df = CONS_C1_df_dup_FEB_2020, cs_id = "hash_key", ts_id = "fech_ing")
options(rsconnect.http.trace = TRUE)
options(rsconnect.http.verbose = TRUE)
options(rsconnect.launch.browser = FALSE)
getOption("rsconnect.max.bundle.size")

