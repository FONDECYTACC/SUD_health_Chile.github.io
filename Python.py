#https://cran.r-project.org/web/packages/reticulate/vignettes/python_packages.html

#ver cómo hacer que el paquete sea efectivo
devtools::install_github("rstudio/reticulate", INSTALL_opts = c('--no-lock')) 


#creo qe hay que correrlo sólo si lo pide el programa
reticulate::install_miniconda(path="C:/ProgramData/Miniconda3", force =T)
reticulate::use_python('C:/Users/CISS Fondecyt/Documents/.conda/envs/r-reticulate/python.exe', required = TRUE)
#reticulate::use_python('C:/Users/CISS Fondecyt/AppData/Local/Programs/Python/Python38/python.exe', required = TRUE)
reticulate::use_virtualenv("r-reticulate")
#para ver la configuracióny ubicación de los paquetes
reticulate::py_config()
#use_virtualenv("~/myenv")
#use_condaenv("myenv")

#para intantar de instalar n módulo. Me tira error 127
reticulate::py_install("pandas")
#intentaré insalarlo de este modo

reticulate::conda_install("r-reticulate", "scipy")
reticulate::conda_install("r-reticulate", "pandas")
reticulate::conda_install("r-reticulate", "numpy")
reticulate::conda_install("r-reticulate", "scipy")
reticulate::conda_install("r-reticulate", "wordcloud")
reticulate::conda_install("r-reticulate", "pip")
reticulate::conda_install("r-reticulate", "unidecode")
reticulate::conda_install("r-reticulate", "future")
reticulate::conda_install("r-reticulate", "dedupe")
reticulate::conda_install("r-reticulate", "csv")
reticulate::conda_install("r-reticulate", "psycopg2")
reticulate::conda_install("r-reticulate", "os")
reticulate::conda_install("r-reticulate", "csv")
reticulate::conda_install("r-reticulate", "re")
reticulate::conda_install("r-reticulate", "logging")
reticulate::conda_install("r-reticulate", "optparse")
reticulate::conda_install("r-reticulate", "itertools")
reticulate::conda_install("r-reticulate", "seaborn")
reticulate::conda_install("r-reticulate", "matplotlib.pyplot")


reticulate::conda_install("r-reticulate", "recordlinkage")
reticulate::conda_install("r-reticulate", "dedupe")

#otra forma pero no en entorno anaconda, sino pip
py_install("fhirclient", pip = TRUE)
py_module_available("fhirclient")
fh <- import("fhirclient")

To use pip in your environment, in your terminal window or an Anaconda Prompt, run:

conda install -n myenv pip
conda activate myenv
pip <pip_subcommand>

#para chequear que estoy ocupando python
reticulate::repl_python()

import sys
print(sys.path)

import pandas as pd
import numpy as np
import scipy as sp
import os
import csv
import re
import logging
import optparse
import recordlinkage
import dedupe
import matplotlib.pyplot as plt
import seaborn as sns


write.csv2(CONS_C1_df_dup, "_CONS_C1_df_dup.csv")

#enc::all_utf8(CONS_C1_df_dup) no funciona, deja pegado el computador

#enc::as_utf8(CONS_C1_df_dup) no funciona, deja pegado el compu
#example
df = pd.read_csv('_CONS_C1_df_dup.csv')
encode(CONS_C1_df_dup, "utf-8")
CONS_C1_df_dup_py <- reticulate::r_to_py(CONS_C1_df_dup, encoding='latin-1')


# do some array manipulations with NumPy
a <- np$array(c(1:4))
sum <- a$cumsum()

# convert to R explicitly at the end
py_to_r(sum)

r_to_py(CONS_C1_df_dup, convert = FALSE)


#I had the same problem. I tried to add .Rprofile in my working or project directory and restarting R but no. 
#It only worked once I put the .Rprofile in my home directory. Then close RStudio altogether and open it again.
#echo "Sys.setenv(RETICULATE_PYTHON = '~/anaconda3/envs/py35/bin/python')" > ~/.Rprofile



import pandas as pd
#I suggest you add RETICULATE_PYTHON config to ~/.Renviron
echo "RETICULATE_PYTHON = /your_python_3.7.1/bin/python" > ~/.Renviron
#which can automatically launch all Python session from this engine after you re-login the session.


###########################

library(reticulate)
pybuiltins <- reticulate::import_builtins(CONS_C1_df_dup)
pybuiltins$set(list(1,2,3))



#para hacer n heatmat
sns$heatmap(r_to_py(df1), fmt="g", cmap ='viridis')
sns$pairplot(r_to_py(iris), hue = 'Species')
plt$show()

