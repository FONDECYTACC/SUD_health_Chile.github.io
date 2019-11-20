## Data preparation & Standardization

Esta página será de utilidad para ir comentando los avances y dudas del proceso de normalización y estandarización de la base de datos SISTRAT C1 (Convenio 1), con las consiguientes dudas.


### Building dataset

Define working directories which contains different text files that will be merged, correspondent to the years of the C1 Sistrat.

#Get working directory

```markdown
dir_c1 <-toString(paste0(getwd(),"/Encriptado c1/Personas tratadas c1/"))
dir_top <-toString(paste0(getwd(),"/encriptados TOP/"))
```

#Define files in the Directory folder, as long as they have the excel extension,
#and they do not start with ~  (represent a working temporary file in excel)

```markdown
SISTRAT_c1<-list.files(path=toString(dir_c1), all.files=T, pattern="^[2].*\\s*txt$")
SISTRAT_top<-list.files(path=toString(dir_top), all.files=T, pattern="^[t].*\\s*txt$")
```

Function to read tab delimited text files, with UTF-8 encoding and, based on the first 7 letters of its name, assign them an object name.

```markdown
read_excel_mult <- function(dir, filename) {
  assign(paste0(substr(filename, 1, 7)),read.delim(paste0(dir, filename),
        na.strings="null", header = T, fileEncoding="UTF-8"),envir = .GlobalEnv)
  }
```


