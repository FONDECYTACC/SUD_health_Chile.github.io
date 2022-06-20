---
output:
  word_document: default
  html_document: default
  pdf_document: default
---
# SISTRAT Datasets

Welcome to the repositories of the construction of the treatment information system (SISTRAT) datasets. On this repository you can find the different processes and actions taken to standardize and prepare the data for the analysis of the investigators of the project.

This page is composed by the following main topics:

1. [Encryption of RUTs and Generation of HASHs](Encript.html)

2. [Data Preparation and Standardization of C1](Data_prep_C1) 

  1.1. [Preliminary Results for SER 2020](SER_Stata.html)
  
  1.2. [Deduplication of C1](Duplicates)
  
  1.3. [Deduplication of C1, Part 2](Duplicates2)
  
  1.4. [Deduplication of C1, Part 3](Duplicates3)
  
  1.5. [Deduplication of C1, Part 4](Duplicates4)
  
  1.6. [Presentation of Preliminary Processes and Descriptive Data to Institutions](Presentación2)
  
  1.7. [Descriptive Glimpse of the Database](Desc)
  
  1.8. [Codebook of C1](codebook)

3. Associations & Analytic Exercises

  3a.1. Ambulatory or residential? a multi-state analysis of treatments for substace use disorders- Main analyses

  * 3a.1.1.a [Step 1: Imputation, Matching & set the database](Matching_Process1_APR_22)
  
  * 3a.1.1.b [Stata, Step 1: Set the database, AJ estimates, Compute Transition Probabilities](Matching_Process1_stata_APR_22)

  * 3a.1.2.b [Stata, Step 2: Compute Transition Probabilities at 3 years](Matching_Process2_stata_APR_22)

  * 3a.1.3.b [Stata, Step 3: Plot probabilities and differences](Matching_Process3_stata_APR_22)

  * 3a.1.4.b [Stata, Step 4: Compute Transition Probabilities for a patient that completed treatment at 3 years](Matching_Process4_stata_APR_22)

  * 3a.1.4.5.b [Stata, Step 4.5: Summarise estimates in tables and export](Matching_Process4_5_stata_APR_22)
  
  * 3a.1.6.a [Step 5: Sensitivity analyses, Sankey, Transition Trees](Matching_Process5_APR_22) 
  
  3a.2. Ambulatory or residential? a multi-state analysis of treatments for substace use disorders- Supplemental analyses
  
  * 3a.2.1.a [Step 1.25](Matching_Process1_25_APR_22)
      
  * 3a.2.2.a [Step 1.5](Matching_Process15_APR_22)

  * 3a.2.3.a [Step 2)](Matching_Process2_APR_22)

  * 3a.2.4.a [Step 3)](Matching_Process3_APR_22)

  * 3a.2.5.a [Step 4)](Matching_Process4_APR_22)

  * 3a.2.6.b [Stata, Step 5)](Matching_Process5_stata_APR_22)

  * 3a.2.1.c [Stata, Step 1 w/ only complete cases)](Matching_Process1_stata_APR_22_cc)

  3b.1. Treatment outcome and readmission risk among women in women-only versus mixed-gender drug treatment programs in Chile- Main

  * 3b.1.1.a [Step 1: Descriptive tables, Partitioned Survival analyses, Competing risks, Select Survival Distributions](Proyecto_carla3)

  * 3b.1.2.a [Supplemental: Step 2: Compute Transition Probabilities](Proyecto_carla32)

  * 3b.1.3.a [Supplemental: Step 2: Get Transition Intensities, Compute Transition Probabilities, Comparative Plots, ](Proyecto_carla33)

  * 3b.4.a [Step 4: After review, Survival distributions revised, Simulate probabilities to get readmitted](Proyecto_carla34)

  3b.2. Treatment outcome and readmission risk among women in women-only versus mixed-gender drug treatment programs in Chile- Supplemental

  * 3b.2.1.b [Stata, Step 1](Proyecto_carla1_stata_JUN_21)
  
  * 3b.2.2.b [Stata, Step 2](Proyecto_carla2_stata_JUN_21)
  
  * 3b.2.3.b [Stata, Step 3](Proyecto_carla3_stata_JUN_21)
  
  * 3b.2.4.b [Stata, Step 4](Proyecto_carla4_stata_JUN_21)
  
  3c.1. Living with [conslidation](analisis_joel2)
  
  3c.2. Living with (Stata) [conslidation](analisis_joel_oct_2021_stata1)
  
  3c.3. Living with (Stata)- [Latent Class Analysis](analisis_joel_oct_2021_stata2)
  
  3c.4. Living with (Stata)- [Survival Regression](analisis_joel_oct_2021_stata3)
  
4. [Data Preparation and Standardization of TOP or Profile of Treatment Results](Data_prep_TOP)

  * 4.1. [Codebook of TOP](codebook_TOP)

5. [Chilean prosecutor’s office Data merge](Fiscalia_merge)

6. [Webinar "¿Qué sabemos de los programas de tratamiento de drogas en Chile? (What do we know about Chilean substance use treatments?)](https://youtu.be/xuROIbzEw5I)

<br>

The main processes are summarized in the following figures.

<br>

##### Figure 1. Diagram of data preparation
<a href="https://fondecytacc.github.io/SUD_health_Chile.github.io/Figures/RUT_Administraci%C3%B3n.svg" target="_blank">To open in a new window</a>

![Diagram](Figures/RUT_Administración.svg) 

##### Figure 2. STROBE Diagram
<a href="https://fondecytacc.github.io/SUD_health_Chile.github.io/Figures/Diagram_STROBE.svg" target="_blank">To open in a new window</a>

![STROBE](Figures/Diagram_STROBE.svg)