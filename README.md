# ExplorationModeling

Final Project Report: Exploration and Modeling 

Introduction: 

For this project, we have decided to predict whether a patient have renal or respiratory disease. In the team, we have Michael Chang, Shruthi Rapole, Rohini, and Meelad Amouzgar. We extract information utilizing the MIMIC III dataset. We extract data from table ADMISSIONS, DIAGNOSES_ICD, PATIENTS and PRESCRIPTIONS. From table ADMISSIONS, we select subject_ID, hadm_ID, admittime, dischtime, insurance, language, ethnicity, diagnosis, and hospital_expire_flag. From DIAGNOSES, we select icd9_code. From PATIENTS table, we select subject_id, gender, and expire_flag. From PRESCRIPTIONS table, we select subject_id, drug, and drug_name_generic. 

Our inclusion and exclusion criteria are: 
Inclusion criteria: 
-	Patient with icd9 code for diseases of the respiratory, patient with icd9 code for renal impairments.
-	Demographic and prescription information.
Exclusion criteria: 
-	Patient with serious conditions such as cancers (patient who is diagnosed with cancers).

Data extraction: The data were being extracted from MIMIC tables (ADMISSIONS, DIAGNOSES_ICD, PATIENTS) joined with the  PRESCRIPTIONS table, and removed all repeated measurements. 

Our final dataset has a total of 24 variables and 34,280 observations. Our response variable called ‘class’ with two factors: renal and resp. I typecast class to a binary variable with 0 for Respiratory and 1 for Renal. 
