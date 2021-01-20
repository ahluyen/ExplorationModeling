install.packages("rJava")
install.packages("RH2")  
install.packages("RSQLite") 
install.packages("sqldf")
library("RSQLite")
library("RH2")
library(DBI)
library(dplyr)
library(dbplyr)
library(RPostgreSQL)
library(tidyverse)
library(tibble)
require("dplyr.teradata")
require("bigmemory")
require("speedglm")
library("sqldf")
memory.limit()
detach("package:RPostgreSQL",unload=TRUE)
# set max memory usage

memory.size(max=30000)
library(RPostgreSQL)
pw <- 'ojal1234'
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname = 'mimic', host = 'localhost', user = 'postgres', password = pw)
con
#____________________________________________________________________________________________________________________________________________________________________________________________

chest_x<- dbGetQuery(con, "select subject_id, hadm_id, charttime, value from mimiciii.CHARTEVENTS where itemid ='3366'")

#APACH_renal<- dbGetQuery(con, "select subject_id, hadm_id, charttime, value from mimiciii.CHARTEVENTS where itemid ='226748'")
#APACH_renal

creati<- dbGetQuery(con, "select subject_id, hadm_id, charttime, value as creat from mimiciii.CHARTEVENTS where itemid in ('1525','220615')")
creati

#High_resp<- dbGetQuery(con, "select subject_id, hadm_id, charttime, value from mimiciii.CHARTEVENTS where itemid ='1635'")
#High_resp

#resp_suppo<- dbGetQuery(con, "select subject_id, hadm_id, charttime, value from mimiciii.CHARTEVENTS where itemid ='3605'")
#resp_suppo

creati2<- dbGetQuery(con, "select subject_id, hadm_id, charttime, value from mimiciii.LABEVENTS where itemid in ('51082','50912')")
creati2

urin_vol<- dbGetQuery(con, "select * from mimiciii.LABEVENTS where itemid = '51109'")
urin_vol

plat <- dbGetQuery(con, "select * from mimiciii.LABEVENTS where itemid = '51265'")

O2<- dbGetQuery(con, "select * from mimiciii.LABEVENTS where itemid = '50818'")

wbc <-dbGetQuery(con, "select * from mimiciii.LABEVENTS where itemid = '51300'")

Char_evnt <- rbind(chest_x,APACH_renal,creati,High_resp,resp_suppo)

lab_evnt <- rbind(creati2,urin_vol,plat,O2)


##converting lab tables for join
#chest x-ray  for join
#chest_x <- sqldf("SELECT subject_id ,hadm_id,charttime, value as chest_X from chest_x")
chest_x<- sqldf("SELECT subject_id,hadm_id,charttime, value as chest_X  FROM  chest_x WHERE chest_X !=\"NA\"
                GROUP BY subject_id, charttime ")               
chest_x <- chest_x[chest_x$chest_X != 0,]     
names(chest_x)[2] <- "chest_x_hadm_id"

#
Creati <- sqldf("SELECT subject_id,hadm_id,charttime, creat FROM  creati WHERE creat !=\"NA\"
                GROUP BY subject_id, charttime ")               
Creati <- Creati[Creati$creat!= 0,]     
names(Creati)[2] <- "creati_hadm_id"


#high_resp <- sqldf("SELECT subject_id,hadm_id,charttime, value as Hi_resp FROM  High_resp WHERE High_resp !=\"NA\"
#                GROUP BY subject_id, charttime ")               
#high_resp <- high_resp[high_resp$Hi_resp!= 0,]     
#names(high_resp)[2] <- "Hi_resp_hadm_id"


Creati2<- sqldf("SELECT subject_id,hadm_id,charttime, value as creati2 FROM  creati2 WHERE creati2 !=\"NA\"
                GROUP BY subject_id, charttime ")               
Creati2 <- Creati2[Creati2$creati2!= 0,]     
names(Creati2)[2] <- "creati2_hadm_id"

Urin_vol <- sqldf("SELECT subject_id,hadm_id,charttime, value as urin_vol FROM  urin_vol WHERE urin_vol !=\"NA\"
                GROUP BY subject_id, charttime ")               
Urin_vol <- Urin_vol[Urin_vol$urin_vol!= 0,]     
names(Urin_vol)[2] <- "Urin_hadm_id"


#platlate for join
lab_plat <- sqldf("SELECT plat.subject_id ,plat.hadm_id,plat.charttime, plat.valuenum as platelet_count from plat plat")
lab_plat<- sqldf("SELECT subject_id,hadm_id,charttime, min(platelet_count) as min_plat FROM lab_plat WHERE platelet_count !=\"NA\"
                      GROUP BY subject_id, charttime ")               
lab_plat<- lab_plat[lab_plat$min_plat != 0,]     
names(lab_plat)[2] <- "plat_hadm_id"  #rename hadm_id for recognization after join


O2 <-sqldf("SELECT subject_id,hadm_id,charttime, value as o2 FROM  O2 WHERE o2 !=\"NA\"
                GROUP BY subject_id, charttime ")               
O2 <- O2[O2$o2!= 0,]     
names(O2)[2] <- "O2_hadm_id"

WBC <-sqldf("SELECT subject_id,hadm_id,charttime, value as wbc FROM wbc WHERE wbc !=\"NA\"
                GROUP BY subject_id, charttime ")               
WBC <- WBC[WBC$wbc!= 0,]     
names(O2)[2] <- "WBC_hadm_id"

#merging all the labtest together
Char_evnt1 <- merge(x = chest_x, y = Creati, all = TRUE)
Char_evnt2 <- merge(x = Char_evnt1, y = Creati2 , all = TRUE)
Char_evnt3 <- merge(x =Char_evnt2 , y =Urin_vol , all = TRUE)
Char_evnt4 <- merge(x = Char_evnt3 , y = lab_plat , all = TRUE)
Char_evnt5 <- merge(x = Char_evnt4 , y = O2, all = TRUE)
Char_evnt6 <- merge(x = Char_evnt5 , y = WBC, all = TRUE)



#STEP1 d-> create new column to hold hadm_id regardless comming from INR, PTT, PLAT table
Char_evnt6$hadm_id2 <- ifelse(!is.na(Char_evnt6$chest_x_hadm_id),Char_evnt6$chest_x_hadm_id,
                              ifelse(!is.na(Char_evnt6$creati_hadm_id),Char_evnt6$creati_hadm_id,
                                     ifelse(!is.na(Char_evnt6$creati2_hadm_id),Char_evnt6$creati2_hadm_id,
                                            ifelse(!is.na(Char_evnt6$Urin_hadm_id),Char_evnt6$Urin_hadm_id,
                                                   ifelse(!is.na(Char_evnt6$plat_hadm_id),Char_evnt6$plat_hadm_id,
                                                          ifelse(!is.na(Char_evnt6$O2_hadm_id),Char_evnt6$O2_hadm_id,
                                                                 ifelse(!is.na(Char_evnt6$WBC_hadm_id),Char_evnt6$WBC_hadm_id,-9999)))))))



Char_evnt6<- Char_evnt6[-c(3,5,7,9,11,13,15)]
getwd()
setwd("Users/Andy/Spring2019/HS614/")
write.csv(Char_evnt6,file ="Char_evnt6.csv",row.names=FALSE)

# comman prescriptions
#anticoag_drug <- dbGetQuery(con,"select subject_id, hadm_id, drug_name_generic
#                  from mimiciii.prescriptions where drug_name_generic in ('Argatroban', 'Apixaban','Fondaparinux','Dabigatran Etexilate', 'Rivaroxaban','Ticagrelor',
#                            'Prasugrel','Clopidogrel')")
#K_bind <- dbGetQuery(con, "SELECT subject_id,hadm_id, drug_name_generic from prescriptions where drug_name_generic like 'Kayexalate%'")

Diuretic <- dbGetQuery(con, "SELECT subject_id,hadm_id, drug_name_generic from prescriptions where drug_name_generic like 'Furosemide%'")

Albut <- dbGetQuery(con, "SELECT subject_id,hadm_id, drug_name_generic from prescriptions where drug_name_generic like '%Albuter%'")

Advair <- dbGetQuery(con, "SELECT subject_id,hadm_id, drug_name_generic from prescriptions where drug_name_generic like 'Fluticasone%'")

Budesonide <- dbGetQuery(con, "SELECT subject_id,hadm_id, drug_name_generic from prescriptions where drug_name_generic like '%Budesonide%'")

Losartan <- dbGetQuery(con, "SELECT subject_id,hadm_id, drug_name_generic from prescriptions where drug_name_generic like '%Losart%'")

Montelukast<- dbGetQuery(con, "SELECT subject_id,hadm_id, drug_name_generic from prescriptions where drug_name_generic like '%Montelukast%'")

## Prepare prescription for join
O2 <-sqldf("SELECT subject_id,hadm_id,charttime, value as o2 FROM  O2 WHERE o2 !=\"NA\"
                GROUP BY subject_id, charttime ")               
O2 <- O2[O2$o2!= 0,]     
names(O2)[2] <- "O2_hadm_id"


prescrip <- cbind(Diuretic,Albut,Advair,Budesonide,Losartan,Montelukast)
#labevent <-  dbGetQuery(con, "select * from mimiciii.LABEVENTS where itemid in ('50818','51109','51300','51082','50912','51265')")
#labevent<- labevent[-c(1,7,8,9)]
#all_lab <- merge(x=chartevent,y=labevent, x.all=TRUE )


#Getting all the patients from admissions table

admissions <-  dbGetQuery(con,"select subject_id, hadm_id,admittime, dischtime,insurance, language, religion, marital_status, ethnicity, diagnosis,
                          HOSPITAL_EXPIRE_FLAG,deathtime,HAS_CHARTEVENTS_DATA FROM mimiciii.admissions")
#ad<- dbGetQuery(con,"select * FROM mimiciii.admissions limit 5")

#-> create a table from patients table to extract patients gender and dob
gender_age <- dbGetQuery(con, "SELECT subject_id,gender,dob FROM mimiciii.patients ")
##a <- ICD_icustay_bleed[ICD_icustay_bleed$age_yrs < 18,]
#-> this will give us the complete demographic info table
admissions_gen <- merge(x = admissions, y = gender_age, all.x = TRUE)

#-> calculate age of patient by substracting admittime and dob.
admissions_gen$dobb <- format.Date(admissions_gen$dob,format= "%Y")
admissions_gen$admittimee <- format.Date(admissions_gen$admittime,format = "%Y")
admissions_gen$age_yrs <- (as.numeric(admissions_gen$admittimee))- (as.numeric(admissions_gen$dobb))


#calculate the total hospital stay 
admissions_gen$hosp_stay <- round(as.numeric(difftime(admissions_gen$dischtime,admissions_gen$admittime,units = "days")),digits = 0)
admissions_gen$deathtime <- as.character(admissions_gen$deathtime)


#STEP6g -> flag patients status as expired and not_expired 
#admissions_gen$deathtime[is.na (admissions_gen$deathtime)]<- 0
admissions_gen$patient_status<- ifelse(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}",admissions_gen$deathtime),"expired","not_expired")

#get only adults
admissions_gen <-admissions_gen [admissions_gen$age_yrs >=18,]
admissions_gen[] <- lapply(admissions_gen, function() ifelse(x>300, 90, x))


#GIT <- dbGetQuery(con, "select * from mimiciii.diagnoses_icd where icd9_code like '53%'")
#GIT$class <- 'GIT'

RESP <- dbGetQuery(con, "select * from mimiciii.diagnoses_icd where icd9_code like '47%' or icd9_code like '48%' or icd9_code like '49%' or icd9_code like '50%' or icd9_code like '51%'")
RESP$class <- 'RESP'

Renal <- dbGetQuery(con, "select * from mimiciii.diagnoses_icd where icd9_code like '40%' or icd9_code like '44%' or icd9_code like '58%' or icd9_code like '59%' or icd9_code like '75%'")
Renal$class <- 'Renal'
# join two clinical condition with demographic info
all_ICD <- rbind(Renal ,RESP)

data <- merge(x=all_ICD, y=admissions_gen,x.all=TRUE )
data<- data [-c(3,4,10,11,12,16,17,19,20,21,24)]

Resp_renal <-sqldf("SELECT D.*, l.charttime,l.chest_x, l.creat,l.creati2,l.urin_vol,l.min_plat,l.o2, l.wbc
                                          FROM data D
                                          LEFT JOIN Char_evnt6 l
                                          ON (l.subject_id=D.subject_id AND l.charttime >=D.admittime AND l.charttime <= D.dischtime)")


getwd()
setwd("Users/Andy/Spring2019/HS614/")
write.csv(data, file= "Resp_renal",row.names=FALSE)

names(Resp_renal)

#chartevent <- dbGetQuery(con, "select * from mimiciii.CHARTEVENTS where itemid ('3366','226748','1525','220615','1635','3605')")
#chartevent<- chartevent[-c(1,3,7,8,11,12,13,14,15)]
recent_Resp_renal <- sqldf("SELECT subject_id,hadm_id,intime,outtime,first_careunit,los,insurance,language,
              religion,marital_status,ethnicity,diagnosis,icd9_code,ICD_title,admittime,dischtime,discharge_location,gender,dob,age_yrs,hosp_stay,
              patient_status,charttime,min_plat,max_PTT,max_inr,max_inrcat
              from non_distinct_patient_INR_study
              WHERE (subject_id, admittime ) IN
              (select subject_id, MAX(admittime) AS max_admit_time 
                          FROM non_distinct_patient_INR_study
                          GROUP BY  subject_id)")


getwd()
#setwd("Users/Andy/Spring2019/HS614/")
write.csv(data, file= "data.csv",row.names=FALSE)