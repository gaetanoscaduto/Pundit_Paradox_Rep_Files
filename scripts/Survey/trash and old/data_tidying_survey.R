library(rio)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/negri_scaduto_SISP")

data = import(paste0(getwd(),"/data/survey/data_parsed.RDS"), encoding = "UTF-8")

#remove those too fast

plot(data$Tempo.totale) 
quantile(data$Tempo.totale, probs = seq(0,1,0.01))

data = data |> filter(Tempo.totale>80 & Tempo.totale<920)
#exclude who failed checks
prop.table(table(data$check1))

data = data |> filter(check1=="Esteri")

prop.table(table(data$check2))

data = data |> filter(check2=="Un calciatore")


toexclude = which(grepl("Tempo.per.la.domanda..gender", names(data)))
toexclude = toexclude[1]
toexclude = seq(toexclude, ncol(data), 1)

data = data[-toexclude]

#View(data)
#####
### CHECK MISSING

data = data[!is.na(data$submitdate), ] #Note that in doing so you remove those that
#did not complete all the questionnaire but maybe part of it yes

# see how many only partially completed the survey (no second page)

nrow(data[!is.na(data$competence_conflitti), ])

datanomiss=na.omit(data[-which(grepl("followup", names(data)))])

if(sum(!(datanomiss==data[-which(grepl("followup", names(data)))]))!=0)
{
  stop("Occhio ai missing!")
}

#####
###tidying the column names (question_subquestion)

#####
colnames(data) <- gsub("\\[", "_", colnames(data)) # Replace '[' with '_'
colnames(data) <- gsub("\\]", "", colnames(data))  # Remove ']'

print(colnames(data))




####### 
#gender
######
data = data |>
  mutate(gender, 
         gender_r = case_when(
           gender == "Maschio" ~ "Maschio",
           gender == "Femmina" ~ "Femmina",
           gender != "Maschio" & gender != "Femmina" ~ NA,
           #IGNORO NON BINARIO? CHIEDERE PROF NEGRI
           is.na(gender) ~ NA
         ) 
  )

table(data$gender, data$gender_r)

######
#### age
#####
table(data$age)
plot(data$age)

data$age_r = as.numeric(as.character((data$age)))

data$age_r2 = ifelse(data$age_r >=60, "Anziano", 
                     ifelse(data$age_r<=35, "Giovane", "Adulto"))

table(data$age, data$age_r2)

table(data$age_r2)

#####
####### region
#####

data = data |>
  mutate(region, 
         macroarea1 = case_when(
           region %in% c("Lombardia", "Piemonte", "Val d'Aosta", "Liguria") ~ "Nord-ovest",
           region %in% c("Veneto", "Trentino-Alto Adige", "Emilia-Romagna", "Friuli-Venezia Giulia") ~ "Nord-est",
           region %in% c("Lazio", "Marche", "Toscana", "Umbria") ~ "Centro",
           region %in% c("Abruzzo", "Basilicata", "Molise", "Calabria", "Campania", "Puglia", "Sardegna", "Sicilia") ~ "Sud e isole",
           region == "Non abito in Italia" ~ NA,
           is.na(region) ~ NA
         ) 
  )

table(data$region, data$macroarea1)

table(data$macroarea1)

######
########## education 
######

data = data |>
  mutate(educ, 
         educ_r = case_when(
           grepl("diploma", educ, ignore.case = T) | grepl("licenza", educ, ignore.case = T)  ~ "Low", #Low
           grepl("laurea", educ, ignore.case = T) | grepl("dottorato", educ, ignore.case = T)  ~ "High", #High
           is.na(educ) ~ NA
         ) 
  )

data$educ_r = factor(data$educ_r, levels = c("Low", "High"))

table(data$educ, data$educ_r)

table(data$educ_r)

##### 
############# social position
#####
data = data |>
  mutate(socposition_SQ001, 
         social_position_r = case_when(
           grepl("0", socposition_SQ001, ignore.case = T) & !grepl("10", socposition_SQ001, ignore.case = T) ~0,
           grepl("10", socposition_SQ001, ignore.case = T) ~ 10,
           socposition_SQ001 %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") ~ as.numeric(socposition_SQ001),
           is.na(socposition_SQ001) ~ NA,
           socposition_SQ001 == "Non saprei" ~ NA 
         ) 
      ) 

table(data$socposition_SQ001, data$social_position_r, useNA="always")

######
################ interest in politics
######

data$interest_r = as.numeric(str_extract(data$interest, "\\d+"))

table(data$interest, data$interest_r, useNA="always")

#####
######### media exposure
#####

data = data |>
  mutate(exposure, 
         exposure_r1 = case_when(
           exposure == "Più di due ore" ~ "High",
           exposure == "Fra una e due ore" ~ "High",
           exposure == "Fra mezz'ora e un'ora" ~ "High",
           exposure == "Fra dieci minuti e mezz'ora" ~ "High",
           exposure == "Meno di dieci minuti" ~ "Low",
           exposure == "Non lo faccio mai" ~ "Low",
           is.na(exposure) ~ NA
         ) 
  ) 

data$exposure_r1 = factor(data$exposure_r1, levels = c("Low", "High"))
table(data$exposure, data$exposure_r1)

#####
########Political ideology: only numbers (creates NAs!)
#####

data$ideology_r = as.numeric(str_extract(data$ideology_SQ001, "\\d+"))

table(data$ideology_r, data$ideology_SQ001, useNA="always")

#####
######### Political ideology: recode with 4 cathegories (including nowhere)
#####

data = data |>
  mutate(ideology_SQ001, 
         ideology_r2 = 
           case_when(
             ideology_SQ001 == "10 - Estrema destra" ~ "Destra",
             ideology_SQ001 == "9" ~  "Destra",
             ideology_SQ001 == "8" ~  "Destra",
             ideology_SQ001 == "7" ~  "Destra",
             ideology_SQ001 == "6" ~  "Destra",
             ideology_SQ001 == "5 - Centro" ~ "Centro",
             ideology_SQ001 == "4" ~ "Sinistra",
             ideology_SQ001 == "3" ~ "Sinistra",
             ideology_SQ001 == "2" ~ "Sinistra",
             ideology_SQ001 == "1" ~ "Sinistra",
             ideology_SQ001 == "0 - Estrema sinistra" ~ "Sinistra",
             ideology_SQ001 == "Da nessuna parte" ~ "Da nessuna parte",  
             is.na(ideology_SQ001) ~ NA
           ) 
  )

data$ideology_r2 = factor(data$ideology_r2, levels = c("Centro", "Da nessuna parte", "Destra", "Sinistra"))
table(data$ideology_SQ001, data$ideology_r2)

table(data$ideology_r2)

##DA RICONTROLLARE

#####
# Outgroup, ingroup POLITICO
#####

data$outgroup = ifelse(data$ideology_r2 =="Centro", "Destra e Sinistra",
                       ifelse(data$ideology_r2=="Destra", "Centro e Sinistra",
                              ifelse(data$ideology_r2=="Sinistra", "Centro e Destra",
                                            ifelse(grepl("destra",data$ideologyfollowup, ignore.case = T), "Destra", "Sinistra"))))

table(data$outgroup, data$ideology_r2)

data$ingroup = data$ideology_r2

table(data$ingroup)

i=1

while(i<=nrow(data))
{  
  if(grepl("Da nessuna parte", data$ideology_r2[i], ignore.case = T))
  {
    data$ingroup[i] = ifelse(grepl("destra",data$ideologyfollowup[i], ignore.case = T), 
                        "Sinistra", "Destra")
  }
  i=i+1
} 
table(data$ingroup)
table(data$ideology_r2, data$ingroup)


#####
# Outgroup, ingroup, GENDER
#####


data$outgroup_gender = ifelse(data$gender_r =="Maschio", "Femmina","Maschio")

table(data$outgroup_gender, data$gender_r)

data$ingroup_gender = data$gender_r

data$ingroup_gender=ifelse(data$ingroup_gender=="Femmina", "F", "M")

table(data$ingroup_gender, data$gender_r)

#####
# Outgroup, ingroup, AGE
#####

data$outgroup_age = ifelse(data$age_r2 =="Adulto", "Giovane e anziano",
                           ifelse(data$age_r2 == "Giovane", "Adulto e anziano",
                                  "Giovane e adulto"))

table(data$outgroup_age, data$age_r2)

data$ingroup_age = data$age_r2

table(data$ingroup_age, data$age_r2)

data$ingroup_age=ifelse(data$ingroup_age=="Giovane", "37", 
                        ifelse(data$ingroup_age=="Adulto", "52", "67"))

table(data$ingroup_age, data$age_r2)

#####
# Outgroup, ingroup, MACROREGION
#####

data$macroregion = ifelse(data$macroarea1=="Nord-est" | data$macroarea1 == "Nord-ovest",
                          "Nord", data$macroarea1)

data$macroregion = ifelse(data$macroregion=="Sud e isole",
                          "Sud", data$macroregion)

table(data$macroarea1, data$macroregion)

data$outgroup_macroregion = ifelse(data$macroregion =="Nord", "Centro e sud",
                           ifelse(data$macroregion == "Sud", "Centro e nord",
                                  "Nord e sud"))

table(data$outgroup_macroregion, data$macroregion)

data$ingroup_macroregion = data$macroregion

table(data$ingroup_macroregion, data$macroregion)

data$ingroup_macroregion=ifelse(data$ingroup_macroregion=="Sud", "Sud Italia", 
                        ifelse(data$ingroup_macroregion=="Nord", "Nord Italia", "Centro Italia"))

table(data$ingroup_macroregion, data$macroregion)


#####
############## trust
#####

### gove

data$trust_gove_r = as.numeric(str_extract(data$trust_gove, "\\d+"))

table(data$trust_gove_r, data$trust_gove, useNA="always")

### parties

data$trust_part_r = as.numeric(str_extract(data$trust_part, "\\d+"))

table(data$trust_part_r, data$trust_part, useNA="always")

### media

data$trust_medi_r = as.numeric(str_extract(data$trust_medi, "\\d+"))

table(data$trust_medi_r, data$trust_medi, useNA="always")

### experts

data$trust_expe_r = as.numeric(str_extract(data$trust_expe, "\\d+"))

table(data$trust_expe_r, data$trust_expe, useNA="always")

# ### associations 
# 
# data <- data |>
#   mutate(
#     across(
#       starts_with("associations"),
#       ,
#       .names = "{.col}__r"
#     )
#   )



export(data, paste0(getwd(), "/data/survey/","data_survey_ready.RDS"))

##################################################################
################################
# Make it a conjoint dataset
#####

ntask = 8

nprofiles = ntask*2

#last_pos_before_gender_11 = which(names(data)=="gender11")-1

cjdata = data.frame("id" = data$id)
cjdata$task_number = "task_number"
cjdata$profile_number = "profile_number"
#cjdata = cbind(cjdata[1:5],cjdata[which(names(data)=="gender_r"):ncol(data)])
cjdata$c_gender = "c_gender"
cjdata$c_age = "c_age"
cjdata$c_prov = "c_prov"
cjdata$c_job = "c_job"
cjdata$c_ideo = "c_ideo"

cjdata$chosen = "chosen"

names(data)
for(i in 1:nrow(data))
{
  for(j in 1:nprofiles)
  {
    cjdata[nprofiles*(i-1)+j, "id"] = data[i, "id"]
    cjdata[nprofiles*(i-1)+j, "task_number"] = (j+1)%/%2 
    cjdata[nprofiles*(i-1)+j, "profile_number"] = (j-1)%%2 + 1
    cjdata[nprofiles*(i-1)+j, "c_gender"] = data[i, which(names(data)=="gender11")-1+j]
    cjdata[nprofiles*(i-1)+j, "c_age"] = data[i, which(names(data)=="age11")-1+j]
    cjdata[nprofiles*(i-1)+j, "c_prov"] = data[i, which(names(data)=="prov11")-1+j]
    cjdata[nprofiles*(i-1)+j, "c_ideo"] = data[i, which(names(data)=="ideo11")-1+j]
    cjdata[nprofiles*(i-1)+j, "c_job"] = data[i, which(names(data)=="job11")-1+j]
    cjdata[nprofiles*(i-1)+j, "chosen"] = ifelse(j%%2==1 && grepl("Ospite 1", data[i, paste0("task", as.character((j+1)%/%2))]), 1,
                                                 ifelse(j%%2==0 && grepl("Ospite 2", data[i, paste0("task", as.character((j+1)%/%2))]), 1, 0))
    
    }
}


#setting ingroup/outgroup


cjdata$chosen = as.numeric(cjdata$chosen)

cjdata$c_gender_r = ifelse(cjdata$c_gender==1, "F", "M")

cjdata$c_gender_r = factor(cjdata$c_gender_r)

table(cjdata$c_gender, cjdata$c_gender_r)


cjdata$c_age_r = ifelse(cjdata$c_age==1, "37",
                      ifelse(cjdata$c_age==2, "52", "67"))

cjdata$c_age_r = factor(cjdata$c_age_r)

table(cjdata$c_age_r)


cjdata$c_prov_r = ifelse(cjdata$c_prov==1, "Centro Italia",
                      ifelse(cjdata$c_prov==2, "Nord Italia", "Sud Italia"))

cjdata$c_prov_r = factor(cjdata$c_prov_r)

table(cjdata$c_prov_r)

cjdata$c_job_r = ifelse(cjdata$c_job==1, "Economista",
                         ifelse(cjdata$c_job==2, "Filosofo/a", 
                                ifelse(cjdata$c_job==3, "Giornalista",
                                       ifelse(cjdata$c_job==4, "Giurista", "Politologo/a"))))

cjdata$c_job_r = factor(cjdata$c_job_r, levels = c("Giornalista", "Economista", "Filosofo/a",
                                                   "Giurista", "Politologo/a"))

table(cjdata$c_job_r, cjdata$c_job)

cjdata$c_ideo_r = ifelse(cjdata$c_ideo==1, "Non note",
                         ifelse(cjdata$c_ideo==2, "Centro", 
                                ifelse(cjdata$c_ideo==3, "Destra", "Sinistra")))
                                       
cjdata$c_ideo_r = factor(cjdata$c_ideo_r, levels = c("Non note", "Centro", "Destra", "Sinistra"))

table(cjdata$c_ideo_r, cjdata$c_ideo)

cjdata = merge(cjdata, data)
cjdata$c_inoutgr = "c_inoutgr"

for(i in 1:nrow(cjdata))
{
  cjdata$c_inoutgr[i] = ifelse(grepl("Non note", cjdata$c_ideo_r[i]), "Non note", 
         ifelse(grepl(cjdata$c_ideo_r[i], cjdata$outgroup[i], ignore.case = T), "Outgroup", "Ingroup"))
}

table(cjdata$c_inoutgr)



cjdata$c_inoutgr_gender = ifelse(cjdata$ingroup_gender == cjdata$c_gender_r, "Gender ingroup", "Gender outgroup")


table(cjdata$c_inoutgr_gender, cjdata$c_gender_r, cjdata$ingroup_gender)



cjdata$c_inoutgr_age = ifelse(cjdata$ingroup_age == cjdata$c_age_r, "Age ingroup", "Age outgroup")


table(cjdata$c_inoutgr_age, cjdata$c_age_r, cjdata$ingroup_age)



cjdata$c_inoutgr_macroregion = ifelse(cjdata$ingroup_macroregion == cjdata$c_prov_r, "Macroregional ingroup", "Macroregional outgroup")


table(cjdata$c_inoutgr_macroregion, cjdata$c_prov_r, cjdata$ingroup_macroregion)


export(cjdata, paste0(getwd(), "/data/survey/","cjdata_ready.RDS"))

export(cjdata, "C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/Pundit_Paradox_Rep_Files/data/survey/cjdata_ready.RDS")
