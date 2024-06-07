##### From coded data to clean data
library(rio)
library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)

wd = getwd()

output_dir = paste0(wd, "/outputs/talk/")
input_dir = paste0(wd, "/data/talk/")

ospiti = import(paste0(input_dir, "ospiti_april.xlsx"))
ospitate = import(paste0(input_dir, "Ospitate_april.xlsx"))
ospitate = ospitate[c(1,3,4)]
ospitate1 = ospitate

#elimina gli errori di lorenzo che indica due volte le persone nelle tramsissioni
#nello stesso giorno


ospitate = ospitate1 |>
  group_by(Ospite, Trasmissione, Date_fixed) |>
  summarise(n=n())

nrow(ospitate[ospitate$n>1, ])

rm(ospitate1)
ospitate$n = NULL
#Butto la colonna che mi segnala l'errore dell'ospite riportato due volte

data1= merge(ospitate, ospiti, by="Ospite", all = TRUE)

data2 = data1 |> 
  group_by(Ospite, Trasmissione, Date_fixed) |>
  summarise(n=n())

#View(data2[data2$n>1, ])

data2= merge(ospitate, ospiti, all = TRUE)
data3= merge(ospitate, ospiti, all.x = TRUE)

data2[!(data2$Ospite %in% unique(ospiti$Ospite)), ]



#View(data4[data4$n>1, ])

ospiti[!(ospiti$Ospite %in% unique(ospitate$Ospite)), ]

sum(is.na(data3$Date_fixed))
sum(is.na(data3$`Pagina wikipedia`))

rm(data1)
rm(data2)
rm(data3)

ospiti$anno_di_nascita_guessed = ifelse(is.na(ospiti$anno_di_nascita_guessed),
                                        ospiti$anno_di_nascita,
                                        ospiti$anno_di_nascita_guessed)

ospiti$age_guessed = 2023-as.numeric(ospiti$anno_di_nascita_guessed)


table(ospiti$age_guessed, useNA = "always")


ospiti = ospiti |>
  mutate(
    age_guessed, 
    age_guessed_r = case_when(
      age_guessed <= 40 ~ "Less than 40 years old",
      age_guessed > 40 & age_guessed <=60 ~ "Between 40 and 60 years old",
      age_guessed > 60 ~ "Above 60 years old"
    )
  )

ospiti$age_guessed_r = factor(ospiti$age_guessed_r, 
                              levels =  c("Less than 40 years old",
                                          "Between 40 and 60 years old",
                                          "Above 60 years old"))
table(ospiti$age_guessed_r)


ospiti = ospiti |>
  mutate(
    Professione1, 
    Professione_r = case_when(
      Professione1 == "giornalist" ~ "Journalist",
      Professione1 == "politic" ~ "Politician",
      Professione1 == "altr" ~ "Other",
      TRUE ~ "Scholar\n(social sciences/humanities)"
    )
  )


table(ospiti$Professione_r)
data= merge(ospitate, ospiti, all.x = TRUE)

write.xlsx(ospitate, paste0(input_dir,"Ospitate_correct.xlsx"))

data$Professione1 = tolower(data$Professione1)
data$Professione2 = tolower(data$Professione2)


#also check al the missings!
length(data[is.na(data$Professione1), ]$Ospite)

data[is.na(data$Professione1), ]$Ospite

data$Professione1 = ifelse(data$Professione1 =="politica" | data$Professione1 == "politico", 
                           "politic",
                           data$Professione1)
table(data$Professione1)


#check them at hand
# 
# data[data$Ospite=="Erica Patti", ]$Professione1 = "altro"
# data[data$Ospite=="Daniela Di Maggio", ]$Professione1 = "altro"
# data[data$Ospite=="Bianca Tedone", ]$Professione1 = "politica"
# data[data$Ospite=="Alessandro Di Battista", ]$Professione1 = "politico"
# data[data$Ospite=="Licia Ronzulli", ]$Professione1 = "politica"
# data[data$Ospite=="Paola Tommasi", ]$Professione1 = "giornalista"
# data[data$Ospite=="Elisabetta Ponzani", ]$Professione1 = "altro"
# data[data$Ospite=="Pasquale Guadagno", ]$Professione1 = "altro"
# data[data$Ospite=="Stefania Matteuzzi", ]$Professione1 = "altro"
# data[data$Ospite=="Saif Eddine Abouabid", ]$Professione1 = "altro"
# data[data$Ospite=="Jumana Shahin", ]$Professione1 = "altro"
# data[data$Ospite=="Andrea Grieco", ]$Professione1 = "altro"
# data[data$Ospite=="Andrea Favozza", ]$Professione1 = "altro"
# 
# data[is.na(data$Professione1), ]$Ospite
# 
# ### Fix Paolo mieli
# 
# data$Professione1 = ifelse(data$Ospite=="Paolo Mieli", "storico", data$Professione1)
# data$Professione2 = ifelse(data$Ospite=="Paolo Mieli", "giornalista", data$Professione2)
# 
# data[data$Ospite == "Paolo Mieli", ]$Professione1
# data[data$Ospite == "Paolo Mieli", ]$Professione2
# 
# ### Fix Italo Bocchino
# 
# 
# data$Professione1 = ifelse(data$Ospite=="Italo Bocchino", "giornalista", data$Professione1)
# data$Professione2 = ifelse(data$Ospite=="Italo Bocchino", "politico", data$Professione2)
# 
# data[data$Ospite == "Italo Bocchino", ]$Professione1
# data[data$Ospite == "Italo Bocchino", ]$Professione2
# 
# ### Fix gender for Andrea e Nicola
# 
# data[grepl("Andrea", data$Ospite), ]$Genere = "M" 
# data[grepl("Nicola", data$Ospite), ]$Genere = "M" 
# data[grepl("Luca", data$Ospite), ]$Genere = "M" 
# data[grepl("Saif", data$Ospite), ]$Genere = "M" 
# 
# #Are there any other contested names?

split_names <- strsplit(data$Ospite, " ")

data$name <- sapply(split_names, `[`, 1)
data$surname <- sapply(split_names, `[`, 2)

data$is_male = (data$Genere == "M")
data$is_female = (data$Genere == "F")

aus=data.frame(table(data$name, data$is_male))

names(aus) = c("Nome", "x", "Freq_1")
aus1 =data.frame(table(data$name, data$is_female))
names(aus1) = c("Nome", "x", "Freq_2")


i=1
ok=0
while(i<=nrow(aus))
{
  ifelse(aus$Freq_1[i] == 0 | aus1$Freq_2[i]==0, ok+1, print(paste0("Occhio", i)))
  i=i+1
}

# aus[126, ]#Luca!
# aus[188, ]#Saif
# 
# data[data$name == "Saif", ]

#Are there any missing in genere?
missing_gender = data[is.na(data$Genere), ]$Ospite

missing_gender

# data[data$Ospite %in% missing_gender, ]$Genere = "M"

### TIDY PROFESSIONS
table(data$Professione1)


# data$Professione1 = substr(data$Professione1, 1, nchar(data$Professione1) - 1)
# data$Professione2 = substr(data$Professione2, 1, nchar(data$Professione2) - 1)

table(data$Professione1)

# #correct typos
# data[grepl("politoc", data$Professione1), ]$Professione1 = "politic"
# data[grepl("gionalist", data$Professione1), ]$Professione1 = "giornalist"
# data[grepl("gironalist", data$Professione1), ]$Professione1 = "giornalist"
# data[grepl("giornlist", data$Professione1), ]$Professione1 = "giornalist"
# data[grepl("giornalit", data$Professione1), ]$Professione1 = "giornalist"
# 

#correct scienziata politica and scienziato politico

# data$Professione1 = ifelse(grepl("scienziata politic", data$Professione1) | grepl("scienziato politic", data$Professione1), 
#                            "politolog",
#                            data$Professione1)


table(data$Professione1)


#same for professione2


table(data$Professione2)
# 
# #correct typos
# data[grepl("politoc", data$Professione2), ]$Professione2 = "politic"
# data[grepl("gionalist", data$Professione2), ]$Professione2 = "giornalist"
# data[grepl("gironalist", data$Professione2), ]$Professione2 = "giornalist"
# data[grepl("giornlist", data$Professione2), ]$Professione2 = "giornalist"
# data[grepl("giornalit", data$Professione2), ]$Professione2 = "giornalist"


#correct scienziata politica and scienziato politico
# 
# data$Professione2 = ifelse(grepl("scienziata politic", data$Professione2) | grepl("scienziato politic", data$Professione2), 
#                            "politolog",
#                            data$Professione2)
# 

# table(data$Professione2)
# 
# data[grepl("avvocat", data$Professione2), ]$Professione2 = "altr"
# 
# data[grepl("cantant", data$Professione2), ]$Professione2 = "altr"
# 
# table(data$Professione2)
#### Is there different coding between me and fedra? Or other inconsistencies?

rm(aus)
rm(aus1)

full = data %>%
  group_by(Ospite) %>%
  summarize(Professions = toString(unique(Professione1)))

full

inconsistencies <- data %>%
  group_by(Ospite) %>%
  summarize(Professions = toString(unique(Professione1))) %>%
  filter(lengths(strsplit(Professions, ",\\s*")) > 1)

inconsistencies
# DISCUSS THESE INCONSISTENCIES WITH @FEDRA

#my solutions
# 
# data[data$Ospite == "Alessandra Moretti", ]$Professione1 = "politic"
# data[data$Ospite == "Corrado Augias", ]$Professione1 = "giornalist"
# data[data$Ospite == "Francesca Barra", ]$Professione1 = "politic"
# data[data$Ospite == "Francesco Specchia", ]$Professione1 = "giornalist"
# data[data$Ospite == "Matilde Siracusano", ]$Professione1 = "politic"
# data[data$Ospite == "Matteo Piantedosi", ]$Professione1 = "politic"
# data[data$Ospite == "Nicola Gratteri", ]$Professione1 = "giurist" #DISCUSS!!!
# data[data$Ospite == "Paolo Crepet", ]$Professione1 = "altr"
# data[data$Ospite == "Rita Dalla Chiesa", ]$Professione1 = "politic"
# data[data$Ospite == "Tomaso Montanari", ]$Professione1 = "altr"
# 
#recheck


inconsistencies <- data %>%
  group_by(Ospite) %>%
  summarize(Professions = toString(unique(Professione1))) %>%
  filter(lengths(strsplit(Professions, ",\\s*")) > 1)

inconsistencies

#same with professione2


full2 = data %>%
  group_by(Ospite) %>%
  summarize(Professions = toString(unique(Professione1)))

inconsistencies2 <- data %>%
  group_by(Ospite) %>%
  summarize(Professions = toString(unique(Professione2))) %>%
  filter(lengths(strsplit(Professions, ",\\s*")) > 1)

inconsistencies2

# #my solution to inconsistencies
# 
# data[data$Ospite == "Alessandra Moretti", ]$Professione2 = "altr"
# data[data$Ospite == "Alessandro Cecchi Paone", ]$Professione2 = "politic"
# data[data$Ospite == "Alessandro Di Battista", ]$Professione2 = "giornalist"
# data[data$Ospite == "Antonio Scurati", ]$Professione2 = "giornalist"
# data[data$Ospite == "Corrado Augias", ]$Professione2 = "storic"
# data[data$Ospite == "Flavia Perina", ]$Professione2 = "politic"
# data[data$Ospite == "Hoara Borselli", ]$Professione2 = "giornalist"
# data[data$Ospite == "Lucio Caracciolo", ]$Professione2 = "politolog"
# data[data$Ospite == "Luigi De Magistris", ]$Professione2 = "altr" #DISCUSS AND BE
# #CONSISTENT WITH GRATTERI
# data[data$Ospite == "Mauro Corona", ]$Professione2 = NA ###sure?
# data[data$Ospite == "Rita Dalla Chiesa", ]$Professione2 = "giornalist"
# 
# #recheck
# inconsistencies2 <- data %>%
#   group_by(Ospite) %>%
#   summarize(Professions = toString(unique(Professione2))) %>%
#   filter(lengths(strsplit(Professions, ",\\s*")) > 1)
# 
# inconsistencies2
# 
# 
# #DISCUSS THESE INCONSISTENCIES WITH @FEDRA
# 
# #check year date

data$anno_di_nascita = as.character(round(as.numeric(data$anno_di_nascita), digits=0))
table(data$anno_di_nascita)

# 
# #aggiustiamo quelli inconsistent
# 
# i=1
# 
# while(i<=nrow(data))
# {
#   if(is.na(data$anno_di_nascita[i]))
#   {
#     j=1
#     
#     aus = data[data$Ospite == data$Ospite[i], ]
#     
#     while(j<=nrow(aus))
#     {
#       if(!is.na(aus$anno_di_nascita[j]))
#         data$anno_di_nascita[i] = aus$anno_di_nascita[j]
#       j=j+1
#     }
#   }
#   i=i+1
#   
# }
# 
# data[is.na(data$anno_di_nascita), ]$Ospite

#da aggiustarea mano almeno quelli più famosi. Gli altri? Codificare ad occhio?
#CHIEDI


ggplot(data, aes(x=anno_di_nascita))+
  geom_bar()

#perché c'è l'anomalia del 1974?

#View(data[data$anno_di_nascita == "1974" & !is.na(data$anno_di_nascita), ])

# aus = data |> 
#   filter(anno_di_nascita==1974) |>
#   select(Ospite, Data) |>
#   group_by(Ospite) |>
#   summarise(n=n())
# 
# aus

#l'anomalia è scanzi 23 volte forse

#qualche ospite Na?

data[is.na(data$Ospite), ]

#typo o inconstencies nel nome della trasmissione?

table(data$Trasmissione, useNA = "always")

#Carta Bianca!

#data[data$Trasmissione== "Carta Bianca", ]$Trasmissione = "È sempre Carta Bianca"

table(data$Trasmissione, useNA = "always")


#Ci sono errori in genere?

table(data$Genere, useNA = "always")
prop.table(table(data$Genere, useNA = "always"))

#cazz che bias

#ne abbiamo dimenticato qualcuno?

table(data$`Fatto?`, useNA = "always")

data[is.na(data$`Fatto?`), ]

## non sembra ci siano grossi probemi, li segno a s
data[is.na(data$`Fatto?`), ]$`Fatto?` = "s"

data[is.na(data$`Fatto?`), ]

## 

ggplot(data, aes(x=Professione1))+
  geom_bar()

table(data$anno_di_nascita_guessed, useNA = "always")
table(data$anno_di_nascita, useNA = "always")

data$anno_di_nascita_guessed = ifelse(is.na(data$anno_di_nascita_guessed),
                                      data$anno_di_nascita,
                                      data$anno_di_nascita_guessed)

data[which(is.na(data$anno_di_nascita_guessed)), ]$Ospite

table(data$anno_di_nascita_guessed, useNA = "always")

write.xlsx(data, paste0(input_dir,"data_to_analyze.xlsx"))
