#analyses and plots

library(rio)
library(ggplot2)
library(scales)
library(dplyr)

wd = getwd()

output_dir = paste0(wd, "/outputs/talk/ita/")
input_dir = paste0(wd, "/data/talk/")

ospitate = import(paste0(input_dir, "ospitate.xlsx"))
ospiti = import(paste0(input_dir, "ospiti.xlsx"))

ospitate = ospitate |>
  select(Data, Trasmissione, id, Ospite_id)

data = merge(ospiti, ospitate, by.x = "Ospite_id", by.y = "Ospite_id", all = T)

data[is.na(data$id.x) ==T, ]

data[is.na(data$id.y) ==T, ]


#gender

#check for missing

sum(is.na(data$Genere)) == 0

#everything fine

#gender gap in ospitate
ggplot(data, aes(x=Genere))+
  geom_bar(aes(fill=Genere), show.legend = F)+
  scale_y_continuous(breaks = seq(0,850,by=100))+
  scale_x_discrete(labels = c("Women", "Men"))+
  xlab("Gender")+
  ylab("Number of talk show appearances")
ggplot(data, aes(x=Genere))+
  geom_bar(aes(fill=Genere), show.legend = F)+
  scale_y_continuous(breaks = seq(0,850,by=100))+
  scale_x_discrete(labels = c("Donne", "Uomini"))+
  xlab("Sesso")+
  ylab("Numero di partecipazioni ad un talk")

ggsave("gender_gap_ospitate_count.png", path=output_dir, width = 8, height = 8)

#gender gap in ospiti

ggplot(ospiti, aes(x=Genere))+
  geom_bar(aes(fill=Genere), show.legend = F)+
  scale_y_continuous(breaks = seq(0,270,by=30))+
  scale_x_discrete(labels = c("Donne", "Uomini"))+
  xlab("Sesso")+
  ylab("Numero di partecipazioni ad un talk")

ggsave("gender_gap_ospiti.png", path=output_dir, width = 8, height = 8)

# gender gap per trasmissione ospitate

ggplot(ospitate, aes(x=Genere))+
  geom_bar(aes(fill=Genere), show.legend = F)+
  facet_wrap(~Trasmissione, nrow = 3, scales = "free_y")+
  scale_x_discrete(labels = c("Donne", "Uomini"))+
  xlab("Sesso")+
  ylab("Numero di partecipazioni per talk show")

ggsave("gender_gap_per_trasmissione.png", path=output_dir, width = 8, height = 8)



#gender gap per trasmissione percentuale ospitate

df_percent <- data %>%
  group_by(Trasmissione, Genere) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total))

ggplot(df_percent, aes(x = Trasmissione, y = percentage, fill = Genere)) +
  geom_col() +
  labs(y = "Percentuale", x = "Trasmissione", fill = "Genere") +
  scale_y_continuous(breaks = seq(0,1,by=0.1), labels = label_percent())

  ggsave("gender_gap_per_trasmissione_percentuale_ospitate.png", 
       path=output_dir, width = 10, height = 8)

  
  #gender gap per trasmissione percentuale ospiti
  
  df_aus <- data %>%
    group_by(Trasmissione, Ospite, Genere) %>%
    summarise(count = n()) #%>%
  
  df_percent <- df_aus %>%
    group_by(Trasmissione, Genere) %>%
    summarise(count = n()) %>%
    mutate(total = sum(count)) %>%
    ungroup() %>%
    mutate(percentage = (count / total))
  
  ggplot(df_percent, aes(x = Trasmissione, y = percentage, fill = Genere)) +
    geom_col() +
    labs(y = "Percentuale (sui diversi ospiti)", x = "Trasmissione", fill = "Genere") +
    scale_y_continuous(breaks = seq(0,1,by=0.1), labels = label_percent())
  
  ggsave("gender_gap_per_trasmissione_percentuale_ospiti.png", 
         path=output_dir, width = 10, height = 8)
  
  

#età per ospitate

data$age = 2023-data$anno_di_nascita_guessed
ospiti$age = 2023-ospiti$anno_di_nascita_guessed

ggplot(data, aes(x=age))+
  geom_histogram(binwidth = 5,fill='blue', col='black')+
  xlab("Età")+
  ylab("Numero di partecipazioni ad un talk")

ggsave("age_ospitate.png", path=output_dir, width = 8, height = 8)

#età per ospiti

ggplot(ospiti, aes(x=age))+
  geom_histogram(binwidth = 5,fill='blue', col='black')+
  xlab("Età")+
  ylab("Ospiti dei talk")

ggsave("age_ospiti.png", path=output_dir, width = 8, height = 8)

#età per ospitate per programma

ggplot(data, aes(x=age))+
  geom_histogram(binwidth = 5,fill='blue', col='black')+
  facet_wrap(~Trasmissione, nrow=3, scale = "free_y")+
  xlab("Età")+
  ylab("Numero di partecipazioni per talk")

ggsave("age_ospitate_per_talk.png", path=output_dir, width = 8, height = 8)

### professione

aus = data.frame(Professione= c(data$Professione1, data[!is.na(data$Professione2), ]$Professione2),
                 Trasmissione = c(data$Trasmissione, data[!is.na(data$Professione2), ]$Trasmissione))

aus$Professione <- factor(aus$Professione, levels = names(sort(table(aus$Professione), decreasing = TRUE)))

ggplot(aus, aes(x=Professione))+
  geom_bar(aes(fill=Professione), show.legend = F)+
  scale_x_discrete(labels = c("Giornalista", "Politico/a", "Altro",
                              "Storico/a", "Filosofo/a", "Politologo/a",
                              "Economista", "Giurista", "Sociologo/a"))+
  ylab("Numero di ospitate\n(contando doppie professioni)")

ggsave("ospitate_per_professione.png", path=output_dir, width = 8, height = 8)

ggplot(aus, aes(x=Professione))+
  geom_bar(aes(fill=Professione), show.legend = F)+
  scale_x_discrete(labels = c("Giornalista", "Politico/a", "Altro",
                     "Storico/a", "Filosofo/a", "Politologo/a",
                     "Economista", "Giurista", "Sociologo/a"))+
  ylab("Numero di ospitate\n(contando doppie professioni)")+
  facet_wrap(~Trasmissione, scale = "free_y")
                     
ggsave("ospitate_per_professione_per_programma.png", path=output_dir, width = 15, height = 8)



aus = aus |>
  mutate(
    Professione, 
    Professione_r = case_when(
      Professione == "giornalist" ~ "giornalist",
      Professione == "politic" ~ "politic",
      Professione == "altr" ~ "altr",
      TRUE ~ "accademic"
    )
  )

aus$Professione_r <- factor(aus$Professione_r, levels = names(sort(table(aus$Professione_r), decreasing = TRUE)))


ggplot(aus, aes(x=Professione_r))+
  geom_bar(aes(fill=Professione_r), show.legend = F)+
  scale_x_discrete(labels = c("Giornalista", "Politico/a", "Altro",
                              "Accademico/a"))+
  ylab("Numero di ospitate\n(contando doppie professioni)")+
  xlab("Professione")

ggsave("ospitate_per_professione_recoded.png", path=output_dir, width = 8, height = 8)

#lo stesso grafico di sopra ma diviso per programma


ggplot(aus, aes(x=Professione_r))+
  geom_bar(aes(fill=Professione_r), show.legend = F)+
  scale_x_discrete(labels = c("Giornalista", "Politico/a", "Altro",
                              "Accademico/a"))+
  ylab("Numero di ospitate\n(contando doppie professioni)")+
  xlab("Professione")+
  facet_wrap(~Trasmissione, ncol = 3, scale = "free_y")

ggsave("ospitate_per_professione_recoded_per_trasmissione.png", path=output_dir, width = 8, height = 8)

#ospiti per professione


aus = data.frame(Professione= c(ospiti$Professione1, 
                                ospiti[!is.na(ospiti$Professione2), ]$Professione2))

aus$Professione <- factor(aus$Professione, levels = names(sort(table(aus$Professione), decreasing = TRUE)))

ggplot(aus, aes(x=Professione))+
  geom_bar(aes(fill=Professione), show.legend = F)+
  scale_x_discrete(labels = c("Giornalista", "Politico/a", "Altro",
                              "Storico/a", "Filosofo/a", "Politologo/a",
                              "Economista", "Giurista", "Sociologo/a"))+
  ylab("Singoli ospiti\n(contando doppie professioni)")+
  scale_y_continuous(breaks = seq(0,150, by=15))

ggsave("ospiti_per_professione.png", path=output_dir, width = 8, height = 8)

aus = aus |>
  mutate(
    Professione, 
    Professione_r = case_when(
      Professione == "giornalist" ~ "giornalist",
      Professione == "politic" ~ "politic",
      Professione == "altr" ~ "altr",
      TRUE ~ "accademic"
    )
  )

aus$Professione_r <- factor(aus$Professione_r, levels = names(sort(table(aus$Professione_r), decreasing = TRUE)))


ggplot(aus, aes(x=Professione_r))+
  geom_bar(aes(fill=Professione_r), show.legend = F)+
  scale_x_discrete(labels = c("Giornalista", "Politico/a", "Altro",
                              "Accademico/a"))+
  ylab("Singoli ospiti\n(contando doppie professioni)")+
  scale_y_continuous(breaks = seq(0,150, by=15))+
  xlab("Professione")

ggsave("ospiti_per_professione_recoded.png", path=output_dir, width = 8, height = 8)


#quelli di prima con le percentuali?
