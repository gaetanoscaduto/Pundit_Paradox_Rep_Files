#GRAFICI UFFICIALI

library(dplyr)
library(rio)
library(ggplot2)
library(scales)
library(patchwork)
library(lubridate)

wd = "C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/negri_scaduto_SISP"

output_dir_graph = paste0(wd, "/outputs/talk/ita/grafici_def")
output_dir_table = paste0(wd, "/outputs/talk/ita/tabelle_def/")
input_dir = paste0(wd, "/data/talk/")

ospitate = import(paste0(input_dir, "ospitate.xlsx"))
ospiti = import(paste0(input_dir, "ospiti.xlsx"))


ospiti$age_guessed = 2023 - ospiti$anno_di_nascita_guessed

table(ospiti$age_guessed, useNA = "always")


ospiti = ospiti |>
  mutate(
    age_guessed, 
    age_guessed_r = case_when(
      age_guessed <= 40 ~ "Meno di 40 anni",
      age_guessed > 40 & age_guessed <=60 ~ "Fra i 40 e i 60 anni",
      age_guessed > 60 ~ "Più di 60 anni"
    )
  )

ospiti$age_guessed_r = factor(ospiti$age_guessed_r, 
                               levels =  c("Meno di 40 anni",
                                           "Fra i 40 e i 60 anni",
                                           "Più di 60 anni"))
table(ospiti$age_guessed_r)


ospiti = ospiti |>
  mutate(
    Professione1, 
    Professione_r = case_when(
      Professione1 == "giornalist" ~ "Giornalista",
      Professione1 == "politic" ~ "Politico/a",
      Professione1 == "altr" ~ "Altro",
      TRUE ~ "Scienziati/e sociali"
    )
  )


table(ospiti$Professione_r)

ospitate = ospitate |>
  select(Data, Trasmissione, id, Ospite_id)

data = merge(ospiti, ospitate, by.x = "Ospite_id", by.y = "Ospite_id", all = T)

data$Data <- dmy(data$Data)

# Function to create week labels
create_week_label <- function(date) {
  week_start <- floor_date(date, "week")
  week_end <- ceiling_date(date, "week") - days(1)
  paste(format(week_start, "%d/%m"), "-", format(week_end, "%d/%m/%Y"))
}

# Add intermediary week number and year columns for sorting
data <- data %>%
  mutate(
    week_number = week(Data),
    year = year(Data),
    week = create_week_label(Data)
  )

# Order the dataset by year and week_number
data <- data %>%
  arrange(year, week_number)

# Optionally, remove the intermediary columns
# View the updated dataset
head(data)

table(data$week_number)

table(data$week, data$week_number)

table(data$Trasmissione)
#1. un pannello avente i seguenti 8 grafici: Politico, Giornalista,
# Scienziati/e sociali e Altro in % in generale + la stessa cosa 
# nelle 7 trasmissioni. Nel farlo, elimina la parentesi doppie professioni
# su Y e metti le categorie di X in verticale o storte a 45° così dovrebbero 
# risultare leggibili. Cartabianca si scrive tutto attaccato (ho controllato
#                                                             online).



aus = data.frame(Professione= c(data$Professione1, data[!is.na(data$Professione2), ]$Professione2),
                 Trasmissione = c(data$Trasmissione, data[!is.na(data$Professione2), ]$Trasmissione),
                 Genere = c(data$Genere, data[!is.na(data$Professione2), ]$Genere))

aus$Professione <- factor(aus$Professione, levels = names(sort(table(aus$Professione), decreasing = TRUE)))

aus = aus |>
  mutate(
    Professione, 
    Professione_r = case_when(
      Professione == "giornalist" ~ "Giornalista",
      Professione == "politic" ~ "Politico/a",
      Professione == "altr" ~ "Altro",
      TRUE ~ "Scienziati/e sociali"
    )
  )

aus$Professione_r = factor(aus$Professione_r, levels = c("Giornalista", "Politico/a", "Scienziati/e sociali", "Altro"))

table(aus$Professione_r)


df_percent <- aus %>%
  group_by(Trasmissione, Professione_r) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total))

#calcola il totale

df_percent_aus <- aus %>%
  group_by(Professione_r) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total))

df_percent_aus$Trasmissione = "Totale"

df_percent = rbind(df_percent, df_percent_aus)


ggplot(df_percent, aes(x = Trasmissione, y = percentage, fill = Professione_r)) +
  geom_col() +
  labs(y = "Percentuale", x = "Trasmissione", fill = "Professione") +
  scale_y_continuous(breaks = seq(0,1,by=0.1), labels = label_percent())+
  scale_fill_grey(start = 0.8, end = 0.2)+
  theme(legend.title = element_text(size = 15), # Change size of legend title
        legend.text = element_text(size = 15),  # Change size of legend levels
        axis.text.x = element_text(angle = 30, hjust = 1, size=15),  # Change size of x-axis labels
        axis.text.y = element_text(size = 15),  # Change size of y-axis labels
        axis.title.x = element_text(size = 16), # Change size of x-axis label
        axis.title.y = element_text(size = 16))  # C)

ggsave("ospitate_per_professione_per_trasmissione_e_totale.eps", path=output_dir_graph, width = 10, height = 5)

export(df_percent, paste0(output_dir_table, "dati_grafico_professione_per_trasmissione_scienziati.xlsx"))

#2. un secondo pannello di 8 grafici (complessivo e le 7 trasmissioni) ma questa volta dedicato alla sola
#sottocategoria Scienziati/e sociali (=100), che faccia vedere quanto pesano le diverse discipline.


aus = data.frame(Professione= c(data$Professione1, data[!is.na(data$Professione2), ]$Professione2),
                 Trasmissione = c(data$Trasmissione, data[!is.na(data$Professione2), ]$Trasmissione),
                 Genere = c(data$Genere, data[!is.na(data$Professione2), ]$Genere))

aus$Professione <- factor(aus$Professione, levels = names(sort(table(aus$Professione), decreasing = TRUE)))

aus = aus |>
  mutate(
    Professione, 
    Professione_r = case_when(
      Professione == "giornalist" ~ "Giornalista",
      Professione == "politic" ~ "Politico/a",
      Professione == "altr" ~ "Altro",
      TRUE ~ "Scienziati/e sociali"
    )
  )

aus = aus |> 
  filter(Professione_r=="Scienziati/e sociali")

table(aus$Professione)


aus = aus |>
  mutate(
    Professione, 
    Professione_r1 = case_when(
      Professione == "storic" ~ "Storico/a",
      Professione == "filosof" ~ "Filosofo/a",
      Professione == "politolog" ~ "Sc. politico/a",
      Professione == "economist" ~ "Economista",
      Professione == "giurist" ~ "Giurista",
      Professione == "sociolog" ~ "Sociologo/a"
    )
  )

table(aus$Professione_r1)

sum(table(aus$Professione_r1))



df_percent <- aus %>%
  group_by(Trasmissione, Professione_r1) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total))

#calcola il totale

df_percent_aus <- aus %>%
  group_by(Professione_r1) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total))

df_percent_aus$Trasmissione = "Totale"

df_percent = rbind(df_percent, df_percent_aus)


ggplot(df_percent, aes(x = Trasmissione, y = percentage, fill = Professione_r1)) +
  geom_col() +
  labs(y = "Percentuale", x = "Trasmissione", fill = "Professione") +
  scale_y_continuous(breaks = seq(0,1,by=0.1), labels = label_percent())+
  scale_fill_grey(start = 0.99, end = 0.01)+
  theme(legend.title = element_text(size = 15), # Change size of legend title
        legend.text = element_text(size = 15),  # Change size of legend levels
        axis.text.x = element_text(angle = 30, hjust = 1, size=15),  # Change size of x-axis labels
        axis.text.y = element_text(size = 15),  # Change size of y-axis labels
        axis.title.x = element_text(size = 16), # Change size of x-axis label
        axis.title.y = element_text(size = 16))

ggsave("ospitate_per_professione_per_trasmissione_e_totale_Scienziati.eps", path=output_dir_graph, width = 10, height = 5)

export(df_percent, paste0(output_dir_table, "dati_grafico_professione_per_trasmissione.xlsx"))


# 3. un pannello con Politico, Giornalista, Scienziati/e sociali e Altro by gender in %
# in generale con le stacked bars per non aumentare il numero totale di bars + un grafico di lato con 
# % di donne complessiva by talk (tutti i talk nello stesso grafico con pallino o diamante in corrispondenza
#     ella loro % di donne totale e una riga solid in corrispondenza della media complessiva per vedere chi
#     è sopra e chi è sotto).


aus = data |>
  select(Ospite_id, Genere, Trasmissione, Professione1)


aus = aus |>
  mutate(
    Professione1, 
    Professione_r = case_when(
      Professione1 == "giornalist" ~ "Giornalista",
      Professione1 == "politic" ~ "Politico/a",
      Professione1 == "altr" ~ "Altro",
      TRUE ~ "Scienziati/e sociali"
    )
  )


df_percent <- aus %>%
  group_by(Professione_r, Genere) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total))

df_percent= df_percent|>
  filter(Genere=="F")


p1 = ggplot(df_percent, aes(x=Professione_r, y=percentage))+
  geom_col()+
  labs(y = "Percentuale di donne", x = "Professione", fill = "Genere") +
  scale_y_continuous(breaks = seq(0,1,by=0.1), labels = label_percent())+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size=15),  # Change size of x-axis labels
        axis.text.y = element_text(size = 15),  # Change size of y-axis labels
        axis.title.x = element_text(size = 16), # Change size of x-axis label
        axis.title.y = element_text(size = 16))

p1 


df_percent_1 <- aus %>%
  group_by(Trasmissione, Genere) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total))

df_percent_1 = df_percent_1 |> 
  filter(Genere == "F")

label_info = data.frame(label = "Media", x="Quarta Repubblica", y=0.32)

p2 = ggplot(data = df_percent_1, aes(x=Trasmissione, y=percentage))+
  geom_col()+
  labs(y = "Percentuale di donne", x = "Trasmissione") +
  scale_y_continuous(breaks = seq(0,1,by=0.1), labels = label_percent())+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_hline(yintercept= nrow(data[data$Genere == "F", ])/nrow(data))+
  geom_text(data = label_info, 
            aes(x = x, y = y, label = label),
            size = 5, hjust = "right", vjust = "bottom")+
  theme(# Change size of legend levels
        axis.text.x = element_text(angle = 30, hjust = 1, size=15),  # Change size of x-axis labels
        axis.text.y = element_text(size = 15),  # Change size of y-axis labels
        axis.title.x = element_text(size = 16), # Change size of x-axis label
        axis.title.y = element_text(size = 16))

p2

p1|p2


ggsave("genderbias.eps", path=output_dir_graph, width = 10, height = 5)

export(df_percent, paste0(output_dir_table, "genderbias1.xlsx"))
export(df_percent_1, paste0(output_dir_table, "genderbias2.xlsx"))


# 4. un pannello che faccia una cosa simile, ma con l'età. Dovrebbe risultare abbastanza leggibile 
# il primo grafico a barre: Politico, Giornalista, Scienziati/e sociali e Altro by classe d'
# età in % in generale con le stacked bars. Il secondo grafico, però, potrebbe risultare incasinato. 
# Forse si possono mettere tutti i talk nello stesso grafico con un pallino in corrispondenza della loro
# età media e una riga solid in corrispondenza dell'età media complessiva per vedere chi è sopra e chi è
# sotto.

data$age_guessed_r = factor(data$age_guessed_r, 
                              levels =  c("Meno di 40 anni",
                                          "Fra i 40 e i 60 anni",
                                          "Più di 60 anni"))
table(data$age_guessed_r)

labels = as.data.frame(round(prop.table(table(data$age_guessed_r)), digits = 4)*100)



aus <- data %>%
  group_by(Professione_r, age_guessed_r) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = round((count / total), digits=2))


export(aus, paste0(output_dir_table, "age_stacked.xlsx"))

# Create a pie chart with percentages

p3 = ggplot(aus, aes(x=Professione_r, y=percentage, fill=age_guessed_r))+
  geom_col()+
  labs(y = "Percentuale", x = "Professione", fill = "Fascia d'età")+
  scale_y_continuous(breaks = seq(0,1,by=0.1), labels = label_percent())+
  scale_fill_grey(start = 0.95, end = 0.05)+
  theme(legend.title = element_text(size = 15), # Change size of legend title
        legend.text = element_text(size = 15),  # Change size of legend levels
        axis.text.x = element_text(angle = 30, hjust = 1, size=15),  # Change size of x-axis labels
        axis.text.y = element_text(size = 15),  # Change size of y-axis labels
        axis.title.x = element_text(size = 16), # Change size of x-axis label
        axis.title.y = element_text(size = 16))
p3

ggplot(aus, aes(x="", y=percentage, fill=age_guessed_r)) +
  geom_col(width = 1) +
  coord_polar("y", start=0) +
  scale_fill_grey(start = 0.35, end = 0.85) +
  theme_void() +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5), col = 'black') +
  labs(title = "Ospitate nei talk show per fascia d'età")+
  theme(legend.title = element_text(size = 15), # Change size of legend title
        legend.text = element_text(size = 15),  # Change size of legend levels
        axis.text.x = element_text(angle = 30, hjust = 1, size=15),  # Change size of x-axis labels
        axis.text.y = element_text(size = 15),  # Change size of y-axis labels
        axis.title.x = element_text(size = 16), # Change size of x-axis label
        axis.title.y = element_text(size = 16))
  
aus <- data %>%
  group_by(Trasmissione) %>%
  summarise(mean_age = round(mean(age_guessed), digits = 2),
            n = n())

export(rbind(aus, c("Totale", mean(data$age_guessed))), paste0(output_dir_table, 
                                                               "mean_age_per_trasmissione.xlsx"))

label_info = data.frame(label = "Media", x="Quarta Repubblica", y=56.2)

p4 = ggplot(aus, aes(x=Trasmissione, y=mean_age))+
  geom_point(shape = 18, size = 4)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_hline(yintercept= mean(aus$mean_age))+
  geom_text(data = label_info, 
            aes(x = x, y = y, label = label),
            size = 5, hjust = "center", vjust = "bottom")+
  scale_y_continuous(breaks = seq(50,62, by=1), limits = c(50,61))+
  ylab("Età media (ospitate)")+
  theme(legend.title = element_text(size = 15), # Change size of legend title
        legend.text = element_text(size = 15),  # Change size of legend levels
        axis.text.x = element_text(angle = 30, hjust = 1, size=15),  # Change size of x-axis labels
        axis.text.y = element_text(size = 15),  # Change size of y-axis labels
        axis.title.x = element_text(size = 16), # Change size of x-axis label
        axis.title.y = element_text(size = 16))

p3|p4

ggsave("Età_per_classe_e_per_trasmissione_media.eps", path=output_dir_graph, width = 10, height = 5)

export(data, paste0(wd, "/data/", "data_tidy.xlsx"))

aus <- data %>%
  group_by(week_number, Genere) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total))

aus = aus |> filter(Genere == "F")

ggplot(aus, aes(x=week_number, y=percentage))+
  geom_col()+
  xlab("Settimana (Cecchettin uccisa settimana 45)")+
  ylab("Percentuale di donne")

ggsave("ipotesi_cecchettin.eps", path = output_dir_graph, width=10, height=5)

