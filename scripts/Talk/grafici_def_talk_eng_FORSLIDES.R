#GRAFICI UFFICIALI


library(rio)
library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)
library(lubridate)
library(openxlsx)

wd = "C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/negri_scaduto_SISP"

output_dir_graph = paste0(wd, "/outputs/talk/eng/grafici_def/slides")
output_dir_table = paste0(wd, "/outputs/talk/eng/tabelle_def/")
input_dir = paste0(wd, "/data/talk/")

ospitate = import("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/negri_scaduto_SISP/data/talk/Ospitate_correct.xlsx")
ospiti = import("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/negri_scaduto_SISP/data/talk/ospiti_april.xlsx")

sum(is.na(ospiti$anno_di_nascita_guessed))


data = import(paste0(input_dir, "data_to_analyze.xlsx"))

data$Data <- data$Date_fixed

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
# Scholar\n(social sciences/humanities) e Altro in % in generale + la stessa cosa 
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
      Professione == "giornalist" ~ "Journalist",
      Professione == "politic" ~ "Politician",
      Professione == "altr" ~ "Other",
      TRUE ~ "Scholar\n(social sciences/humanities)"
    )
  )

aus$Professione_r = factor(aus$Professione_r, levels = c("Journalist", "Politician", 
                                                         "Scholar\n(social sciences/humanities)", 
                                                         "Other"))

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

df_percent_aus$Trasmissione = "Total"

df_percent = rbind(df_percent, df_percent_aus)


ggplot(df_percent, aes(x = Trasmissione, y = percentage, fill = Professione_r)) +
  geom_col() +
  labs(y = "", x = "Talk show", fill = "Profession") +
  scale_y_continuous(breaks = seq(0,1,by=0.1), labels = label_percent())+
  #scale_fill_grey(start = 0.8, end = 0.2)+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 16),
        legend.position = "bottom")

ggsave("ospitate_per_professione_per_trasmissione_e_totale_eng_FORSLIDES.png", path=output_dir_graph, width = 10, height = 6)

write.xlsx(df_percent, paste0(output_dir_table, "dati_grafico_professione_per_trasmissione_scienziati_eng.xlxs"))

#2. un secondo pannello di 8 grafici (complessivo e le 7 trasmissioni) ma questa volta dedicato alla sola
#sottocategoria Scholar\n(social sciences/humanities) (=100), che faccia vedere quanto pesano le diverse discipline.


aus = data.frame(Professione= c(data$Professione1, data[!is.na(data$Professione2), ]$Professione2),
                 Trasmissione = c(data$Trasmissione, data[!is.na(data$Professione2), ]$Trasmissione),
                 Genere = c(data$Genere, data[!is.na(data$Professione2), ]$Genere))

aus$Professione <- factor(aus$Professione, levels = names(sort(table(aus$Professione), decreasing = TRUE)))

aus = aus |>
  mutate(
    Professione, 
    Professione_r = case_when(
      Professione == "giornalist" ~ "Journalist",
      Professione == "politic" ~ "Politician",
      Professione == "altr" ~ "Other",
      TRUE ~ "Scholar\n(social sciences/humanities)"
    )
  )

aus = aus |> 
  filter(Professione_r=="Scholar\n(social sciences/humanities)")

table(aus$Professione)


aus = aus |>
  mutate(
    Professione, 
    Professione_r1 = case_when(
      Professione == "storic" ~ "Historian",
      Professione == "filosof" ~ "Philosopher",
      Professione == "politolog" ~ "Politologist",
      Professione == "economist" ~ "Economist",
      Professione == "giurist" ~ "Jurist",
      Professione == "sociolog" ~ "Sociologist"
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

df_percent_aus$Trasmissione = "Total"

df_percent = rbind(df_percent, df_percent_aus)


ggplot(df_percent, aes(x = Trasmissione, y = percentage, fill = Professione_r1)) +
  geom_col() +
  labs(y = "Percentage", x = "Talk show", fill = "Profession") +
  scale_y_continuous(breaks = seq(0,1,by=0.1), labels = label_percent())+
  #scale_fill_grey(start = 0.99, end = 0.01)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size=16),
        legend.position = "bottom")

ggsave("ospitate_per_professione_per_trasmissione_e_totale_Scienziati_eng.png", path=output_dir_graph, width = 10, height = 6)

write.xlsx(df_percent, paste0(output_dir_table, "dati_grafico_professione_per_trasmissione_eng.xlxs"))


# 3. un pannello con Politico, Giornalista, Scholar\n(social sciences/humanities) e Altro by gender in %
# in generale con le stacked bars per non aumentare il numero totale di bars + un grafico di lato con 
# % of women complessiva by talk (tutti i talk nello stesso grafico con pallino o diamante in corrispondenza
#     ella loro % of women totale e una riga solid in corrispondenza della media complessiva per vedere chi
#     è sopra e chi è sotto).


aus = data |>
  select(Ospite, Genere, Trasmissione, Professione1)


aus = aus |>
  mutate(
    Professione1, 
    Professione_r = case_when(
      Professione1 == "giornalist" ~ "Journalist",
      Professione1 == "politic" ~ "Politician",
      Professione1 == "altr" ~ "Other",
      TRUE ~ "Scholar\n(social sciences/humanities)"
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
  geom_col(fill="red")+
  labs(y = "Percentage of women", x = "Profession", fill = "Gender") +
  scale_y_continuous(breaks = seq(0,1,by=0.05), labels = label_percent())+
  #scale_fill_grey(start = 0.3, end = 0.7)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size=15),
        )


p1 

ggsave("genderbias1_engFORSLIDES.png", path=output_dir_graph, width = 10, height = 6)



df_percent_1 <- aus %>%
  group_by(Trasmissione, Genere) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total))

df_percent_1 = df_percent_1 |> 
  filter(Genere == "F")

mean(df_percent_1$percentage)

label_info = data.frame(label = "Mean", x="Stasera Italia Weekend", y=mean(df_percent_1$percentage)+0.01)

p2 = ggplot(data = df_percent_1, aes(x=Trasmissione, y=percentage))+
  geom_col(fill="red")+
  labs(y = "Percentage of women", x = "Talk show") +
  scale_y_continuous(breaks = seq(0,1,by=0.05), labels = label_percent())+
  #scale_fill_grey(start = 0.3, end = 0.7)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size=15))+
  geom_hline(yintercept= mean(df_percent_1$percentage))+
  geom_text(data = label_info, 
            aes(x = x, y = y, label = label),
            size = 4, hjust = "right", vjust = "bottom")

p2

ggsave("genderbias2_eng.png", path=output_dir_graph, width = 10, height = 6)

write.xlsx(df_percent, paste0(output_dir_table, "genderbias1_eng.xlxs"))
write.xlsx(df_percent_1, paste0(output_dir_table, "genderbias2_eng.xlxs"))


# 4. un pannello che faccia una cosa simile, ma con l'età. Dovrebbe risultare abbastanza leggibile 
# il primo grafico a barre: Politico, Giornalista, Scholar\n(social sciences/humanities) e Altro by classe d'
# età in % in generale con le stacked bars. Il secondo grafico, però, potrebbe risultare incasinato. 
# Forse si possono mettere tutti i talk nello stesso grafico con un pallino in corrispondenza della loro
# età media e una riga solid in corrispondenza dell'età media complessiva per vedere chi è sopra e chi è
# sotto.

table(data$age_guessed_r)


data$age_guessed_r = factor(data$age_guessed_r, 
                            levels =  c("Less than 40 years old",
                                        "Between 40 and 60 years old",
                                        "Above 60 years old"))

table(data$age_guessed_r)

labels = as.data.frame(round(prop.table(table(data$age_guessed_r)), digits = 4)*100)



aus <- data %>%
  group_by(Professione_r, age_guessed_r) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = round((count / total), digits=2))


write.xlsx(aus, paste0(output_dir_table, "age_stacked_eng.xlxs"))

# Create a pie chart with percentages

p3 = ggplot(aus, aes(x=Professione_r, y=percentage, fill=age_guessed_r))+
  geom_col()+
  labs(y = "Percentage", x = "Profession", fill = "Age range")+
  scale_y_continuous(breaks = seq(0,1,by=0.1), labels = label_percent())+
  #scale_fill_grey(start = 0.95, end = 0.05)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size=15),
        legend.position = "bottom")

p3

ggsave("Età_per_professione_engFORSLIDES.png", path=output_dir_graph, width = 10, height = 6)


ggplot(aus, aes(x="", y=percentage, fill=age_guessed_r)) +
  geom_col(width = 1) +
  coord_polar("y", start=0) +
  #scale_fill_grey(start = 0.35, end = 0.85) +
  theme_void() +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5), col = 'black') +
  labs(title = "Talk show guests by age range")

aus <- data %>%
  group_by(Trasmissione) %>%
  summarise(mean_age = round(mean(age_guessed), digits = 2),
            n = n())

write.xlsx(rbind(aus, c("Total", mean(data$age_guessed))), paste0(output_dir_table, 
                                                                  "mean_age_per_trasmissione_eng.xlxs"))

label_info = data.frame(label = "Mean", x="Stasera Italia Weekend", y=mean(aus$mean_age)+.3)

p4 = ggplot(aus, aes(x=Trasmissione, y=mean_age))+
  geom_point(shape = 18, size = 8, col="red")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size=15))+
  geom_hline(yintercept= mean(aus$mean_age))+
  geom_text(data = label_info,
            aes(x = x, y = y, label = label),
            size = 4, hjust = "center", vjust = "bottom")+
  scale_y_continuous(breaks = seq(50,62, by=1), limits = c(50,62))+
  ylab("Età media (ospitate)")

p4

ggsave("Età_pertrasmissione_media_engSLIDES.png", path=output_dir_graph, width = 10, height = 6)

write.xlsx(data, paste0(wd, "/data/", "data_tidy_eng.xlxs"))

aus <- data %>%
  group_by(week_number, Genere) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total))

aus = aus |> filter(Genere == "F")

ggplot(aus, aes(x=week_number, y=percentage))+
  geom_col()+
  xlab("Week\n(Cecchettin murdered week 45)")+
  ylab("Percentage of women")

ggsave("ipotesi_cecchettin_eng.png", path = output_dir_graph, width=10, height=5)


#######
#TABLE 1 AND RELATED
######
table1 = import("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/negri_scaduto_SISP/outputs/talk/table1.xlsx")

table1=table1[1:6]
nrow(data |> group_by(Trasmissione, Date_fixed) |> summarise(n=n())) #numero di singole transmissioni

#number of episodes for every talk show
aus = data |> 
  group_by(Trasmissione, Date_fixed) |>
  summarise(nospiti=n()) |>
  ungroup() |>
  group_by(Trasmissione) |>
  summarise(`N of episodes`=n())

sum(aus$`N of episodes`) # TOTAL NUMBER OF UNIQUE EPISODES
table1 = merge(table1, aus, by.x = "Title (Italian)", by.y="Trasmissione")

#number of unique guests for every talk show
aus = data |> 
  group_by(Trasmissione, Ospite) |>
  summarise(nospiti=n()) |>
  ungroup() |>
  group_by(Trasmissione) |>
  summarise(`N of (unique) guests`=n())



table1 = merge(table1, aus, by.x = "Title (Italian)", by.y="Trasmissione")

#number of total guests for every talk show
aus = data |> 
  group_by(Trasmissione) |>
  summarise(`N of (total) guests`=n())

table1 = merge(table1, aus, by.x = "Title (Italian)", by.y="Trasmissione")

write.xlsx(table1, "C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/negri_scaduto_SISP/outputs/talk/table1.xlsx")

length(unique(data$Trasmissione)) #numero di show

sort(unique(data$Trasmissione)) #quali show)
