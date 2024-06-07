######## From OSPITATE to data of ospiti to code!
library(rio)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(rvest)
library(openxlsx)

wd = getwd()

output_dir = paste0(wd, "/outputs/talk/")
input_dir = paste0(wd, "/data/talk/")

data = import(paste0(input_dir, "ospitate_april.xlsx"))

#check data to be sure
Data_giusta <- as.Date(data$Data, format = "%d/%m/%Y")

Data_giusta

sum(is.na(Data_giusta))
which(is.na(data$Data))

View(data[which(is.na(data$Data)), ])
View(data[which(is.na(Data_giusta)), ])
# Format the date column as "GG/MM/AAAA" and update the data frame
data$Data <- format(Data_giusta, format = "%d/%m/%Y")

View(data)

#create the id column
ids = 1:nrow(data)
data$id = ids

#create the ospite_code column

aus = data |>
  select(Ospite) |>
  group_by(Ospite) |>
  summarize(n_ospitate_totali=n())

length(unique(aus$Ospite))==length(unique(data$Ospite))

View(aus[order(aus$n_ospitate_totali, decreasing = T), ])

#aus = aus[order(aus$n_ospitate_totali, decreasing = T), ]
aus$Ospite_code = 1:length(aus$Ospite)

View(aus)

# ggplot(data, aes(x=Ospite))+
#   geom_bar(aes(fill=Ospite))

#data$Data = toString(data$Data)
data1 = merge(data, aus, by.x = "Ospite", by.y = "Ospite")
View(data1)
#everyrhing except names should be lowercase!

ospiti = import(paste0(input_dir, "ospiti.xlsx"))

View(ospiti)
View(aus)

ospiti1 = merge(ospiti, aus, by.x = "Ospite", by.y = "Ospite", all =T)

#Nota: ospiti1 ha tante righe quante aus? Se sì, tutto a posto

View(ospiti1)

sum(!is.na(ospiti1$`Pagina wikipedia`))
### mettiamo le pagine wikipedia agli ospiti nuovi

aux = str_split(ospiti1$Ospite, " ", n = Inf, simplify = FALSE)


#il pattern per le regex per estrarre l'anno dalla pagina wikipedia
pattern <- "\\(.*?\\b\\d{1,2}\\s+(?:gennaio|febbraio|marzo|aprile|maggio|giugno|luglio|agosto|settembre|ottobre|novembre|dicembre)\\s+(\\d{4})\\b.*?\\)"


for(i in 1:nrow(ospiti1))
{
  if(is.na(ospiti1$`Pagina wikipedia`[i]))
  {
    ospiti1$`Pagina wikipedia`[i] = paste0("https://it.wikipedia.org/wiki/", aux[[i]][1])  
    for(j in 2:length(aux[[i]]))
        ospiti1$`Pagina wikipedia`[i] = paste0(ospiti1$`Pagina wikipedia`[i], "_", aux[[i]][j])
  
  
  ### scrappo le prime righe della loro pagina wikipedia
   if(is.na(ospiti1$Prime_righe[i]))
    {
      wikipage <- tryCatch({
        read_html(ospiti1$`Pagina wikipedia`[i])
     }, error = function(e) {
      # Handle the error if needed
        cat("Error occurred:", conditionMessage(e), "\n")
      # You can return a default value or do something else
        return(NULL)
     })
    
    if (!is.null(wikipage)) {
      # Code to execute if no error occurred
      print("Ok")
      ospiti1$Prime_righe[i] = toString(html_text(html_nodes(wikipage, "p"))[1:3])
    } else {
      # Code to execute if an error occurred
      cat("Skipping due to error.\n")
      ospiti1$Prime_righe[i] = "Can't scrap wiki!"
    }
    
    
    rm(wikipage)
    
    #voglio estrarre l'anno di nascita da quelle info su wikipedia. Per farlo utilizzo
    #la variabile pattern di cui sopra e un po' di sintassi con le regex ospiti1 da chatgpt per trovare
    #la regex del tipo "("+"nome città"+"ospiti1di nascita"
      match <- regmatches(ospiti1$Prime_righe[i], regexec(pattern, ospiti1$Prime_righe[i]))
      ospiti1$anno_di_nascita[i] = match[[1]][2]
    
    }
  }
  
}

View(ospiti1)

ospiti1$`luogo di nascita` = NULL

ospiti2 = merge(ospiti1, aus, by="Ospite", all=T)

sum(ospiti2$n_ospitate_totali.y)

ospiti2$n_ospitate_totali.x = NULL
ospiti2$Number_of_Appearances = ospiti2$n_ospitate_totali.y
ospiti2$n_ospitate_totali.y = NULL
#ospiti2$Ospite_code.y = NULL

ospiti2$Ospite_code.x = NULL
ospiti2$is_female = NULL
ospiti2$is_male = NULL
ospiti2$name =NULL
ospiti2$id = NULL
ospiti2$Ospite_id = NULL

sum(!is.na(ospiti2$Genere)) #esattamente quanti erano prima, giusto? 

write.xlsx(ospiti2, paste0(output_dir,"to_code.xlsx"))

######################################

