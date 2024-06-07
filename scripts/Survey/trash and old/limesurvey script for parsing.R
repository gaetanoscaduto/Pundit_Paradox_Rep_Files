setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/negri_scaduto_SISP/data/survey")

data <- read.csv("survey_881684_R_data_file.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")


# LimeSurvey Field type: F
data[, 1] <- as.numeric(data[, 1])
attributes(data)$variable.labels[1] <- "id"
names(data)[1] <- "id"
# LimeSurvey Field type: DATETIME23.2
data[, 2] <- as.character(data[, 2])
attributes(data)$variable.labels[2] <- "submitdate"
names(data)[2] <- "submitdate"
# LimeSurvey Field type: F
data[, 3] <- as.numeric(data[, 3])
attributes(data)$variable.labels[3] <- "lastpage"
names(data)[3] <- "lastpage"
# LimeSurvey Field type: A
data[, 4] <- as.character(data[, 4])
attributes(data)$variable.labels[4] <- "startlanguage"
names(data)[4] <- "startlanguage"
# LimeSurvey Field type: A
data[, 5] <- as.character(data[, 5])
attributes(data)$variable.labels[5] <- "seed"
names(data)[5] <- "seed"
# LimeSurvey Field type: A
data[, 6] <- as.character(data[, 6])
attributes(data)$variable.labels[6] <- "token"
names(data)[6] <- "token"
# LimeSurvey Field type: DATETIME23.2
data[, 7] <- as.character(data[, 7])
attributes(data)$variable.labels[7] <- "startdate"
names(data)[7] <- "startdate"
# LimeSurvey Field type: DATETIME23.2
data[, 8] <- as.character(data[, 8])
attributes(data)$variable.labels[8] <- "datestamp"
names(data)[8] <- "datestamp"
# LimeSurvey Field type: A
data[, 9] <- as.character(data[, 9])
attributes(data)$variable.labels[9] <- "ipaddr"
names(data)[9] <- "ipaddr"
# LimeSurvey Field type: A
data[, 10] <- as.character(data[, 10])
attributes(data)$variable.labels[10] <- "refurl"
names(data)[10] <- "refurl"
# LimeSurvey Field type: A
data[, 11] <- as.character(data[, 11])
attributes(data)$variable.labels[11] <- "Tu sei..."
data[, 11] <- factor(data[, 11], levels=c("AO01","AO02","AO03"),labels=c("Maschio", "Femmina", "Altro/Preferisco non specificare"))
names(data)[11] <- "gender"
# LimeSurvey Field type: A
data[, 12] <- as.character(data[, 12])
attributes(data)$variable.labels[12] <- "Indica la tua età in anni compiuti"
data[, 12] <- factor(data[, 12], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13","AO14","AO15","AO16","AO17","AO18","AO19","AO20","AO21","AO22","AO23","AO24","AO25","AO26","AO27","AO28","AO29","AO30","AO31","AO32","AO33","AO34","AO35","AO36","AO37","AO38","AO39","AO40","AO41","AO42","AO43","AO44","AO45","AO46","AO47","AO48","AO49","AO50","AO51","AO52","AO53","AO54","AO55","AO56","AO57","AO58","AO59","AO60","AO61","AO62","AO63","AO64","AO65","AO66","AO67","AO68","AO69","AO70","AO71","AO72","AO73","AO74","AO75","AO76","AO77","AO78","AO79","AO80","AO81","AO82","AO83","AO84"),labels=c("18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100", "Più di 100"))
names(data)[12] <- "age"
# LimeSurvey Field type: A
data[, 13] <- as.character(data[, 13])
attributes(data)$variable.labels[13] <- "In quale regione italiana abiti? "
data[, 13] <- factor(data[, 13], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13","AO14","AO15","AO16","AO17","AO18","AO19","AO20","AO21"),labels=c("Abruzzo", "Basilicata", "Calabria", "Campania", "Emilia-Romagna", "Friuli-Venezia Giulia", "Lazio", "Liguria", "Lombardia", "Marche", "Molise", "Piemonte", "Puglia", "Sardegna", "Sicilia", "Toscana", "Trentino-Alto Adige", "Umbria", "Val d\'Aosta", "Veneto", "Non abito in Italia"))
names(data)[13] <- "region"
# LimeSurvey Field type: A
data[, 14] <- as.character(data[, 14])
attributes(data)$variable.labels[14] <- "Qual è il tuo titolo di studio?"
data[, 14] <- factor(data[, 14], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09"),labels=c("Nessun titolo", "Licenza elementare", "Licenza media", "Diploma di qualifica professionale (2-3 anni)", "Diploma di maturità (4-5 anni)", "Laurea triennale di I livello", "Laurea specialistica/magistrale di II livello o laurea a ciclo unico (4-5 anni)", "Master/scuola di specializzazione post laurea", "Dottorato di ricerca"))
names(data)[14] <- "educ"
# LimeSurvey Field type: A
data[, 15] <- as.character(data[, 15])
attributes(data)$variable.labels[15] <- "[Dove ti collocheresti?] Nella nostra società c’è chi sta più in alto e chi sta più in basso. Tu dove ti posizioneresti, oggi, su una scala da 0 a 10, dove \"0\" indica il “livello più basso della nostra società\" e \"10\" indica il \"livello più alto della nostra società\"?"
data[, 15] <- factor(data[, 15], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12"),labels=c("0 - Al fondo della nostra società", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Al vertice della nostra società", "Non saprei"))
names(data)[15] <- "socposition_SQ001"
# LimeSurvey Field type: A
data[, 16] <- as.character(data[, 16])
attributes(data)$variable.labels[16] <- "Quanto diresti di essere interessato alla politica?"
data[, 16] <- factor(data[, 16], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Per nulla interessato", "2", "3", "4", "5 - Molto interessato"))
names(data)[16] <- "interest"
# LimeSurvey Field type: A
data[, 17] <- as.character(data[, 17])
attributes(data)$variable.labels[17] <- "In una giornata normale, all\'incirca quanto tempo passi a guardare, leggere o ascoltare notizie di politica e attualità?"
data[, 17] <- factor(data[, 17], levels=c("AO01","AO03","AO04","AO05","AO06","AO07"),labels=c("Non lo faccio mai", "Meno di dieci minuti", "Fra dieci minuti e mezz\'ora", "Fra mezz\'ora e un\'ora", "Fra una e due ore", "Più di due ore"))
names(data)[17] <- "exposure"
# LimeSurvey Field type: A
data[, 18] <- as.character(data[, 18])
attributes(data)$variable.labels[18] <- "[Quale numero riflette meglio la tua posizione?] In politica spesso si parla di “destra” e “sinistra”. Tu dove ti collocheresti? Per rispondere usa una scala da 0 a 10, dove “0” significa “estrema sinistra” e “10” significa “estrema destra”."
data[, 18] <- factor(data[, 18], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13"),labels=c("0 - Estrema sinistra", "1", "2", "3", "4", "5 - Centro", "6", "7", "8", "9", "10 - Estrema destra", "Da nessuna parte", "Non saprei"))
names(data)[18] <- "ideology_SQ001"
# LimeSurvey Field type: A
data[, 19] <- as.character(data[, 19])
attributes(data)$variable.labels[19] <- "Se dovessi dire però da quale parte politica ti senti più distante, diresti che..."
data[, 19] <- factor(data[, 19], levels=c("AO01","AO02"),labels=c("Mi sento più distante dalla destra", "Mi sento più distante dalla sinistra"))
names(data)[19] <- "ideologyfollowup"
# LimeSurvey Field type: A
data[, 20] <- as.character(data[, 20])
attributes(data)$variable.labels[20] <- "Questa domanda ci serve per capire se stai leggendo con attenzione le domande che ti poniamo. A questa domanda, devi rispondere selezionando l’opzione “Esteri”. Non devi dare la risposta corretta alla domanda. Devi solo selezionare “Esteri”. Quale ministero si occupa di ordine pubblico?"
data[, 20] <- factor(data[, 20], levels=c("AO01","AO02","AO03","AO04"),labels=c("Interno", "Esteri", "Giustizia", "Economia"))
names(data)[20] <- "check1"
# LimeSurvey Field type: A
data[, 21] <- as.character(data[, 21])
attributes(data)$variable.labels[21] <- "Se ci fossero le elezioni politiche domani, per quale partito voteresti?"
data[, 21] <- factor(data[, 21], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13"),labels=c("Fratelli d\'Italia", "Partito Democratico", "Movimento 5 Stelle", "Lega per Salvini Premier", "Forza Italia", "Azione", "Alleanza Verdi e Sinistra", "+Europa", "Italia Viva", "Un altro partito non elencato", "Voterei scheda bianca/nulla", "Mi asterrei", "Non saprei/Non ho ancora deciso"))
names(data)[21] <- "vote"
# LimeSurvey Field type: A
data[, 22] <- as.character(data[, 22])
attributes(data)$variable.labels[22] <- "[Il governo] In generale, quanto ti fidi personalmente delle seguenti istituzioni? Per rispondere usa una scala da 1 a 5, dove “1” significa “nessuna fiducia” e “5” significa “piena fiducia”"
data[, 22] <- factor(data[, 22], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Nessuna fiducia", "2", "3", "4", "5 - Piena fiducia"))
names(data)[22] <- "trust_gove"
# LimeSurvey Field type: A
data[, 23] <- as.character(data[, 23])
attributes(data)$variable.labels[23] <- "[I partiti politici] In generale, quanto ti fidi personalmente delle seguenti istituzioni? Per rispondere usa una scala da 1 a 5, dove “1” significa “nessuna fiducia” e “5” significa “piena fiducia”"
data[, 23] <- factor(data[, 23], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Nessuna fiducia", "2", "3", "4", "5 - Piena fiducia"))
names(data)[23] <- "trust_part"
# LimeSurvey Field type: A
data[, 24] <- as.character(data[, 24])
attributes(data)$variable.labels[24] <- "[I media] In generale, quanto ti fidi personalmente delle seguenti istituzioni? Per rispondere usa una scala da 1 a 5, dove “1” significa “nessuna fiducia” e “5” significa “piena fiducia”"
data[, 24] <- factor(data[, 24], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Nessuna fiducia", "2", "3", "4", "5 - Piena fiducia"))
names(data)[24] <- "trust_medi"
# LimeSurvey Field type: A
data[, 25] <- as.character(data[, 25])
attributes(data)$variable.labels[25] <- "[Gli esperti] In generale, quanto ti fidi personalmente delle seguenti istituzioni? Per rispondere usa una scala da 1 a 5, dove “1” significa “nessuna fiducia” e “5” significa “piena fiducia”"
data[, 25] <- factor(data[, 25], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Nessuna fiducia", "2", "3", "4", "5 - Piena fiducia"))
names(data)[25] <- "trust_expe"
# LimeSurvey Field type: A
data[, 26] <- as.character(data[, 26])
attributes(data)$variable.labels[26] <- "{if(is_empty(gender11), rand(1,2), gender11)}"
names(data)[26] <- "gender11"
# LimeSurvey Field type: A
data[, 27] <- as.character(data[, 27])
attributes(data)$variable.labels[27] <- "{if(is_empty(gender21), rand(1,2), gender21)}"
names(data)[27] <- "gender21"
# LimeSurvey Field type: A
data[, 28] <- as.character(data[, 28])
attributes(data)$variable.labels[28] <- "{if(is_empty(gender12), rand(1,2), gender12)}"
names(data)[28] <- "gender12"
# LimeSurvey Field type: A
data[, 29] <- as.character(data[, 29])
attributes(data)$variable.labels[29] <- "{if(is_empty(gender22), rand(1,2), gender22)}"
names(data)[29] <- "gender22"
# LimeSurvey Field type: A
data[, 30] <- as.character(data[, 30])
attributes(data)$variable.labels[30] <- "{if(is_empty(gender13), rand(1,2), gender13)}"
names(data)[30] <- "gender13"
# LimeSurvey Field type: A
data[, 31] <- as.character(data[, 31])
attributes(data)$variable.labels[31] <- "{if(is_empty(gender23), rand(1,2), gender23)}"
names(data)[31] <- "gender23"
# LimeSurvey Field type: A
data[, 32] <- as.character(data[, 32])
attributes(data)$variable.labels[32] <- "{if(is_empty(gender14), rand(1,2), gender14)}"
names(data)[32] <- "gender14"
# LimeSurvey Field type: A
data[, 33] <- as.character(data[, 33])
attributes(data)$variable.labels[33] <- "{if(is_empty(gender24), rand(1,2), gender24)}"
names(data)[33] <- "gender24"
# LimeSurvey Field type: A
data[, 34] <- as.character(data[, 34])
attributes(data)$variable.labels[34] <- "{if(is_empty(gender15), rand(1,2), gender15)}"
names(data)[34] <- "gender15"
# LimeSurvey Field type: A
data[, 35] <- as.character(data[, 35])
attributes(data)$variable.labels[35] <- "{if(is_empty(gender25), rand(1,2), gender25)}"
names(data)[35] <- "gender25"
# LimeSurvey Field type: A
data[, 36] <- as.character(data[, 36])
attributes(data)$variable.labels[36] <- "{if(is_empty(gender16), rand(1,2), gender16)}"
names(data)[36] <- "gender16"
# LimeSurvey Field type: A
data[, 37] <- as.character(data[, 37])
attributes(data)$variable.labels[37] <- "{if(is_empty(gender26), rand(1,2), gender26)}"
names(data)[37] <- "gender26"
# LimeSurvey Field type: A
data[, 38] <- as.character(data[, 38])
attributes(data)$variable.labels[38] <- "{if(is_empty(gender17), rand(1,2), gender17)}"
names(data)[38] <- "gender17"
# LimeSurvey Field type: A
data[, 39] <- as.character(data[, 39])
attributes(data)$variable.labels[39] <- "{if(is_empty(gender27), rand(1,2), gender27)}"
names(data)[39] <- "gender27"
# LimeSurvey Field type: A
data[, 40] <- as.character(data[, 40])
attributes(data)$variable.labels[40] <- "{if(is_empty(gender18), rand(1,2), gender18)}"
names(data)[40] <- "gender18"
# LimeSurvey Field type: A
data[, 41] <- as.character(data[, 41])
attributes(data)$variable.labels[41] <- "{if(is_empty(gender28), rand(1,2), gender28)}"
names(data)[41] <- "gender28"
# LimeSurvey Field type: A
data[, 42] <- as.character(data[, 42])
attributes(data)$variable.labels[42] <- "{if(is_empty(age11), rand(1,3), age11)}"
names(data)[42] <- "age11"
# LimeSurvey Field type: A
data[, 43] <- as.character(data[, 43])
attributes(data)$variable.labels[43] <- "{if(is_empty(age21), rand(1,3), age21)}"
names(data)[43] <- "age21"
# LimeSurvey Field type: A
data[, 44] <- as.character(data[, 44])
attributes(data)$variable.labels[44] <- "{if(is_empty(age12), rand(1,3), age12)}"
names(data)[44] <- "age12"
# LimeSurvey Field type: A
data[, 45] <- as.character(data[, 45])
attributes(data)$variable.labels[45] <- "{if(is_empty(age22), rand(1,3), age22)}"
names(data)[45] <- "age22"
# LimeSurvey Field type: A
data[, 46] <- as.character(data[, 46])
attributes(data)$variable.labels[46] <- "{if(is_empty(age13), rand(1,3), age13)}"
names(data)[46] <- "age13"
# LimeSurvey Field type: A
data[, 47] <- as.character(data[, 47])
attributes(data)$variable.labels[47] <- "{if(is_empty(age23), rand(1,3), age23)}"
names(data)[47] <- "age23"
# LimeSurvey Field type: A
data[, 48] <- as.character(data[, 48])
attributes(data)$variable.labels[48] <- "{if(is_empty(age14), rand(1,3), age14)}"
names(data)[48] <- "age14"
# LimeSurvey Field type: A
data[, 49] <- as.character(data[, 49])
attributes(data)$variable.labels[49] <- "{if(is_empty(age24), rand(1,3), age24)}"
names(data)[49] <- "age24"
# LimeSurvey Field type: A
data[, 50] <- as.character(data[, 50])
attributes(data)$variable.labels[50] <- "{if(is_empty(age15), rand(1,3), age15)}"
names(data)[50] <- "age15"
# LimeSurvey Field type: A
data[, 51] <- as.character(data[, 51])
attributes(data)$variable.labels[51] <- "{if(is_empty(age25), rand(1,3), age25)}"
names(data)[51] <- "age25"
# LimeSurvey Field type: A
data[, 52] <- as.character(data[, 52])
attributes(data)$variable.labels[52] <- "{if(is_empty(age16), rand(1,3), age16)}"
names(data)[52] <- "age16"
# LimeSurvey Field type: A
data[, 53] <- as.character(data[, 53])
attributes(data)$variable.labels[53] <- "{if(is_empty(age26), rand(1,3), age26)}"
names(data)[53] <- "age26"
# LimeSurvey Field type: A
data[, 54] <- as.character(data[, 54])
attributes(data)$variable.labels[54] <- "{if(is_empty(age17), rand(1,3), age17)}"
names(data)[54] <- "age17"
# LimeSurvey Field type: A
data[, 55] <- as.character(data[, 55])
attributes(data)$variable.labels[55] <- "{if(is_empty(age27), rand(1,3), age27)}"
names(data)[55] <- "age27"
# LimeSurvey Field type: A
data[, 56] <- as.character(data[, 56])
attributes(data)$variable.labels[56] <- "{if(is_empty(age18), rand(1,3), age18)}"
names(data)[56] <- "age18"
# LimeSurvey Field type: A
data[, 57] <- as.character(data[, 57])
attributes(data)$variable.labels[57] <- "{if(is_empty(age28), rand(1,3), age28)}"
names(data)[57] <- "age28"
# LimeSurvey Field type: A
data[, 58] <- as.character(data[, 58])
attributes(data)$variable.labels[58] <- "{if(is_empty(prov11), rand(1,3), prov11)}"
names(data)[58] <- "prov11"
# LimeSurvey Field type: A
data[, 59] <- as.character(data[, 59])
attributes(data)$variable.labels[59] <- "{if(is_empty(prov21), rand(1,3), prov21)}"
names(data)[59] <- "prov21"
# LimeSurvey Field type: A
data[, 60] <- as.character(data[, 60])
attributes(data)$variable.labels[60] <- "{if(is_empty(prov12), rand(1,3), prov12)}"
names(data)[60] <- "prov12"
# LimeSurvey Field type: A
data[, 61] <- as.character(data[, 61])
attributes(data)$variable.labels[61] <- "{if(is_empty(prov22), rand(1,3), prov22)}"
names(data)[61] <- "prov22"
# LimeSurvey Field type: A
data[, 62] <- as.character(data[, 62])
attributes(data)$variable.labels[62] <- "{if(is_empty(prov13), rand(1,3), prov13)}"
names(data)[62] <- "prov13"
# LimeSurvey Field type: A
data[, 63] <- as.character(data[, 63])
attributes(data)$variable.labels[63] <- "{if(is_empty(prov23), rand(1,3), prov23)}"
names(data)[63] <- "prov23"
# LimeSurvey Field type: A
data[, 64] <- as.character(data[, 64])
attributes(data)$variable.labels[64] <- "{if(is_empty(prov14), rand(1,3), prov14)}"
names(data)[64] <- "prov14"
# LimeSurvey Field type: A
data[, 65] <- as.character(data[, 65])
attributes(data)$variable.labels[65] <- "{if(is_empty(prov24), rand(1,3), prov24)}"
names(data)[65] <- "prov24"
# LimeSurvey Field type: A
data[, 66] <- as.character(data[, 66])
attributes(data)$variable.labels[66] <- "{if(is_empty(prov15), rand(1,3), prov15)}"
names(data)[66] <- "prov15"
# LimeSurvey Field type: A
data[, 67] <- as.character(data[, 67])
attributes(data)$variable.labels[67] <- "{if(is_empty(prov25), rand(1,3), prov25)}"
names(data)[67] <- "prov25"
# LimeSurvey Field type: A
data[, 68] <- as.character(data[, 68])
attributes(data)$variable.labels[68] <- "{if(is_empty(prov16), rand(1,3), prov16)}"
names(data)[68] <- "prov16"
# LimeSurvey Field type: A
data[, 69] <- as.character(data[, 69])
attributes(data)$variable.labels[69] <- "{if(is_empty(prov26), rand(1,3), prov26)}"
names(data)[69] <- "prov26"
# LimeSurvey Field type: A
data[, 70] <- as.character(data[, 70])
attributes(data)$variable.labels[70] <- "{if(is_empty(prov17), rand(1,3), prov17)}"
names(data)[70] <- "prov17"
# LimeSurvey Field type: A
data[, 71] <- as.character(data[, 71])
attributes(data)$variable.labels[71] <- "{if(is_empty(prov27), rand(1,3), prov27)}"
names(data)[71] <- "prov27"
# LimeSurvey Field type: A
data[, 72] <- as.character(data[, 72])
attributes(data)$variable.labels[72] <- "{if(is_empty(prov18), rand(1,3), prov18)}"
names(data)[72] <- "prov18"
# LimeSurvey Field type: A
data[, 73] <- as.character(data[, 73])
attributes(data)$variable.labels[73] <- "{if(is_empty(prov28), rand(1,3), prov28)}"
names(data)[73] <- "prov28"
# LimeSurvey Field type: A
data[, 74] <- as.character(data[, 74])
attributes(data)$variable.labels[74] <- "{if(is_empty(ideo11), rand(1,4), ideo11)}"
names(data)[74] <- "ideo11"
# LimeSurvey Field type: A
data[, 75] <- as.character(data[, 75])
attributes(data)$variable.labels[75] <- "{if(is_empty(ideo21), rand(1,4), ideo21)}"
names(data)[75] <- "ideo21"
# LimeSurvey Field type: A
data[, 76] <- as.character(data[, 76])
attributes(data)$variable.labels[76] <- "{if(is_empty(ideo12), rand(1,4), ideo12)}"
names(data)[76] <- "ideo12"
# LimeSurvey Field type: A
data[, 77] <- as.character(data[, 77])
attributes(data)$variable.labels[77] <- "{if(is_empty(ideo22), rand(1,4), ideo22)}"
names(data)[77] <- "ideo22"
# LimeSurvey Field type: A
data[, 78] <- as.character(data[, 78])
attributes(data)$variable.labels[78] <- "{if(is_empty(ideo13), rand(1,4), ideo13)}"
names(data)[78] <- "ideo13"
# LimeSurvey Field type: A
data[, 79] <- as.character(data[, 79])
attributes(data)$variable.labels[79] <- "{if(is_empty(ideo23), rand(1,4), ideo23)}"
names(data)[79] <- "ideo23"
# LimeSurvey Field type: A
data[, 80] <- as.character(data[, 80])
attributes(data)$variable.labels[80] <- "{if(is_empty(ideo14), rand(1,4), ideo14)}"
names(data)[80] <- "ideo14"
# LimeSurvey Field type: A
data[, 81] <- as.character(data[, 81])
attributes(data)$variable.labels[81] <- "{if(is_empty(ideo24), rand(1,4), ideo24)}"
names(data)[81] <- "ideo24"
# LimeSurvey Field type: A
data[, 82] <- as.character(data[, 82])
attributes(data)$variable.labels[82] <- "{if(is_empty(ideo15), rand(1,4), ideo15)}"
names(data)[82] <- "ideo15"
# LimeSurvey Field type: A
data[, 83] <- as.character(data[, 83])
attributes(data)$variable.labels[83] <- "{if(is_empty(ideo25), rand(1,4), ideo25)}"
names(data)[83] <- "ideo25"
# LimeSurvey Field type: A
data[, 84] <- as.character(data[, 84])
attributes(data)$variable.labels[84] <- "{if(is_empty(ideo16), rand(1,4), ideo16)}"
names(data)[84] <- "ideo16"
# LimeSurvey Field type: A
data[, 85] <- as.character(data[, 85])
attributes(data)$variable.labels[85] <- "{if(is_empty(ideo26), rand(1,4), ideo26)}"
names(data)[85] <- "ideo26"
# LimeSurvey Field type: A
data[, 86] <- as.character(data[, 86])
attributes(data)$variable.labels[86] <- "{if(is_empty(ideo17), rand(1,4), ideo17)}"
names(data)[86] <- "ideo17"
# LimeSurvey Field type: A
data[, 87] <- as.character(data[, 87])
attributes(data)$variable.labels[87] <- "{if(is_empty(ideo27), rand(1,4), ideo27)}"
names(data)[87] <- "ideo27"
# LimeSurvey Field type: A
data[, 88] <- as.character(data[, 88])
attributes(data)$variable.labels[88] <- "{if(is_empty(ideo18), rand(1,4), ideo18)}"
names(data)[88] <- "ideo18"
# LimeSurvey Field type: A
data[, 89] <- as.character(data[, 89])
attributes(data)$variable.labels[89] <- "{if(is_empty(ideo28), rand(1,4), ideo28)}"
names(data)[89] <- "ideo28"
# LimeSurvey Field type: A
data[, 90] <- as.character(data[, 90])
attributes(data)$variable.labels[90] <- "{if(is_empty(job11), rand(1,5), job11)}"
names(data)[90] <- "job11"
# LimeSurvey Field type: A
data[, 91] <- as.character(data[, 91])
attributes(data)$variable.labels[91] <- "{if( (gender11==gender21) and (age11==age21) and (prov11==prov21) and (ideo11==ideo21), if(job11==5, rand(1,4), sum(job11,1)), rand(1,5))}"
names(data)[91] <- "job21"
# LimeSurvey Field type: A
data[, 92] <- as.character(data[, 92])
attributes(data)$variable.labels[92] <- "{if(is_empty(job12), rand(1,5), job12)}"
names(data)[92] <- "job12"
# LimeSurvey Field type: A
data[, 93] <- as.character(data[, 93])
attributes(data)$variable.labels[93] <- "{if( (gender12==gender22) and (age12==age22) and (prov12==prov22) and (ideo12==ideo22), if(job12==5, rand(1,4), sum(job12,1)), rand(1,5))}"
names(data)[93] <- "job22"
# LimeSurvey Field type: A
data[, 94] <- as.character(data[, 94])
attributes(data)$variable.labels[94] <- "{if(is_empty(job13), rand(1,5), job13)}"
names(data)[94] <- "job13"
# LimeSurvey Field type: A
data[, 95] <- as.character(data[, 95])
attributes(data)$variable.labels[95] <- "{if( (gender13==gender23) and (age13==age23) and (prov13==prov23) and (ideo13==ideo23), if(job13==5, rand(1,4), sum(job13,1)), rand(1,5))}"
names(data)[95] <- "job23"
# LimeSurvey Field type: A
data[, 96] <- as.character(data[, 96])
attributes(data)$variable.labels[96] <- "{if(is_empty(job14), rand(1,5), job14)}"
names(data)[96] <- "job14"
# LimeSurvey Field type: A
data[, 97] <- as.character(data[, 97])
attributes(data)$variable.labels[97] <- "{if( (gender14==gender24) and (age14==age24) and (prov14==prov24) and (ideo14==ideo24), if(job14==5, rand(1,4), sum(job14,1)), rand(1,5))}"
names(data)[97] <- "job24"
# LimeSurvey Field type: A
data[, 98] <- as.character(data[, 98])
attributes(data)$variable.labels[98] <- "{if(is_empty(job15), rand(1,5), job15)}"
names(data)[98] <- "job15"
# LimeSurvey Field type: A
data[, 99] <- as.character(data[, 99])
attributes(data)$variable.labels[99] <- "{if( (gender15==gender25) and (age15==age25) and (prov15==prov25) and (ideo15==ideo25), if(job15==5, rand(1,4), sum(job15,1)), rand(1,5))}"
names(data)[99] <- "job25"
# LimeSurvey Field type: A
data[, 100] <- as.character(data[, 100])
attributes(data)$variable.labels[100] <- "{if(is_empty(job16), rand(1,5), job16)}"
names(data)[100] <- "job16"
# LimeSurvey Field type: A
data[, 101] <- as.character(data[, 101])
attributes(data)$variable.labels[101] <- "{if( (gender16==gender26) and (age16==age26) and (prov16==prov26) and (ideo16==ideo26), if(job16==5, rand(1,4), sum(job16,1)), rand(1,5))}"
names(data)[101] <- "job26"
# LimeSurvey Field type: A
data[, 102] <- as.character(data[, 102])
attributes(data)$variable.labels[102] <- "{if(is_empty(job17), rand(1,5), job17)}"
names(data)[102] <- "job17"
# LimeSurvey Field type: A
data[, 103] <- as.character(data[, 103])
attributes(data)$variable.labels[103] <- "{if( (gender17==gender27) and (age17==age27) and (prov17==prov27) and (ideo17==ideo27), if(job17==5, rand(1,4), sum(job17,1)), rand(1,5))}"
names(data)[103] <- "job27"
# LimeSurvey Field type: A
data[, 104] <- as.character(data[, 104])
attributes(data)$variable.labels[104] <- "{if(is_empty(job18), rand(1,5), job18)}"
names(data)[104] <- "job18"
# LimeSurvey Field type: A
data[, 105] <- as.character(data[, 105])
attributes(data)$variable.labels[105] <- "{if( (gender18==gender28) and (age18==age28) and (prov18==prov28) and (ideo18==ideo28), if(job18==5, rand(1,4), sum(job18,1)), rand(1,5))}"
names(data)[105] <- "job28"
# LimeSurvey Field type: A
data[, 106] <- as.character(data[, 106])
attributes(data)$variable.labels[106] <- "Basandoti sulle caratteristiche che ti vengono mostrate, quale fra questi due ospiti ti ispira più fiducia? .UserTable {         border-collapse: collapse; /* Collapse borders */         width: 100%; /* Make the table span the full width of its container */     }     .UserTable td, .UserTable th {         border: 1px solid black; /* Add black border */         padding: 10px; /* Keep your padding */         text-align: center; /* Center-align text */         width: 33.33%; /* Set each column to a third of the table width */     }    	 		 			   			 			Ospite 1 			  			 			Ospite 2 			  		 		 			Professione  			{if(job11== \"1\", \"Economista\", if(job11==\"2\",\"Filosofo/a\", if(job11==\"3\", \"Giornalista\", if(job11==\"4\", \"Giurista\", \"Politologo/a\"))))}  			{if(job21== \"1\", \"Economista\", if(job21==\"2\",\"Filosofo/a\", if(job21==\"3\", \"Giornalista\", if(job21==\"4\", \"Giurista\", \"Politologo/a\"))))}  		 		 			Sesso  			{if(gender11== \"1\", \"Femmina\", \"Maschio\")}  			{if(gender21== \"1\", \"Femmina\", \"Maschio\")}  		 		 			Età  			{if(age11== \"1\", \"37\", if(age11==\"2\",\"52\",\"67\"))}  			{if(age21== \"1\", \"37\", if(age21==\"2\",\"52\",\"67\"))}  		 		 			Area italiana di provenienza  			{if(prov11== \"1\", \"Centro\", if(prov11==\"2\",\"Nord\",\"Sud\"))}  			{if(prov21== \"1\", \"Centro\", if(prov21==\"2\",\"Nord\",\"Sud\"))}  		 		 			Preferenze politiche  			{if(ideo11== \"1\", \"Non note\", if(ideo11==\"2\",\"Tendenzialmente di centro\", if(ideo11==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}  			{if(ideo21== \"1\", \"Non note\", if(ideo21==\"2\",\"Tendenzialmente di centro\", if(ideo21==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}"
data[, 106] <- factor(data[, 106], levels=c("AO01","AO02"),labels=c("Ospite 1", "Ospite 2"))
names(data)[106] <- "task1"
# LimeSurvey Field type: A
data[, 107] <- as.character(data[, 107])
attributes(data)$variable.labels[107] <- "Basandoti sulle caratteristiche che ti vengono mostrate, quale fra questi due ospiti ti ispira più fiducia? .UserTable {         border-collapse: collapse; /* Collapse borders */         width: 100%; /* Make the table span the full width of its container */     }     .UserTable td, .UserTable th {         border: 1px solid black; /* Add black border */         padding: 10px; /* Keep your padding */         text-align: center; /* Center-align text */         width: 33.33%; /* Set each column to a third of the table width */     }    	 		 			   			 			Ospite 1 			  			 			Ospite 2 			  		 		 			Professione  			{if(job12== \"1\", \"Economista\", if(job12==\"2\",\"Filosofo/a\", if(job12==\"3\", \"Giornalista\", if(job12==\"4\", \"Giurista\", \"Politologo/a\"))))}  			{if(job22== \"1\", \"Economista\", if(job22==\"2\",\"Filosofo/a\", if(job22==\"3\", \"Giornalista\", if(job22==\"4\", \"Giurista\", \"Politologo/a\"))))}  		 		 			Sesso  			{if(gender12== \"1\", \"Femmina\", \"Maschio\")}  			{if(gender22== \"1\", \"Femmina\", \"Maschio\")}  		 		 			Età  			{if(age12== \"1\", \"37\", if(age12==\"2\",\"52\",\"67\"))}  			{if(age22== \"1\", \"37\", if(age22==\"2\",\"52\",\"67\"))}  		 		 			Area italiana di provenienza  			{if(prov12== \"1\", \"Centro\", if(prov12==\"2\",\"Nord\",\"Sud\"))}  			{if(prov22== \"1\", \"Centro\", if(prov22==\"2\",\"Nord\",\"Sud\"))}  		 		 			Preferenze politiche  			{if(ideo12== \"1\", \"Non note\", if(ideo12==\"2\",\"Tendenzialmente di centro\", if(ideo12==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}  			{if(ideo22== \"1\", \"Non note\", if(ideo22==\"2\",\"Tendenzialmente di centro\", if(ideo22==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}"
data[, 107] <- factor(data[, 107], levels=c("AO01","AO02"),labels=c("Ospite 1", "Ospite 2"))
names(data)[107] <- "task2"
# LimeSurvey Field type: A
data[, 108] <- as.character(data[, 108])
attributes(data)$variable.labels[108] <- "Basandoti sulle caratteristiche che ti vengono mostrate, quale fra questi due ospiti ti ispira più fiducia? .UserTable {         border-collapse: collapse; /* Collapse borders */         width: 100%; /* Make the table span the full width of its container */     }     .UserTable td, .UserTable th {         border: 1px solid black; /* Add black border */         padding: 10px; /* Keep your padding */         text-align: center; /* Center-align text */         width: 33.33%; /* Set each column to a third of the table width */     }    	 		 			   			 			Ospite 1 			  			 			Ospite 2 			  		 		 			Professione  			{if(job13== \"1\", \"Economista\", if(job13==\"2\",\"Filosofo/a\", if(job13==\"3\", \"Giornalista\", if(job13==\"4\", \"Giurista\", \"Politologo/a\"))))}  			{if(job23== \"1\", \"Economista\", if(job23==\"2\",\"Filosofo/a\", if(job23==\"3\", \"Giornalista\", if(job23==\"4\", \"Giurista\", \"Politologo/a\"))))}  		 		 			Sesso  			{if(gender13== \"1\", \"Femmina\", \"Maschio\")}  			{if(gender23== \"1\", \"Femmina\", \"Maschio\")}  		 		 			Età  			{if(age13== \"1\", \"37\", if(age13==\"2\",\"52\",\"67\"))}  			{if(age23== \"1\", \"37\", if(age23==\"2\",\"52\",\"67\"))}  		 		 			Area italiana di provenienza  			{if(prov13== \"1\", \"Centro\", if(prov13==\"2\",\"Nord\",\"Sud\"))}  			{if(prov23== \"1\", \"Centro\", if(prov23==\"2\",\"Nord\",\"Sud\"))}  		 		 			Preferenze politiche  			{if(ideo13== \"1\", \"Non note\", if(ideo13==\"2\",\"Tendenzialmente di centro\", if(ideo13==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}  			{if(ideo23== \"1\", \"Non note\", if(ideo23==\"2\",\"Tendenzialmente di centro\", if(ideo23==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}"
data[, 108] <- factor(data[, 108], levels=c("AO01","AO02"),labels=c("Ospite 1", "Ospite 2"))
names(data)[108] <- "task3"
# LimeSurvey Field type: A
data[, 109] <- as.character(data[, 109])
attributes(data)$variable.labels[109] <- "Basandoti sulle caratteristiche che ti vengono mostrate, quale fra questi due ospiti ti ispira più fiducia? .UserTable {         border-collapse: collapse; /* Collapse borders */         width: 100%; /* Make the table span the full width of its container */     }     .UserTable td, .UserTable th {         border: 1px solid black; /* Add black border */         padding: 10px; /* Keep your padding */         text-align: center; /* Center-align text */         width: 33.33%; /* Set each column to a third of the table width */     }    	 		 			   			 			Ospite 1 			  			 			Ospite 2 			  		 		 			Professione  			{if(job14== \"1\", \"Economista\", if(job14==\"2\",\"Filosofo/a\", if(job14==\"3\", \"Giornalista\", if(job14==\"4\", \"Giurista\", \"Politologo/a\"))))}  			{if(job24== \"1\", \"Economista\", if(job24==\"2\",\"Filosofo/a\", if(job24==\"3\", \"Giornalista\", if(job24==\"4\", \"Giurista\", \"Politologo/a\"))))}  		 		 			Sesso  			{if(gender14== \"1\", \"Femmina\", \"Maschio\")}  			{if(gender24== \"1\", \"Femmina\", \"Maschio\")}  		 		 			Età  			{if(age14== \"1\", \"37\", if(age14==\"2\",\"52\",\"67\"))}  			{if(age24== \"1\", \"37\", if(age24==\"2\",\"52\",\"67\"))}  		 		 			Area italiana di provenienza  			{if(prov14== \"1\", \"Centro\", if(prov14==\"2\",\"Nord\",\"Sud\"))}  			{if(prov24== \"1\", \"Centro\", if(prov24==\"2\",\"Nord\",\"Sud\"))}  		 		 			Preferenze politiche  			{if(ideo14== \"1\", \"Non note\", if(ideo14==\"2\",\"Tendenzialmente di centro\", if(ideo14==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}  			{if(ideo24== \"1\", \"Non note\", if(ideo24==\"2\",\"Tendenzialmente di centro\", if(ideo24==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}"
data[, 109] <- factor(data[, 109], levels=c("AO01","AO02"),labels=c("Ospite 1", "Ospite 2"))
names(data)[109] <- "task4"
# LimeSurvey Field type: A
data[, 110] <- as.character(data[, 110])
attributes(data)$variable.labels[110] <- "Basandoti sulle caratteristiche che ti vengono mostrate, quale fra questi due ospiti ti ispira più fiducia? .UserTable {         border-collapse: collapse; /* Collapse borders */         width: 100%; /* Make the table span the full width of its container */     }     .UserTable td, .UserTable th {         border: 1px solid black; /* Add black border */         padding: 10px; /* Keep your padding */         text-align: center; /* Center-align text */         width: 33.33%; /* Set each column to a third of the table width */     }    	 		 			   			 			Ospite 1 			  			 			Ospite 2 			  		 		 			Professione  			{if(job15== \"1\", \"Economista\", if(job15==\"2\",\"Filosofo/a\", if(job15==\"3\", \"Giornalista\", if(job15==\"4\", \"Giurista\", \"Politologo/a\"))))}  			{if(job25== \"1\", \"Economista\", if(job25==\"2\",\"Filosofo/a\", if(job25==\"3\", \"Giornalista\", if(job25==\"4\", \"Giurista\", \"Politologo/a\"))))}  		 		 			Sesso  			{if(gender15== \"1\", \"Femmina\", \"Maschio\")}  			{if(gender25== \"1\", \"Femmina\", \"Maschio\")}  		 		 			Età  			{if(age15== \"1\", \"37\", if(age15==\"2\",\"52\",\"67\"))}  			{if(age25== \"1\", \"37\", if(age25==\"2\",\"52\",\"67\"))}  		 		 			Area italiana di provenienza  			{if(prov15== \"1\", \"Centro\", if(prov15==\"2\",\"Nord\",\"Sud\"))}  			{if(prov25== \"1\", \"Centro\", if(prov25==\"2\",\"Nord\",\"Sud\"))}  		 		 			Preferenze politiche  			{if(ideo15== \"1\", \"Non note\", if(ideo15==\"2\",\"Tendenzialmente di centro\", if(ideo15==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}  			{if(ideo25== \"1\", \"Non note\", if(ideo25==\"2\",\"Tendenzialmente di centro\", if(ideo25==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}"
data[, 110] <- factor(data[, 110], levels=c("AO01","AO02"),labels=c("Ospite 1", "Ospite 2"))
names(data)[110] <- "task5"
# LimeSurvey Field type: A
data[, 111] <- as.character(data[, 111])
attributes(data)$variable.labels[111] <- "Basandoti sulle caratteristiche che ti vengono mostrate, quale fra questi due ospiti ti ispira più fiducia? .UserTable {         border-collapse: collapse; /* Collapse borders */         width: 100%; /* Make the table span the full width of its container */     }     .UserTable td, .UserTable th {         border: 1px solid black; /* Add black border */         padding: 10px; /* Keep your padding */         text-align: center; /* Center-align text */         width: 33.33%; /* Set each column to a third of the table width */     }    	 		 			   			 			Ospite 1 			  			 			Ospite 2 			  		 		 			Professione  			{if(job16== \"1\", \"Economista\", if(job16==\"2\",\"Filosofo/a\", if(job16==\"3\", \"Giornalista\", if(job16==\"4\", \"Giurista\", \"Politologo/a\"))))}  			{if(job26== \"1\", \"Economista\", if(job26==\"2\",\"Filosofo/a\", if(job26==\"3\", \"Giornalista\", if(job26==\"4\", \"Giurista\", \"Politologo/a\"))))}  		 		 			Sesso  			{if(gender16== \"1\", \"Femmina\", \"Maschio\")}  			{if(gender26== \"1\", \"Femmina\", \"Maschio\")}  		 		 			Età  			{if(age16== \"1\", \"37\", if(age16==\"2\",\"52\",\"67\"))}  			{if(age26== \"1\", \"37\", if(age26==\"2\",\"52\",\"67\"))}  		 		 			Area italiana di provenienza  			{if(prov16== \"1\", \"Centro\", if(prov16==\"2\",\"Nord\",\"Sud\"))}  			{if(prov26== \"1\", \"Centro\", if(prov26==\"2\",\"Nord\",\"Sud\"))}  		 		 			Preferenze politiche  			{if(ideo16== \"1\", \"Non note\", if(ideo16==\"2\",\"Tendenzialmente di centro\", if(ideo16==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}  			{if(ideo26== \"1\", \"Non note\", if(ideo26==\"2\",\"Tendenzialmente di centro\", if(ideo26==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}"
data[, 111] <- factor(data[, 111], levels=c("AO01","AO02"),labels=c("Ospite 1", "Ospite 2"))
names(data)[111] <- "task6"
# LimeSurvey Field type: A
data[, 112] <- as.character(data[, 112])
attributes(data)$variable.labels[112] <- "Basandoti sulle caratteristiche che ti vengono mostrate, quale fra questi due ospiti ti ispira più fiducia? .UserTable {         border-collapse: collapse; /* Collapse borders */         width: 100%; /* Make the table span the full width of its container */     }     .UserTable td, .UserTable th {         border: 1px solid black; /* Add black border */         padding: 10px; /* Keep your padding */         text-align: center; /* Center-align text */         width: 33.33%; /* Set each column to a third of the table width */     }    	 		 			   			 			Ospite 1 			  			 			Ospite 2 			  		 		 			Professione  			{if(job17== \"1\", \"Economista\", if(job17==\"2\",\"Filosofo/a\", if(job17==\"3\", \"Giornalista\", if(job17==\"4\", \"Giurista\", \"Politologo/a\"))))}  			{if(job27== \"1\", \"Economista\", if(job27==\"2\",\"Filosofo/a\", if(job27==\"3\", \"Giornalista\", if(job27==\"4\", \"Giurista\", \"Politologo/a\"))))}  		 		 			Sesso  			{if(gender17== \"1\", \"Femmina\", \"Maschio\")}  			{if(gender27== \"1\", \"Femmina\", \"Maschio\")}  		 		 			Età  			{if(age17== \"1\", \"37\", if(age17==\"2\",\"52\",\"67\"))}  			{if(age27== \"1\", \"37\", if(age27==\"2\",\"52\",\"67\"))}  		 		 			Area italiana di provenienza  			{if(prov17== \"1\", \"Centro\", if(prov17==\"2\",\"Nord\",\"Sud\"))}  			{if(prov27== \"1\", \"Centro\", if(prov27==\"2\",\"Nord\",\"Sud\"))}  		 		 			Preferenze politiche  			{if(ideo17== \"1\", \"Non note\", if(ideo17==\"2\",\"Tendenzialmente di centro\", if(ideo17==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}  			{if(ideo27== \"1\", \"Non note\", if(ideo27==\"2\",\"Tendenzialmente di centro\", if(ideo27==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}"
data[, 112] <- factor(data[, 112], levels=c("AO01","AO02"),labels=c("Ospite 1", "Ospite 2"))
names(data)[112] <- "task7"
# LimeSurvey Field type: A
data[, 113] <- as.character(data[, 113])
attributes(data)$variable.labels[113] <- "Basandoti sulle caratteristiche che ti vengono mostrate, quale fra questi due ospiti ti ispira più fiducia? .UserTable {         border-collapse: collapse; /* Collapse borders */         width: 100%; /* Make the table span the full width of its container */     }     .UserTable td, .UserTable th {         border: 1px solid black; /* Add black border */         padding: 10px; /* Keep your padding */         text-align: center; /* Center-align text */         width: 33.33%; /* Set each column to a third of the table width */     }    	 		 			   			 			Ospite 1 			  			 			Ospite 2 			  		 		 			Professione  			{if(job18== \"1\", \"Economista\", if(job18==\"2\",\"Filosofo/a\", if(job18==\"3\", \"Giornalista\", if(job18==\"4\", \"Giurista\", \"Politologo/a\"))))}  			{if(job28== \"1\", \"Economista\", if(job28==\"2\",\"Filosofo/a\", if(job28==\"3\", \"Giornalista\", if(job28==\"4\", \"Giurista\", \"Politologo/a\"))))}  		 		 			Sesso  			{if(gender18== \"1\", \"Femmina\", \"Maschio\")}  			{if(gender28== \"1\", \"Femmina\", \"Maschio\")}  		 		 			Età  			{if(age18== \"1\", \"37\", if(age18==\"2\",\"52\",\"67\"))}  			{if(age28== \"1\", \"37\", if(age28==\"2\",\"52\",\"67\"))}  		 		 			Area italiana di provenienza  			{if(prov18== \"1\", \"Centro\", if(prov18==\"2\",\"Nord\",\"Sud\"))}  			{if(prov28== \"1\", \"Centro\", if(prov28==\"2\",\"Nord\",\"Sud\"))}  		 		 			Preferenze politiche  			{if(ideo18== \"1\", \"Non note\", if(ideo18==\"2\",\"Tendenzialmente di centro\", if(ideo18==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}  			{if(ideo28== \"1\", \"Non note\", if(ideo28==\"2\",\"Tendenzialmente di centro\", if(ideo28==\"3\", \"Tendenzialmente di destra\", \"Tendenzialmente di sinistra\")))}"
data[, 113] <- factor(data[, 113], levels=c("AO01","AO02"),labels=c("Ospite 1", "Ospite 2"))
names(data)[113] <- "task8"
# LimeSurvey Field type: A
data[, 114] <- as.character(data[, 114])
attributes(data)$variable.labels[114] <- "Questa domanda ci serve per capire se stai leggendo con attenzione le domande che ti poniamo. A questa domanda, devi rispondere selezionando l’opzione “Un calciatore”. Non devi dare la risposta corretta alla domanda. Devi solo selezionare “Un calciatore”. Chi è Sergio Mattarella?"
data[, 114] <- factor(data[, 114], levels=c("AO01","AO02","AO03","AO04"),labels=c("Il presidente della Repubblica", "Un magistrato", "Un giornalista", "Un calciatore"))
names(data)[114] <- "check2"
# LimeSurvey Field type: A
data[, 115] <- as.character(data[, 115])
attributes(data)$variable.labels[115] <- "[Conflitti in medio-oriente] Nello specifico, questa settimana il talk show politico discuterà i seguenti temi. Per ogni tema, indica quale ospite pensi sia più competente per discuterne.  "
data[, 115] <- factor(data[, 115], levels=c("giorn","giur","poli","filo","econ","soci"),labels=c("Giornalista", "Giurista", "Politologo/a", "Filosofo/a", "Economista", "Sociologo/a"))
names(data)[115] <- "competence_conflitti"
# LimeSurvey Field type: A
data[, 116] <- as.character(data[, 116])
attributes(data)$variable.labels[116] <- "[Elezioni Europee] Nello specifico, questa settimana il talk show politico discuterà i seguenti temi. Per ogni tema, indica quale ospite pensi sia più competente per discuterne.  "
data[, 116] <- factor(data[, 116], levels=c("giorn","giur","poli","filo","econ","soci"),labels=c("Giornalista", "Giurista", "Politologo/a", "Filosofo/a", "Economista", "Sociologo/a"))
names(data)[116] <- "competence_elezioni"
# LimeSurvey Field type: A
data[, 117] <- as.character(data[, 117])
attributes(data)$variable.labels[117] <- "[Riforma della tassazione sul reddito delle persone (IRPEF)] Nello specifico, questa settimana il talk show politico discuterà i seguenti temi. Per ogni tema, indica quale ospite pensi sia più competente per discuterne.  "
data[, 117] <- factor(data[, 117], levels=c("giorn","giur","poli","filo","econ","soci"),labels=c("Giornalista", "Giurista", "Politologo/a", "Filosofo/a", "Economista", "Sociologo/a"))
names(data)[117] <- "competence_tassazione"
# LimeSurvey Field type: A
data[, 118] <- as.character(data[, 118])
attributes(data)$variable.labels[118] <- "[Violenza sulle donne e femminicidi] Nello specifico, questa settimana il talk show politico discuterà i seguenti temi. Per ogni tema, indica quale ospite pensi sia più competente per discuterne.  "
data[, 118] <- factor(data[, 118], levels=c("giorn","giur","poli","filo","econ","soci"),labels=c("Giornalista", "Giurista", "Politologo/a", "Filosofo/a", "Economista", "Sociologo/a"))
names(data)[118] <- "competence_femminicidi"
# LimeSurvey Field type: A
data[, 119] <- as.character(data[, 119])
attributes(data)$variable.labels[119] <- "[Riforma costituzionale] Nello specifico, questa settimana il talk show politico discuterà i seguenti temi. Per ogni tema, indica quale ospite pensi sia più competente per discuterne.  "
data[, 119] <- factor(data[, 119], levels=c("giorn","giur","poli","filo","econ","soci"),labels=c("Giornalista", "Giurista", "Politologo/a", "Filosofo/a", "Economista", "Sociologo/a"))
names(data)[119] <- "competence_costituzionale"
# LimeSurvey Field type: A
data[, 120] <- as.character(data[, 120])
attributes(data)$variable.labels[120] <- "[Presenza di movimenti e gruppi neofascisti] Nello specifico, questa settimana il talk show politico discuterà i seguenti temi. Per ogni tema, indica quale ospite pensi sia più competente per discuterne.  "
data[, 120] <- factor(data[, 120], levels=c("giorn","giur","poli","filo","econ","soci"),labels=c("Giornalista", "Giurista", "Politologo/a", "Filosofo/a", "Economista", "Sociologo/a"))
names(data)[120] <- "competence_neofascisti"
# LimeSurvey Field type: A
data[, 121] <- as.character(data[, 121])
attributes(data)$variable.labels[121] <- "[Crisi di governo] Nello specifico, questa settimana il talk show politico discuterà i seguenti temi. Per ogni tema, indica quale ospite pensi sia più competente per discuterne.  "
data[, 121] <- factor(data[, 121], levels=c("giorn","giur","poli","filo","econ","soci"),labels=c("Giornalista", "Giurista", "Politologo/a", "Filosofo/a", "Economista", "Sociologo/a"))
names(data)[121] <- "competence_crisigoverno"
# LimeSurvey Field type: A
data[, 122] <- as.character(data[, 122])
attributes(data)$variable.labels[122] <- "[Competente] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 122] <- factor(data[, 122], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[122] <- "associations_competente"
# LimeSurvey Field type: A
data[, 123] <- as.character(data[, 123])
attributes(data)$variable.labels[123] <- "[Si esprime in modo comprensibile] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 123] <- factor(data[, 123], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[123] <- "associations_comprensibile"
# LimeSurvey Field type: A
data[, 124] <- as.character(data[, 124])
attributes(data)$variable.labels[124] <- "[Imparziale] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 124] <- factor(data[, 124], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[124] <- "associations_imparziale"
# LimeSurvey Field type: A
data[, 125] <- as.character(data[, 125])
attributes(data)$variable.labels[125] <- "[Discute senza alzare la voce] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 125] <- factor(data[, 125], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[125] <- "associations_discute"
# LimeSurvey Field type: A
data[, 126] <- as.character(data[, 126])
attributes(data)$variable.labels[126] <- "[Snob] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 126] <- factor(data[, 126], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[126] <- "associations_snob"
# LimeSurvey Field type: A
data[, 127] <- as.character(data[, 127])
attributes(data)$variable.labels[127] <- "[Usa dati numerici e analisi statistiche] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 127] <- factor(data[, 127], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[127] <- "associations_quanti"
# LimeSurvey Field type: A
data[, 128] <- as.character(data[, 128])
attributes(data)$variable.labels[128] <- "[Politicamente di sinistra] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 128] <- factor(data[, 128], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[128] <- "associations_sinistra"
# LimeSurvey Field type: A
data[, 129] <- as.character(data[, 129])
attributes(data)$variable.labels[129] <- "[Noioso] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 129] <- factor(data[, 129], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[129] <- "associations_noioso"
# LimeSurvey Field type: A
data[, 130] <- as.character(data[, 130])
attributes(data)$variable.labels[130] <- "[Analizza casi concreti conducendo interviste e andando sul campo] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 130] <- factor(data[, 130], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[130] <- "associations_quali"
# LimeSurvey Field type: A
data[, 131] <- as.character(data[, 131])
attributes(data)$variable.labels[131] <- "[Elabora teorie per spiegare i fatti politici] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 131] <- factor(data[, 131], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[131] <- "associations_teorico"
# LimeSurvey Field type: A
data[, 132] <- as.character(data[, 132])
attributes(data)$variable.labels[132] <- "[Simpatico] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 132] <- factor(data[, 132], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[132] <- "associations_simpa"
# LimeSurvey Field type: A
data[, 133] <- as.character(data[, 133])
attributes(data)$variable.labels[133] <- "[Autorevole] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 133] <- factor(data[, 133], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[133] <- "associations_autorevole"
# LimeSurvey Field type: A
data[, 134] <- as.character(data[, 134])
attributes(data)$variable.labels[134] <- "[Presuntuoso] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 134] <- factor(data[, 134], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[134] <- "associations_presuntuoso"
# LimeSurvey Field type: A
data[, 135] <- as.character(data[, 135])
attributes(data)$variable.labels[135] <- "[Rigoroso] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 135] <- factor(data[, 135], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[135] <- "associations_rigoroso"
# LimeSurvey Field type: A
data[, 136] <- as.character(data[, 136])
attributes(data)$variable.labels[136] <- "[Maschio] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 136] <- factor(data[, 136], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[136] <- "associations_maschio"
# LimeSurvey Field type: A
data[, 137] <- as.character(data[, 137])
attributes(data)$variable.labels[137] <- "[Ha 60 anni o più] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 137] <- factor(data[, 137], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[137] <- "associations_anziano"
# LimeSurvey Field type: A
data[, 138] <- as.character(data[, 138])
attributes(data)$variable.labels[138] <- "[Analizza i fatti senza farsi condizionare dalle sue opinioni] Adesso verranno elencate alcune caratteristiche. Quanto pensi che ciascuna di queste caratteristiche rispecchi la figura del/della politologo/a?"
data[, 138] <- factor(data[, 138], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla", "Poco", "Né poco, né molto", "Abbastanza", "Molto"))
names(data)[138] <- "associations_condizionato"
# LimeSurvey Field type: A
data[, 139] <- as.character(data[, 139])
attributes(data)$variable.labels[139] <- "[Elaborazione di modelli di previsione sugli andamenti del mercato] Adesso verranno elencati una serie di argomenti specifici. Associa ciascun argomento alla professione che ritieni più esperta su quel tema. Se ritieni che il tema non sia associabile a nessuna professione, seleziona \"Nessuna delle precedenti\". Una professione può essere associata a più di un tema.  A quale professione possono essere associati questi argomenti?"
data[, 139] <- factor(data[, 139], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Economista", "Filosofo/a", "Giurista", "Politologo/a", "Sociologo/a", "Nessuna delle precedenti"))
names(data)[139] <- "correctanswer_econ"
# LimeSurvey Field type: A
data[, 140] <- as.character(data[, 140])
attributes(data)$variable.labels[140] <- "[Analisi delle dinamiche politiche interne e delle relazioni internazionali] Adesso verranno elencati una serie di argomenti specifici. Associa ciascun argomento alla professione che ritieni più esperta su quel tema. Se ritieni che il tema non sia associabile a nessuna professione, seleziona \"Nessuna delle precedenti\". Una professione può essere associata a più di un tema.  A quale professione possono essere associati questi argomenti?"
data[, 140] <- factor(data[, 140], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Economista", "Filosofo/a", "Giurista", "Politologo/a", "Sociologo/a", "Nessuna delle precedenti"))
names(data)[140] <- "correctanswer_scipol1"
# LimeSurvey Field type: A
data[, 141] <- as.character(data[, 141])
attributes(data)$variable.labels[141] <- "[Studi sulla storia dell\'arte] Adesso verranno elencati una serie di argomenti specifici. Associa ciascun argomento alla professione che ritieni più esperta su quel tema. Se ritieni che il tema non sia associabile a nessuna professione, seleziona \"Nessuna delle precedenti\". Una professione può essere associata a più di un tema.  A quale professione possono essere associati questi argomenti?"
data[, 141] <- factor(data[, 141], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Economista", "Filosofo/a", "Giurista", "Politologo/a", "Sociologo/a", "Nessuna delle precedenti"))
names(data)[141] <- "correctanswer_wrong1"
# LimeSurvey Field type: A
data[, 142] <- as.character(data[, 142])
attributes(data)$variable.labels[142] <- "[Analisi dei sistemi giuridici e applicazione delle leggi] Adesso verranno elencati una serie di argomenti specifici. Associa ciascun argomento alla professione che ritieni più esperta su quel tema. Se ritieni che il tema non sia associabile a nessuna professione, seleziona \"Nessuna delle precedenti\". Una professione può essere associata a più di un tema.  A quale professione possono essere associati questi argomenti?"
data[, 142] <- factor(data[, 142], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Economista", "Filosofo/a", "Giurista", "Politologo/a", "Sociologo/a", "Nessuna delle precedenti"))
names(data)[142] <- "correctanswer_giuri"
# LimeSurvey Field type: A
data[, 143] <- as.character(data[, 143])
attributes(data)$variable.labels[143] <- "[Analisi del comportamento e delle strutture sociali] Adesso verranno elencati una serie di argomenti specifici. Associa ciascun argomento alla professione che ritieni più esperta su quel tema. Se ritieni che il tema non sia associabile a nessuna professione, seleziona \"Nessuna delle precedenti\". Una professione può essere associata a più di un tema.  A quale professione possono essere associati questi argomenti?"
data[, 143] <- factor(data[, 143], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Economista", "Filosofo/a", "Giurista", "Politologo/a", "Sociologo/a", "Nessuna delle precedenti"))
names(data)[143] <- "correctanswer_soci"
# LimeSurvey Field type: A
data[, 144] <- as.character(data[, 144])
attributes(data)$variable.labels[144] <- "[Storia dell’India moderna e contemporanea] Adesso verranno elencati una serie di argomenti specifici. Associa ciascun argomento alla professione che ritieni più esperta su quel tema. Se ritieni che il tema non sia associabile a nessuna professione, seleziona \"Nessuna delle precedenti\". Una professione può essere associata a più di un tema.  A quale professione possono essere associati questi argomenti?"
data[, 144] <- factor(data[, 144], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Economista", "Filosofo/a", "Giurista", "Politologo/a", "Sociologo/a", "Nessuna delle precedenti"))
names(data)[144] <- "correctanswer_wrong2"
# LimeSurvey Field type: A
data[, 145] <- as.character(data[, 145])
attributes(data)$variable.labels[145] <- "[Etica, logica e metafisica] Adesso verranno elencati una serie di argomenti specifici. Associa ciascun argomento alla professione che ritieni più esperta su quel tema. Se ritieni che il tema non sia associabile a nessuna professione, seleziona \"Nessuna delle precedenti\". Una professione può essere associata a più di un tema.  A quale professione possono essere associati questi argomenti?"
data[, 145] <- factor(data[, 145], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Economista", "Filosofo/a", "Giurista", "Politologo/a", "Sociologo/a", "Nessuna delle precedenti"))
names(data)[145] <- "correctanswer_filo"
# LimeSurvey Field type: A
data[, 146] <- as.character(data[, 146])
attributes(data)$variable.labels[146] <- "[Progettazione di giardini botanici] Adesso verranno elencati una serie di argomenti specifici. Associa ciascun argomento alla professione che ritieni più esperta su quel tema. Se ritieni che il tema non sia associabile a nessuna professione, seleziona \"Nessuna delle precedenti\". Una professione può essere associata a più di un tema.  A quale professione possono essere associati questi argomenti?"
data[, 146] <- factor(data[, 146], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Economista", "Filosofo/a", "Giurista", "Politologo/a", "Sociologo/a", "Nessuna delle precedenti"))
names(data)[146] <- "correctanswer_wrong3"
# LimeSurvey Field type: A
data[, 147] <- as.character(data[, 147])
attributes(data)$variable.labels[147] <- "[Analisi di risultati elettorali] Adesso verranno elencati una serie di argomenti specifici. Associa ciascun argomento alla professione che ritieni più esperta su quel tema. Se ritieni che il tema non sia associabile a nessuna professione, seleziona \"Nessuna delle precedenti\". Una professione può essere associata a più di un tema.  A quale professione possono essere associati questi argomenti?"
data[, 147] <- factor(data[, 147], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Economista", "Filosofo/a", "Giurista", "Politologo/a", "Sociologo/a", "Nessuna delle precedenti"))
names(data)[147] <- "correctanswer_elections"

saveRDS(data, "data_parsed.RDS")
