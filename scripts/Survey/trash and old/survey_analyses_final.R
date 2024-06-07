pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools
)
#
############ functions
#####
break_string <- function(s, max_length = 35) {
  # If the string is shorter than max_length, return it as is
  if (nchar(s) <= max_length) {
    return(s)
  }
  
  # Find the last space before the max_length
  last_space <- max_length
  while(last_space > 0 && substr(s, last_space, last_space) != " ") {
    last_space <- last_space - 1
  }
  
  # If no space was found, use max_length
  if (last_space == 0) {
    last_space <- max_length
  }
  
  # Break the string into two parts
  part1 <- substr(s, 1, last_space - 1)
  part2 <- substr(s, last_space + 1, nchar(s))
  
  # Recursively apply the function to the second part
  # and combine the parts with a newline character
  return(paste(part1, "\n", break_string(part2, max_length), sep = ""))
}
#####



setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/negri_scaduto_SISP")

import_wd = paste0(getwd(), "/data/survey/")
data = import(paste0(import_wd, "cjdata_ready.RDS"))

data_labeled = import(paste0(import_wd, "data_parsed.RDS"))
output_wd = paste0(getwd(), "/outputs/survey/ita/")

#################################
##### PRELIMINARY OPERATIONS ####

#################################
######
# Rinominare le variabili in italiano e come factor
######

data$c_gender_r = ifelse(data$c_gender_r == "M", "Maschio", "Femmina")
data$Genere = factor(data$c_gender_r)

data$Età = factor(data$c_age_r)

data$Provenienza = factor(data$c_prov_r)

data$Professione = factor(data$c_job_r, levels = c("Giornalista", "Economista", "Filosofo/a",
                                                   "Giurista", "Politologo/a"))
data$Ideologia = factor(data$c_ideo_r, levels = c("Non note", "Centro", "Destra", "Sinistra"))

data$c_inoutgr =ifelse(data$c_inoutgr == "Non note", "Non noto", data$c_inoutgr)
data$Gruppo = factor(data$c_inoutgr, levels = c("Non noto", "Ingroup", "Outgroup"))
                     
data$educ_r = ifelse(data$educ_r == "Low", "Non laureati", "Laureati")
data$Istruzione = factor(data$educ_r, levels = c("Non laureati", "Laureati"))

data$Genere_risp = factor(data$gender)

                     
#####
# Diagnostics
#####

#### Randomizzazione tutto ok?

plot(cj_freqs(data, chosen ~ Genere + Età + Provenienza + Professione + Ideologia + Gruppo, id = ~id), col="grey")

#oppure con ggplot
aus = cj_freqs(data, chosen ~ Genere + Età + Provenienza + Professione + Ideologia + Gruppo, id = ~id)

v = list()

for(i in unique(aus$feature))
{
  
  p = aus |>
    filter(feature == i) |>
    ggplot(aes(x=level, y=estimate))+
    geom_col()+
    ylab("")+
    xlab("")+
    ggtitle(as.character(i))+
    scale_y_continuous(breaks = seq(0, round(max(aus$estimate), digits = -1), by = round(max(aus$estimate)/10, digits = -1)),
                       limits = c(0,max(aus$estimate)+1))+
    coord_flip()+
    theme(text = element_text(size = 15),
          legend.position = "none",
          plot.title = element_text(size=14))
  
  v[[i]] = p
}
p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p

ggsave(paste0(output_wd,"diagnostic randomization_ita.eps"), p, height = 10, width = 8)


#### Preferenza per il profilo a destra?

data$profile_fac <- factor(data$profile_number)
plot(cj(data, 
        chosen ~ Genere + Età + Provenienza + Professione + Ideologia + Gruppo, 
        id = ~id,
        by = ~profile_fac,
        group = "profile_fac",
        estimate = "mm"),
     vline = 0.5)

#se non ci sono differenze significative rispetto alla zero, non c'è preferenza 
#particolare per il profilo a destra

#####

#################################
##### GENERAL  ANALYSIS #########
#################################

#AMCE Base
#####

amce <- cjoint::amce(chosen ~ 
                           Genere +
                           Età+
                           Provenienza+
                           Professione+
                           #Ideologia,
                           Gruppo,
                         data = data, 
                         cluster=TRUE,
                         respondent.id="id")

summary(amce)

plot(amce) +
  theme_bw() +
  theme(text = element_text(size=18)) + 
  theme(legend.position = "none")

######
### Marginal means base
######
mm <- cj(data,
         chosen ~ Genere + Età + Provenienza + Professione + Professione + Ideologia,
         id = ~id,
         estimate = "mm")

mm$variable = paste0("(", mm$feature, ") \n", mm$level)

mm$variable <- fct_reorder(mm$variable, desc(mm$estimate))


p = ggplot(mm)+
  geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable), col='black')+
  ylab("Attributo")+
  #xlab("Media marginale")+
  theme(legend.position = "none")+
  ggtitle("Media marginale")

p

ggsave(paste0(output_wd,"mm generale_unito_con_ideologia_ita.eps"), p, height = 10, width = 8)



mm <- cj(data,
         chosen ~ Genere + Età + Provenienza + Professione + Professione + Gruppo,
         id = ~id,
         estimate = "mm")

mm$variable = paste0("(", mm$feature, ") \n", mm$level)

mm$variable <- fct_reorder(mm$variable, desc(mm$estimate))


p = ggplot(mm)+
  geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable), col='black')+
  ylab("Attributo")+
  #xlab("Media marginale")+
  theme(legend.position = "none")+
  ggtitle("Media marginale")

p

ggsave(paste0(output_wd,"mm generale_unito_con_inoutgroup_ita.eps"), p, height = 10, width = 8)



mm <- cj(data,
         chosen ~ Genere + Età + Provenienza + Professione + Professione + Gruppo + Ideologia,
         id = ~id,
         estimate = "mm")

mm$variable = paste0("(", mm$feature, ") \n", mm$level)

mm$variable <- fct_reorder(mm$variable, desc(mm$estimate))


p = ggplot(mm)+
  geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable), col='black')+
  ylab("Attributo")+
  #xlab("Media marginale")+
  theme(legend.position = "none")+
  ggtitle("Media marginale")

p=p+plot_annotation(title="Medie marginali di ogni caratteristica")

p

ggsave(paste0(output_wd,"mm generale_unito_con_inoutgroup_e_ideologia_assieme_ita.eps"), p, height = 10, width = 8)


#####
# Marignal means ma con pannelli separati
#####

mm <- cj(data,
         chosen ~ Genere + Età + Provenienza + Professione + Ideologia + Gruppo,
         id = ~id,
         estimate = "mm")

export(data.frame(mm), paste0(output_wd,"mm_totale_table.xlsx"))

v <- list()

for(i in unique(mm$feature))
{
  
  aus = mm |>
    filter(feature == i) 
  
  #to have it ordered
  aus$level <- fct_reorder(aus$level, desc(aus$estimate))
  
  p=ggplot(aus, aes(x=estimate, y=level)) +
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_point()+
    geom_pointrange(aes(xmin=lower, xmax=upper))+
    scale_x_continuous(limits=c(min(mm$lower)-0.01, max(mm$upper)+0.01, by=0.05))+
    ylab("")+
    xlab("")+
    ggtitle(as.character(i))+
    theme(text = element_text(size = 15),
          legend.position = "none",
          plot.title = element_text(size=14))
  
  v[[i]] <- p
}

v[[length(unique(mm$feature))]] = v[[length(unique(mm$feature))]]+xlab("Media marginale")
p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p
ggsave(paste0(output_wd,"mm generale_separato_ideo_e_inoutgroup_ita.eps"), p, height = 10, width = 8)


#################################
##### SUBGROUP ANALYSIS #########
#################################

#####
# subgroup con amce
#####
amce <- cjoint::amce(chosen ~ 
                       Genere +
                       Età+
                       Provenienza+
                       Professione+
                       Professione*Istruzione+
                       Ideologia,
                     data = data, 
                     cluster=TRUE,
                     respondent.id="id")

summary(amce)
#attento all'effetto della politica!!!

plot(amce) +  scale_colour_grey(start = 0, end = .5) +
  theme_bw() + theme(text = element_text(size=18)) + 
  theme(legend.position = "none")

######
# subgroup con mm, istruzione rispondente con tutto cj
######

mm <- cj(data,
         chosen ~ Genere + Età + Provenienza + Professione + Ideologia+Gruppo,
         id = ~id,
         estimate = "mm",
         by = ~Istruzione)

v <- list()

for(i in unique(mm$feature))
{ 
  data1 = mm |>
    filter(feature == i & BY == "Laureati") 
  
  data2 = mm |>
    filter(feature == i & BY == "Non laureati") 
  
  p=ggplot(data1)+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level),
                  position = position_nudge(y = 1/10),
                  col="darkgrey",
                  shape = 17)+
  geom_pointrange(data = data2,
                  aes(x=estimate, xmin=lower, xmax=upper, y=level),
                  position = position_nudge(y = -1/10),
                  col="black")+
  ylab("")+
  xlab("")+
  scale_x_continuous(breaks=seq(0.1,0.9, by=0.1), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
  ggtitle(as.character(i))+
  theme(text = element_text(size=15))

  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Media marginale")+labs(caption="Nero = Non laureati (rispondenti)\nGrigio = Laureati (rispondenti) ")

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p=p+plot_annotation(title="Medie marginali di ogni caratteristica per sottogruppo di rispondenti\n(laureati vs non laureati)")

ggsave(paste0(output_wd, "mm by istruzione rispondente_ita.eps"), p, height = 12, width = 8)


### Differences


mm <- cj(data,
         chosen ~ Genere + Età + Provenienza + Professione + Ideologia+Gruppo,
         id = ~id,
         estimate = "mm_differences",
         by = ~Istruzione, 
         alpha=0.10 #90% CI!
        )

v <- list()

for(i in unique(mm$feature))
{ 
  aus = mm |>
    filter(feature == i)
  
  p=ggplot(aus)+
    geom_vline(aes(xintercept=0), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level))+
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks=seq(0.1,0.9, by=0.1), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
    ggtitle(as.character(i))+
    theme(text = element_text(size=15))
  
  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Differenza media marginale Laureati-Non laureati (90% C.I)")

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p

ggsave(paste0(output_wd, "mm by istruzione rispondente, diffs_ita.eps"), p, height = 12, width = 8)



####### 
### subgroup con macroarea rispondente
######

data$macroarea2 = ifelse(data$macroarea1 == "Nord-ovest" | data$macroarea1 == "Nord-est", "Nord", 
                         ifelse(data$macroarea1=="Centro", "Centro", "Sud"))
table(data$macroarea2, data$macroarea1)
data$macroarea2 = factor(data$macroarea2)

mm <- cj(data,
         chosen ~ Genere + Età + Provenienza + Professione + Ideologia+ Gruppo,
         id = ~id,
         estimate = "mm",
         by = ~macroarea2)

v <- list()

for(i in unique(mm$feature))
{ 
  data1 = mm |>
    filter(feature == i & BY == "Centro") 
  
  data2 = mm |>
    filter(feature == i & BY == "Nord") 
  
  data3 = mm |>
    filter(feature == i & BY == "Sud") 
  
  p=ggplot(data1)+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    col="black",
                    shape = 17)+
    geom_pointrange(data = data2,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = -1/10),
                    col="red")+
    
    geom_pointrange(data = data3,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = 1/10),
                    col="blue")+
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks=seq(0.1,0.9, by=0.1), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
    ggtitle(as.character(i))+
    theme(text = element_text(size=15))
  
  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Media marginale")+labs(caption="Nero=Centro (rispondenti),\nRosso=Nord (rispondenti),\nBlu=Sud e isole (rispondenti)")

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p=p+plot_annotation(title="Medie marginali di ogni caratteristica per sottogruppo di rispondenti\n(nord vs centro vs sud)")


ggsave(paste0(output_wd, "mm by regione rispondente_ita.eps"), p, height = 18, width = 8)




# subgroup con mm, genere rispondente con tutto cj
#####

aus = data[data$Genere_risp == "Maschio" | data$Genere_risp == "Femmina", ]

aus$Genere_risp = factor(aus$Genere_risp, levels =c("Maschio", "Femmina"))

mm <- cregg::cj(aus,
         chosen ~ Genere + Età + Provenienza + Professione + Ideologia+Gruppo,
         id = ~id,
         estimate = "mm",
         by = ~Genere_risp)

v <- list()

for(i in unique(mm$feature))
{ 
  data1 = mm |>
    filter(feature == i & BY == "Femmina") 
  
  data2 = mm |>
    filter(feature == i & BY == "Maschio") 
  
  p=ggplot(data1)+
    geom_vline(aes(xintercept=0.5), col="darkgrey", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = 1/10),
                    col="darkgrey",
                    shape = 17)+
    geom_pointrange(data = data2,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = -1/10),
                    col="black")+
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks=seq(0.1,0.9, by=0.05), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
    ggtitle(as.character(i))+
    theme(text = element_text(size=15))
  
  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Media marginale")+labs(caption="Nero = Maschi (rispondenti)\nGrigio = Femmine (rispondenti)")

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p=p+plot_annotation(title="Medie marginali di ogni caratteristica per sottogruppo di rispondenti\n(maschi vs femmine)")

ggsave(paste0(output_wd, "mm by genere rispondente_ita.eps"), p, height = 12, width = 8)

### differences

mm <- cregg::cj(aus,
                chosen ~ Genere + Età + Provenienza + Professione + Ideologia+Gruppo,
                id = ~id,
                estimate = "mm_differences",
                by = ~Genere_risp, 
                alpha=0.10 #90% CI!
                )

v <- list()

for(i in unique(mm$feature))
{ 
  aus = mm |>
    filter(feature == i) 
  
  
  p=ggplot(aus)+
    geom_vline(aes(xintercept=0), col="darkgrey", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level))+
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks=seq(0.1,0.9, by=0.05), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
    ggtitle(as.character(i))+
    theme(text = element_text(size=15))
  
  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Media marginale rispondenti femmine-rispondenti maschi  (90% C.I)")

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p

ggsave(paste0(output_wd, "mm by genere rispondente, diffs_ita.eps"), p, height = 12, width = 8)



#################################
#### CJ INTERACTIONS (ACIE) ####
#################################

#####
# Professione con genere (cj) AMCE
#####
# 
# amce <- cjoint::amce(chosen ~ 
#                         Genere +
#                         Età+
#                         Provenienza+
#                         Professione+
#                         Ideologia+
#                        Professione*Genere,
#                      data = data, 
#                      cluster=TRUE,
#                      respondent.id="id")
# 
# summary(amce)
# 
# plot(amce) +  scale_colour_grey(start = 0, end = .5) +
#   theme_bw() + theme(text = element_text(size=18)) + 
#   theme(legend.position = "none")
# 
# #####
# #Professione con ingroup (cj) AMCE
# #####
# 
# amce <- cjoint::amce(chosen ~ 
#                        Genere +
#                        Età+
#                        Provenienza+
#                        Professione+
#                        Gruppo+
#                        Professione*Gruppo,
#                      data = data, 
#                      cluster=TRUE,
#                      respondent.id="id")
# 
# summary(amce)
# 
# plot(amce) +  scale_colour_grey(start = 0, end = .5) +
#   theme_bw() + theme(text = element_text(size=18)) + 
#   theme(legend.position = "none")
# 
# #####
# Professione con genere (cj), MM
#####

mm <- cj(data,
         chosen ~ Professione,
         id = ~id,
         estimate = "mm",
         by = ~Genere)

v <- list()

for(i in unique(mm$feature))
{ 
  data1 = mm |>
    filter(feature == i & BY == "Femmina") 
  
  data2 = mm |>
    filter(feature == i & BY == "Maschio") 
  
  p=ggplot(data1)+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = 1/10),
                    col="darkgrey",
                    shape = 17)+
    geom_pointrange(data = data2,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = -1/10),
                    col="black")+
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks=seq(0.1,0.9, by=0.1), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
    theme(text = element_text(size=15),
          axis.text.x = element_text(size=15),
          axis.text.y = element_text(size=15))
  
  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Media marginale")+labs(caption="Triangolo = Femmine (profili)\n Cerchio = Maschi (profili)")

p = v[[1]]

p

ggsave(paste0(output_wd, "ACIE (mm) genere con professione_ita.eps"), p, height = 8, width = 12)

# plotto le differenze

mm_diffs <- cj(data,
               chosen ~ Professione,
               id = ~id,
               estimate = "mm_differences",
               by = ~Genere)

mm_diffs$level = fct_reorder(mm_diffs$level, desc(mm_diffs$estimate))

p= ggplot(mm_diffs)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level),
                  #position = position_nudge(y = -1/10),
                  col="black")+ 
  ylab("")+
  xlab("Differenza media marginale profili maschi-profili femmine")+
  geom_vline(aes(xintercept = 0),
             col="darkgrey")+
  scale_x_continuous(breaks=seq(-0.2,0.2, by=0.1), limits = c(-0.2, 0.2))+
  theme(text = element_text(size=15))

p

ggsave(paste0(output_wd, "ACIE (mm) genere con professione, diff_ita.eps"), p, width = 12, height = 8)


######
# Professione con ingroup (cj), MM
#####

mm <- cj(data,
         chosen ~ Professione,
         id = ~id,
         estimate = "mm",
         by = ~Gruppo)

v <- list()

for(i in unique(mm$feature))
{ 
  data1 = mm |>
    filter(feature == i & BY == "Ingroup")

  data2 = mm |>
    filter(feature == i & BY == "Non noto") 
  
  data3 = mm |>
    filter(feature == i & BY == "Outgroup")
  
  p=ggplot(data1)+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = 1/10),
                    col="darkgrey",
                    shape = 17)+
    geom_pointrange(data = data2,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level))+
    geom_pointrange(data = data3,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = -1/10),
                    col="darkgrey",
                    shape = 15)+
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks=seq(0.1,0.9, by=0.1), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
    theme(text = element_text(size=15),
          axis.text.x = element_text(size=15),
          axis.text.y = element_text(size=15))
  
  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Media marginale")+labs(caption="Cerchio = Non noto\n Triangolo = Ingroup\n Quadrato = Outgroup")

p = v[[1]]

p

ggsave(paste0(output_wd, "ACIE (mm) Gruppo con professione_ita.eps"), p, height = 8, width = 12)

# plotto le differenze ingroup vs outgroup

aus = data |>
  filter(Gruppo != "Non noto")

aus$Gruppo = factor(aus$Gruppo, levels = c("Outgroup", "Ingroup"))

mm_diffs <- cj(aus,
               chosen ~ Professione,
               id = ~id,
               estimate = "mm_differences",
               by = ~Gruppo)

mm_diffs$level = fct_reorder(mm_diffs$level, desc(mm_diffs$estimate))


p = ggplot(mm_diffs)+
  geom_vline(aes(xintercept=0), col="black", alpha=1/4)+
  geom_pointrange(data = mm_diffs,
                  aes(x=estimate, xmin=lower, xmax=upper, y=level))+
  ylab("")+
  xlab("Differenza media marginale Ingroup-Outgroup")+
  scale_x_continuous(breaks=seq(-0.5,0.5, by=0.1), limits = c(0-0.05,
                                                              max(mm_diffs$upper)+0.05))+
  theme(text = element_text(size=15))

p

ggsave(paste0(output_wd, "ACIE (mm) Gruppo con professione, diff ingroup vs outgroup_ita.eps"), p, width = 12, height = 8)



# plotto le differenze ingroup vs non note

aus = data |>
  filter(Gruppo != "Outgroup")

aus$Gruppo = factor(aus$Gruppo, levels = c("Non noto", "Ingroup"))

mm_diffs <- cj(aus,
               chosen ~ Professione,
               id = ~id,
               estimate = "mm_differences",
               by = ~Gruppo)

mm_diffs$level = fct_reorder(mm_diffs$level, desc(mm_diffs$estimate))


p = ggplot(mm_diffs)+
  geom_vline(aes(xintercept=0), col="black", alpha=1/4)+
  geom_pointrange(data = mm_diffs,
                  aes(x=estimate, xmin=lower, xmax=upper, y=level))+
  ylab("")+
  xlab("Differenza media marginale Ingroup-Non noto")+
  scale_x_continuous(breaks=seq(-0.5,0.5, by=0.1), limits = c(0-0.05,
                                                              max(mm_diffs$upper)+0.05))+
  theme(text = element_text(size=15))

p

ggsave(paste0(output_wd, "ACIE (mm) Gruppo con professione, diff ingroup vs non noto_ita.eps"), p, width = 12, height = 8)


###############################
#### DESCRITTIVE SULLA SURVEY
#### NO CONJOINT
###############################


s_data = import(paste0(getwd(), "/data/survey/","data_survey_ready.RDS"))

### check the missing values!!!!!!

datanomiss = s_data #na.omit(s_data)

##### 
# Descriptive tables
#####

descriptive <- datanomiss |> 
  select(gender, age_r, macroarea1, educ, socposition_SQ001) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, #every statistic shows 1 decimal place
    #  type = all_continuous() ~ "continuous2",
    # statistic = all_continuous() ~ c("{mean}", "{min}", "{max}"),
    label = list(
      gender ~ "Genere",
      age_r ~ "Età",
      macroarea1 ~ "Macroarea",
      educ ~ "Istruzione",
      socposition_SQ001 ~ "Autopiazzamento scala sociale"
    )
  )

#export to word

descriptive |>
  as_gt() |>
  gtsave(filename = paste0(output_wd, "Descriptive_sociodemo.docx"))


descriptive_politics <- datanomiss |> 
  select(ideology_SQ001, vote, interest, exposure, trust_gove_r,
         trust_part_r, trust_medi_r, trust_expe_r) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, #every statistic shows 1 decimal place
    type = list(ideology_SQ001 ~ "categorical", 
                vote ~ "categorical", 
                interest ~ "categorical", 
                exposure ~ "categorical", 
                trust_gove_r ~ "continuous",
                trust_part_r ~ "continuous",
                trust_medi_r ~ "continuous",
                trust_expe_r ~ "continuous"),
    label = list(
      ideology_SQ001 ~ "Ideologia politica", 
      vote ~ "Preferenze di voto", 
      interest ~ "Interesse per la politca", 
      exposure ~ "Esposizione ai news media", 
      trust_gove_r ~ "Fiducia nel governo",
      trust_part_r ~ "Fiducia nei partiti",
      trust_medi_r ~ "Fiducia nei media",
      trust_expe_r ~ "Fiducia negli esperti"
    )
  )

descriptive_politics |>
  as_gt() |>
  gtsave(filename = paste0(output_wd,"Descriptive_pol.docx"))


# Le altre domande: COMPETENCEle materie di competenza del politologo. 
#Pe rcompetece faccio grouped bar plot con le professioni
competence_index = which(grepl("competence", names(datanomiss)))

competence_variables = names(datanomiss)[competence_index]

competence_variables[3]
competence_labels = attributes(data_labeled)$variable.labels[competence_index]
xlabs =sub("^\\[(.*?)\\].*$", "\\1", competence_labels)


xlabs

v=list()
for(i in competence_variables)
{
  p = ggplot(datanomiss, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    ylab("")+
    scale_y_continuous(labels = scales::percent_format())+
    theme(axis.text.x = element_text(angle = 22.5, hjust = 1))
  
  v[[i]] = p
}

for(i in 1:length(xlabs))
{
  v[[i]] = v[[i]] + xlab(xlabs[i])
}

title = "Per ogni tema, indica quale ospite pensi sia più competente per discuterne."#break_string(gsub("^\\[.*?\\]\\s*", "", correctanswer_labels[1]), max_length = 80)


p =v[[1]]+v[[2]]+v[[3]]+v[[4]]+v[[5]]+v[[6]]+v[[7]]+plot_layout(ncol = 3)+plot_annotation(title=title)

p

ggsave(paste0(output_wd,"competence_descriptive_ita.eps"), p, height = 10, width = 10)

#Su quella delle associazioni: diamond (geom_point) in orizzontale ordinando per il voto
#medio per ogni caratteristica

associations_index = which(grepl("associations", names(datanomiss)) & !grepl("__r", names(datanomiss)))

associations_variables = names(datanomiss)[associations_index]
associations_labels = attributes(data_labeled)$variable.labels[associations_index]

xlabs =sub("^\\[(.*?)\\].*$", "\\1", associations_labels)

for(i in 1:length(xlabs))
{
  xlabs[i] = break_string(xlabs[i], max_length = 40)
}

title = break_string(gsub("^\\[.*?\\]\\s*", "", associations_labels[1]), max_length = 80)

datanomiss[associations_variables]
v=list()
for(i in associations_variables)
{
  p = ggplot(datanomiss, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format())+
    ylab("")+
    theme(axis.text.x = element_text(angle = 22.5, hjust = 1))
  
  v[[i]] = p
}

v[[12]]
for(i in 1:length(xlabs))
{
  v[[i]] = v[[i]] + xlab(xlabs[i]) # add x axis label as the part of the question that is different in everry subquestion
}
v[[12]]

v[[17]]

vec = ""
for(i in 2:length(xlabs)-1)
{
  vec=paste0(vec, "v[[", i, "]]+")
}

vec=paste0(vec, 
           "v[[", 
           length(xlabs),
           "]]+plot_layout(ncol = 4)+plot_annotation(title=\'",
           title, "\')"
           )

vec

p=eval(parse(text=vec))
p
ggsave(paste0(output_wd,"associations_descriptive_ita.eps"), p, height = 16, width = 12)

#####
# ## CORRECT ANSWERS (CODICE NON VALIDATO! VEDI SE FUNZIONA!!!!)
#######
# 
# # Load necessary libraries
# library(openxlsx)
# 
# # Your data frame
# # data <- Your data frame
# 
# # Number of variables in correctanswer group
# k <- sum(grepl("correctanswer", names(datanomiss)))  # Update this based on your actual number of variables
# 
# var_names = names(datanomiss)[which(grepl("correctanswer", names(datanomiss)))] 
# 
# # Create a new workbook
# wb <- createWorkbook()
# 
# # Loop through each specific variable
# for (i in 1:k) {
#   var_name <- var_names[i]
#   
#   # Check if the variable exists in the dataframe
#   if (!var_name %in% names(data)) {
#     next  # Skip if the variable doesn't exist
#   }
#   
#   # Add a sheet for the current variable
#   sheet_name <- var_name
#   addWorksheet(wb, sheet_name)
#   
#   # Get the frequency table
#   freq_table <- prop.table(table(data[[var_name]]))
#   
#   # Round the frequency table
#   rounded_table <- round(freq_table, digits = 2)
#   
#   # Write the variable name as the sheet title
#   writeData(wb, sheet = sheet_name, x = as.character(var_name), startRow = 1, startCol = 1)
#   
#   # Write the rounded table
#   writeData(wb, sheet = sheet_name, x = as.data.frame(rounded_table), startRow = 2, startCol = 1)
# }
# 
# # Save the workbook to a file
# saveWorkbook(wb, paste0(output_wd, "correctanswerstables.xlsx"), overwrite = TRUE)
# 

### now i try to do a graph with correct answer


correctanswer_index = which(grepl("correctanswer", names(datanomiss)) & !grepl("__r", names(datanomiss)))

correctanswer_variables = names(datanomiss)[correctanswer_index]
correctanswer_labels = attributes(data_labeled)$variable.labels[correctanswer_index]

xlabs =sub("^\\[(.*?)\\].*$", "\\1", correctanswer_labels)

for(i in 1:length(xlabs))
{
  xlabs[i] = break_string(xlabs[i], max_length = 45)
}

title = "A quale professione possono essere associati questi argomenti?"#break_string(gsub("^\\[.*?\\]\\s*", "", correctanswer_labels[1]), max_length = 80)

datanomiss[correctanswer_variables]
v=list()
for(i in correctanswer_variables)
{
  p = ggplot(datanomiss, aes(x=!!sym(i)))+
    geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+
    scale_y_continuous(labels = scales::percent_format())+
    ylab("")+
    theme(axis.text.x = element_text(angle = 22.5, hjust = 1))
  
  v[[i]] = p
}

v[[9]]
for(i in 1:length(xlabs))
{
  v[[i]] = v[[i]] + xlab(xlabs[i]) # add x axis label as the part of the question that is different in everry subquestion
}
v[[9]]

v[[17]]

vec = ""
for(i in 2:length(xlabs)-1)
{
  vec=paste0(vec, "v[[", i, "]]+")
}

vec=paste0(vec, 
           "v[[", 
           length(xlabs),
           "]]+plot_layout(ncol = 3)+plot_annotation(title=\'",
           title, "\')"
)

vec

p=eval(parse(text=vec))

ggsave(paste0(output_wd,"correctanswer_descriptive_ita.eps"), p, height = 12, width = 12)

