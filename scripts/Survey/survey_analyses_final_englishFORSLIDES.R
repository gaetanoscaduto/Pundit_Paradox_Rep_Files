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
output_wd = paste0(getwd(), "/outputs/survey/eng/slides/")

#################################
##### PRELIMINARY OPERATIONS ####

#################################
######
# Rinominare le variabili in italiano e come factor
######

data$c_gender_r = ifelse(data$c_gender_r == "M", "Male", "Female")
data$Gender = factor(data$c_gender_r)

table(data$Gender, useNA = "always")

data$Age = factor(data$c_age_r)

table(data$Age, useNA = "always")

data$c_prov_r = ifelse(data$c_prov==1, "Center",
                       ifelse(data$c_prov==2, "North", "South"))

table(data$c_prov_r, useNA = "always")

data$Area = factor(data$c_prov_r, levels = c("North", "Center", "South"))

table(data$Area, useNA = "always")

data <- data %>%
  mutate(c_job_r = case_when(
    c_job_r == "Giornalista" ~ "Journalist",
    c_job_r == "Economista" ~ "Economist",
    c_job_r == "Giurista" ~ "Jurist",
    c_job_r == "Filosofo" ~ "Philosopher",
    c_job_r == "Politologo" ~ "Political scientist"))

table(data$c_job_r, useNA = "always")
data$Profession = factor(data$c_job_r, levels = c("Journalist", "Economist", "Philosopher",
                                                  "Jurist", "Political scientist"))
table(data$Profession, useNA = "always")

data <- data %>%
  mutate(c_ideo_r = case_when(
    c_ideo_r == "Non note" ~ "Not disclosed",
    c_ideo_r == "Centro" ~ "Centrist",
    c_ideo_r == "Destra" ~ "Right-wing",
    c_ideo_r == "Sinistra" ~ "Left-wing"))

table(data$c_ideo_r, useNA = "always")

data$Ideology = factor(data$c_ideo_r, levels = c("Not disclosed", "Centrist", "Right-wing", "Left-wing"))

table(data$Ideology, useNA = "always")

table(data$c_inoutgr, useNA = "always")
data$c_inoutgr =ifelse(data$c_inoutgr == "Non note", "Unknown", data$c_inoutgr)
table(data$c_inoutgr, useNA = "always")

data$Group = factor(data$c_inoutgr, levels = c("Unknown", "Ingroup", "Outgroup"))

table(data$Group, useNA = "always")

data$educ_r = ifelse(data$educ_r == "Low", "No degree", "Degree")
data$Education = factor(data$educ_r, levels = c("No degree", "Degree"))

data <- data %>%
  mutate(gender = case_when(
    gender == "Maschio" ~ "Male",
    gender == "Femmina" ~ "Female",
    gender == "Altro/Preferisco non specificare" ~ "Other/Prefer not to say"))

data$Gender_risp = factor(data$gender)

table(data$Gender_risp)


#####
# Diagnostics
#####

#### Randomizzazione tutto ok?

plot(cj_freqs(data, chosen ~ Gender + Age + Area + Profession + Ideology + Group, id = ~id), col="grey")

table(data$chosen)
table(data$Gender)
table(data$Age)
table(data$Area)
table(data$Profession)
table(data$Ideology)
table(data$Group)


#oppure con ggplot
aus = cj_freqs(data, chosen ~ Gender + Age + Area + Profession + Ideology + Group, id = ~id)

v = list()

for(i in unique(aus$feature))
{
  
  p = aus |>
    filter(feature == i) |>
    ggplot(aes(y=level, x=estimate))+
    geom_col()+
    ylab("")+
    xlab("")+
    ggtitle(as.character(i))+
    scale_x_continuous(breaks = seq(0, round(max(aus$estimate), digits = -1), by = round(max(aus$estimate)/10, digits = -1)),
                       limits = c(0,max(aus$estimate)+1))+
    # coord_flip()+
    theme(text = element_text(size = 15),
          legend.position = "none",
          plot.title = element_text(size=14))
  
  v[[i]] = p
}
p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p

##ggsave(paste0(output_wd,"diagnostic randomization_eng.png"), p, height = 10, width = 8)


#### Preferenza per il profilo a destra?

data$profile_fac <- factor(data$profile_number)
plot(cj(data, 
        chosen ~ Gender + Age + Area + Profession + Ideology + Group, 
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
                       Gender +
                       Age+
                       Area+
                       Profession+
                       #Ideology,
                       Group,
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
         chosen ~ Gender + Age + Area + Profession + Profession + Ideology,
         id = ~id,
         estimate = "mm")

mm$variable = paste0("(", mm$feature, ") \n", mm$level)

mm$variable <- fct_reorder(mm$variable, desc(mm$estimate))


p = ggplot(mm)+
  geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable), col='black')+
  ylab("Attribute")+
  xlab("Estimate")+
  #xlab("Marginal mean")+
  theme(legend.position = "none")+
  ggtitle("Marginal mean")

p

##ggsave(paste0(output_wd,"mm generale_unito_con_Ideology_eng.png"), p, height = 10, width = 8)



mm <- cj(data,
         chosen ~ Gender + Age + Area + Profession + Profession + Group,
         id = ~id,
         estimate = "mm")

mm$variable = paste0("(", mm$feature, ") \n", mm$level)

mm$variable <- fct_reorder(mm$variable, desc(mm$estimate))


p = ggplot(mm)+
  geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable), col='black')+
  ylab("Attribute")+
  xlab("Marginal mean")+
  xlab("Estimate")+
  theme(legend.position = "none")+
  ggtitle("Marginal mean")

p

##ggsave(paste0(output_wd,"mm generale_unito_con_inoutgroup_eng.png"), p, height = 10, width = 8)



mm <- cj(data,
         chosen ~ Gender + Age + Area + Profession + Profession + Group + Ideology,
         id = ~id,
         estimate = "mm")

mm$variable = paste0("(", mm$feature, ") \n", mm$level)

mm$variable <- fct_reorder(mm$variable, desc(mm$estimate))


p = ggplot(mm)+
  geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable), col='red')+
  ylab("")+
  xlab("Marginal mean")+
  theme(legend.position = "none",
        axis.text.y = element_text(size=17),
        axis.text.x = element_text(size=17))#+
#ggtitle("Marginal mean")

#p=p+plot_annotation(title="Marginal means for every attribute")

p

ggsave(paste0(output_wd,"mm generale_unito_con_inoutgroup_e_Ideology_assieme_eng.png"), p, height = 10, width = 10)


#####
# Marignal means ma con pannelli separati
#####

mm <- cj(data,
         chosen ~ Gender + Age + Area + Profession + Ideology + Group,
         id = ~id,
         estimate = "mm")

export(data.frame(mm), paste0(output_wd,"mm_totale_table_eng.xlsx"))

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

v[[length(unique(mm$feature))]] = v[[length(unique(mm$feature))]]+xlab("Marginal mean")
p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p
#ggsave(paste0(output_wd,"mm generale_separato_ideo_e_inoutgroup_eng.png"), p, height = 10, width = 8)


#################################
##### SUBGROUP ANALYSIS #########
#################################

#####
# subgroup con amce
#####
amce <- cjoint::amce(chosen ~ 
                       Gender +
                       Age+
                       Area+
                       Profession+
                       Profession*Education+
                       Ideology,
                     data = data, 
                     cluster=TRUE,
                     respondent.id="id")

summary(amce)
#attento all'effetto della politica!!!

plot(amce) +  scale_colour_grey(start = 0, end = .5) +
  theme_bw() + theme(text = element_text(size=18)) + 
  theme(legend.position = "none")

######
# subgroup con mm, Education rispondente con tutto cj
######

mm <- cj(data,
         chosen ~ Gender + Age + Area + Profession + Ideology+Group,
         id = ~id,
         estimate = "mm",
         by = ~Education)

v <- list()

for(i in unique(mm$feature))
{ 
  data1 = mm |>
    filter(feature == i & BY == "Degree") 
  
  data2 = mm |>
    filter(feature == i & BY == "No degree") 
  
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

v[[length(v)]] = v[[length(v)]]+xlab("Marginal means")+labs(caption="Black = No degree (respondents)\n Grey = Degree (respondents)")

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p=p+plot_annotation(title="Marginal means for every attribute\nfor different levels of respondents' education")

p

#ggsave(paste0(output_wd, "mm by Education rispondente_eng.png"), p, height = 12, width = 8)


### Differences


mm <- cj(data,
         chosen ~ Gender + Age + Area + Profession + Ideology+Group,
         id = ~id,
         estimate = "mm_differences",
         by = ~Education, 
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

v[[length(v)]] = v[[length(v)]]+xlab("Marginal mean differences Degree-No Degree (90% C.I)")

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p

#ggsave(paste0(output_wd, "mm by Education rispondente, diffs_eng.png"), p, height = 12, width = 8)



####### 
### subgroup con macroarea rispondente
######

data$macroarea2 = ifelse(data$macroarea1 == "Nord-ovest" | data$macroarea1 == "Nord-est", "North", 
                         ifelse(data$macroarea1=="Centro", "Center", "South"))
table(data$macroarea2, data$macroarea1)
data$macroarea2 = factor(data$macroarea2)

mm <- cj(data,
         chosen ~ Gender + Age + Area + Profession + Ideology+ Group,
         id = ~id,
         estimate = "mm",
         by = ~macroarea2)

v <- list()

for(i in unique(mm$feature))
{ 
  data1 = mm |>
    filter(feature == i & BY == "Center") 
  
  data2 = mm |>
    filter(feature == i & BY == "North") 
  
  data3 = mm |>
    filter(feature == i & BY == "South") 
  
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

v[[length(v)]] = v[[length(v)]]+xlab("Marginal mean")+labs(caption="Black=Center (respondents),\nRed=North (respondents),\nBlue=South and islands (respondents)")

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p=p+plot_annotation(title="Marginal means for every attribute by respondents' area\n(north vs center vs south)")


#ggsave(paste0(output_wd, "mm by regione rispondente_eng.png"), p, height = 18, width = 8)




# subgroup con mm, Gender rispondente con tutto cj
#####

aus = data[data$Gender_risp == "Male" | data$Gender_risp == "Female", ]

aus$Gender_risp = factor(aus$Gender_risp, levels =c("Male", "Female"))

mm <- cregg::cj(aus,
                chosen ~ Gender + Age + Area + Profession + Ideology+Group,
                id = ~id,
                estimate = "mm",
                by = ~Gender_risp)

v <- list()

for(i in unique(mm$feature))
{ 
  data1 = mm |>
    filter(feature == i & BY == "Female") 
  
  data2 = mm |>
    filter(feature == i & BY == "Male") 
  
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

v[[length(v)]] = v[[length(v)]]+xlab("Marginal means")+labs(caption="Black = Males (respondents)\nGrey = Females (respondents)")

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p

p=p+plot_annotation(title="Marginal means for every attribute by respondents' gender\n")

p
#ggsave(paste0(output_wd, "mm by Gender rispondente_eng.png"), p, height = 12, width = 8)

### differences

mm <- cregg::cj(aus,
                chosen ~ Gender + Age + Area + Profession + Ideology+Group,
                id = ~id,
                estimate = "mm_differences",
                by = ~Gender_risp, 
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

v[[length(v)]] = v[[length(v)]]+xlab("Marginal means female-male respondents (90% C.I)")

p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]

p

#ggsave(paste0(output_wd, "mm by Gender rispondente, diffs_eng.png"), p, height = 12, width = 8)



#################################
#### CJ INTERACTIONS (ACIE) ####
#################################

#####
# Profession con Gender (cj) AMCE
#####
# 
# amce <- cjoint::amce(chosen ~ 
#                         Gender +
#                         Age+
#                         Area+
#                         Profession+
#                         Ideology+
#                        Profession*Gender,
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
# #Profession con ingroup (cj) AMCE
# #####
# 
# amce <- cjoint::amce(chosen ~ 
#                        Gender +
#                        Age+
#                        Area+
#                        Profession+
#                        Group+
#                        Profession*Group,
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
# Profession con Gender (cj), MM
#####

mm <- cj(data,
         chosen ~ Profession,
         id = ~id,
         estimate = "mm",
         by = ~Gender)

v <- list()

for(i in unique(mm$feature))
{ 
  data1 = mm |>
    filter(feature == i & BY == "Female") 
  
  data2 = mm |>
    filter(feature == i & BY == "Male") 
  
  p=ggplot(data1)+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = 1/10),
                    col="red",
                    shape = 17, 
                    size=2)+
    geom_pointrange(data = data2,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = -1/10),
                    col="blue", 
                    size=2)+
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks=seq(0.1,0.9, by=0.1), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
    theme(text = element_text(size=25))
  
  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Marginal means")+labs(caption="Triangle = Females (profiles)\n Circles = Males (profiles)")

p = v[[1]]

p

ggsave(paste0(output_wd, "ACIE (mm) Gender con Profession_engSLIDE.png"), p, height = 8, width = 12)

# plotto le differenze

mm_diffs <- cj(data,
               chosen ~ Profession,
               id = ~id,
               estimate = "mm_differences",
               by = ~Gender)

mm_diffs$level = fct_reorder(mm_diffs$level, desc(mm_diffs$estimate))

p= ggplot(mm_diffs)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level),
                  #position = position_nudge(y = -1/10),
                  col="black")+ 
  ylab("")+
  xlab("Difference marginal means males-females (profiles)")+
  geom_vline(aes(xintercept = 0),
             col="darkgrey")+
  scale_x_continuous(breaks=seq(-0.2,0.2, by=0.1), limits = c(-0.2, 0.2))+
  theme(text = element_text(size=15))

p

#ggsave(paste0(output_wd, "ACIE (mm) Gender con Profession, diff_eng.png"), p, width = 12, height = 8)


######
# Profession con ingroup (cj), MM
#####

mm <- cj(data,
         chosen ~ Profession,
         id = ~id,
         estimate = "mm",
         by = ~Group)

v <- list()

for(i in unique(mm$feature))
{ 
  data1 = mm |>
    filter(feature == i & BY == "Ingroup")
  
  data2 = mm |>
    filter(feature == i & BY == "Unknown") 
  
  data3 = mm |>
    filter(feature == i & BY == "Outgroup")
  
  p=ggplot(data1)+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = 1/10),
                    col="green",
                    shape = 17,
                    size=2)+
    geom_pointrange(data = data2,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    size=2,
                    col="grey")+
    geom_pointrange(data = data3,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = -1/10),
                    col="red",
                    shape = 15,
                    size=2)+
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks=seq(0.1,0.9, by=0.1), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
    theme(text = element_text(size=25))
  
  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Marginal means")+labs(caption="Circle = Unknown\nTriangle = Ingroup\nSquare = Outgroup")

p = v[[1]]

p

ggsave(paste0(output_wd, "ACIE (mm) Group con Profession_eng.png"), p, height = 8, width = 12)

# plotto le differenze ingroup vs outgroup

aus = data |>
  filter(Group != "Unknown")

aus$Group = factor(aus$Group, levels = c("Outgroup", "Ingroup"))

mm_diffs <- cj(aus,
               chosen ~ Profession,
               id = ~id,
               estimate = "mm_differences",
               by = ~Group)

mm_diffs$level = fct_reorder(mm_diffs$level, desc(mm_diffs$estimate))


p = ggplot(mm_diffs)+
  geom_vline(aes(xintercept=0), col="black", alpha=1/4)+
  geom_pointrange(data = mm_diffs,
                  aes(x=estimate, xmin=lower, xmax=upper, y=level))+
  ylab("")+
  xlab("Marginal means differences Ingroup-Outgroup")+
  scale_x_continuous(breaks=seq(-0.5,0.5, by=0.1), limits = c(0-0.05,
                                                              max(mm_diffs$upper)+0.05))+
  theme(text = element_text(size=15))

p

#ggsave(paste0(output_wd, "ACIE (mm) Group con Profession, diff ingroup vs outgroup_eng.png"), p, width = 12, height = 8)



# plotto le differenze ingroup vs Unknown

aus = data |>
  filter(Group != "Outgroup")

aus$Group = factor(aus$Group, levels = c("Unknown", "Ingroup"))

mm_diffs <- cj(aus,
               chosen ~ Profession,
               id = ~id,
               estimate = "mm_differences",
               by = ~Group)

mm_diffs$level = fct_reorder(mm_diffs$level, desc(mm_diffs$estimate))


p = ggplot(mm_diffs)+
  geom_vline(aes(xintercept=0), col="black", alpha=1/4)+
  geom_pointrange(data = mm_diffs,
                  aes(x=estimate, xmin=lower, xmax=upper, y=level))+
  ylab("")+
  xlab("Marginal means differences Ingroup-Unknown")+
  scale_x_continuous(breaks=seq(-0.5,0.5, by=0.1), limits = c(0-0.05,
                                                              max(mm_diffs$upper)+0.05))+
  theme(text = element_text(size=15))

p

#ggsave(paste0(output_wd, "ACIE (mm) Group con Profession, diff ingroup vs non noto_eng.png"), p, width = 12, height = 8)


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
      gender ~ "Gender",
      age_r ~ "Age",
      macroarea1 ~ "Macroarea",
      educ ~ "Education",
      socposition_SQ001 ~ "Self-perceived social position"
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
      ideology_SQ001 ~ "Political ideology", 
      vote ~ "Voting preference", 
      interest ~ "Political interest", 
      exposure ~ "News media exposure", 
      trust_gove_r ~ "Trust in government",
      trust_part_r ~ "Trust in parties",
      trust_medi_r ~ "Trust in media",
      trust_expe_r ~ "Trust in expert"
    )
  )

descriptive_politics |>
  as_gt() |>
  gtsave(filename = paste0(output_wd,"Descriptive_pol.docx"))


# Le altre domande: COMPETENCEle materie di competenza del Politologist. 
#Pe rcompetece faccio grouped bar plot con le professioni
competence_index = which(grepl("competence", names(datanomiss)))

competence_variables = names(datanomiss)[competence_index]
#translate it

competence_variables[3]
competence_labels = attributes(data_labeled)$variable.labels[competence_index]
xlabs =sub("^\\[(.*?)\\].*$", "\\1", competence_labels)


xlabs
#translate it!

xlabs =  c("Conflicts in the middle-east",
           "European elections",
           "Taxation reforms",
           "Violence on women",
           "Constitutional reforms",
           "Neofascist movements",
           "Government crisis")

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

title = "For each topic, indicate which kind of guest do you see as more competent to discuss it"#break_string(gsub("^\\[.*?\\]\\s*", "", correctanswer_labels[1]), max_length = 80)


p =v[[1]]+v[[2]]+v[[3]]+v[[4]]+v[[5]]+v[[6]]+v[[7]]+plot_layout(ncol = 3)+plot_annotation(title=title)

p

#ggsave(paste0(output_wd,"competence_descriptive_eng.png"), p, height = 10, width = 10)

#Su quella delle associazioni: diamond (geom_point) in orizzontale ordinando per il voto
#medio per ogni caratteristica

associations_index = which(grepl("associations", names(datanomiss)) & !grepl("__r", names(datanomiss)))

associations_variables = names(datanomiss)[associations_index]
associations_labels = attributes(data_labeled)$variable.labels[associations_index]

xlabs =sub("^\\[(.*?)\\].*$", "\\1", associations_labels)
#translate!

xlabs = c("Competent",
          "Expresses himself in an understandable way",
          "Impartial",
          "Discusses without raising his voice",
          "Snob",
          "Uses numerical data and statistical analysis",
          "Politically leftist",
          "Boring",
          "Analyzes concrete cases by conducting interviews and going into the field",
          "Develops theories to explain political facts",
          "Sympathetic",
          "Authoritative",
          "Presumptuous",
          "Rigorous",
          "Male",
          "Is 60 years old or older",
          "Analyzes facts without being influenced by his opinions")
xlabs

for(i in 1:length(xlabs))
{
  xlabs[i] = break_string(xlabs[i], max_length = 40)
}

title = break_string("Now some characteristics will be listed. How well do you think each of these characteristics reflects the figure of the politologist?", 
                     max_length = 80)

datanomiss[associations_variables]

table(datanomiss[associations_variables[1]])

for(i in associations_variables)
{
  datanomiss = datanomiss |>
    mutate(!!i := recode(!!sym(i),
                         `Per nulla` = "Not at all",
                         `Poco` = "A little",
                         `Né poco, né molto` = "Neither little, nor much",
                         `Abbastanza` = "Much",
                         `Molto` = "A lot"))
}

table(datanomiss[associations_variables[1]])
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
#ggsave(paste0(output_wd,"associations_descriptive_eng.png"), p, height = 16, width = 12)

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
# saveWorkbook(wb, paste0(output_wd, "correctanswerstables_eng.xlsx"), overwrite = TRUE)
# 

### now i try to do a graph with correct answer


correctanswer_index = which(grepl("correctanswer", names(datanomiss)) & !grepl("__r", names(datanomiss)))

correctanswer_variables = names(datanomiss)[correctanswer_index]
correctanswer_labels = attributes(data_labeled)$variable.labels[correctanswer_index]

xlabs =sub("^\\[(.*?)\\].*$", "\\1", correctanswer_labels)

#translate!

xlabs = c("Development of forecasting models on market trends",
          "Analysis of domestic political dynamics and international relations",
          "Art history studies",
          "Analysis of legal systems and law enforcement",
          "Analysis of behavior and social structures",
          "History of Modern and Contemporary India",
          "Ethics, logic and metaphysics",
          "Botanical garden design",
          "Analysis of election results")
xlabs
for(i in 1:length(xlabs))
{
  xlabs[i] = break_string(xlabs[i], max_length = 45)
}

title = "Which profession do you associate each topic which?"#break_string(gsub("^\\[.*?\\]\\s*", "", correctanswer_labels[1]), max_length = 80)

datanomiss[correctanswer_variables]

table(datanomiss[correctanswer_variables[1]])
for(i in correctanswer_variables)
{
  datanomiss = datanomiss |>
    mutate(!!i := recode(!!sym(i),
                         `Economista` = "Economist",
                         `Filosofo/a` = "Philosopher",
                         `Giurista` = "Jurist",
                         `Politologo/a` = "Politologist",
                         `Sociologo/a` = "Sociologist",
                         `Nessuna delle precedenti` = "None of the above"))
}

View(datanomiss[correctanswer_variables])
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

p

#ggsave(paste0(output_wd,"correctanswer_descriptive_eng.png"), p, height = 12, width = 12)

