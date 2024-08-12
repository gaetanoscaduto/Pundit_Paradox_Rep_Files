pacman::p_load(
  cregg, dplyr, ggpubr, cowplot, 
  MASS, cjoint, corrplot, dplyr, 
  forcats, ggplot2, gt, gtools, 
  gtsummary, margins, openxlsx, 
  patchwork, rio, texreg, tools
)

setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/Pundit_Paradox_Rep_Files")


import_wd = paste0(getwd(), "/data/survey/")

data = import(paste0(import_wd, "cjdata_ready.RDS"))

output_wd = paste0(getwd(), "/outputs/survey/eng/")

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

table(data$c_job_r, useNA = "always")

data <- data %>%
  mutate(c_job_r = case_when(
    c_job_r == "Giornalista" ~ "Journalist",
    c_job_r == "Economista" ~ "Economist",
    c_job_r == "Giurista" ~ "Jurist",
    c_job_r == "Filosofo/a" ~ "Philosopher",
    c_job_r == "Politologo/a" ~ "Political scientist"))

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
data$c_inoutgr =ifelse(data$c_inoutgr == "Non note", "Political group unknown", data$c_inoutgr)
data$c_inoutgr =ifelse(data$c_inoutgr == "Ingroup", "Political ingroup", data$c_inoutgr)
data$c_inoutgr =ifelse(data$c_inoutgr == "Outgroup", "Political outgroup", data$c_inoutgr)
table(data$c_inoutgr, useNA = "always")

data$Group = factor(data$c_inoutgr, levels = c("Political group unknown", "Political ingroup", "Political outgroup"))

table(data$Group, useNA = "always")

data$c_inoutgr_gender = factor(data$c_inoutgr_gender)
data$c_inoutgr_age = factor(data$c_inoutgr_age)
data$c_inoutgr_macroregion = factor(data$c_inoutgr_macroregion)

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

#### Is randomization alright?

plot(cj_freqs(data, chosen ~ Gender + Age + Area + Profession + Ideology + Group, id = ~id), col="grey")


#or with ggplot
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

ggsave(paste0(output_wd,"diagnostic randomization_eng.png"), p, height = 10, width = 8)


#### Do people prefer the profile to the right??

data$profile_fac <- factor(data$profile_number)
plot(cj(data, 
        chosen ~ Gender + Age + Area + Profession + Ideology + Group, 
        id = ~id,
        by = ~profile_fac,
        group = "profile_fac",
        estimate = "mm"),
     vline = 0.5)

#no particular preference for profile to the right

#####

#################################
##### GENERAL  ANALYSIS #########
#################################

######
### Marginal means: Figure 6: Effect of guest characteristics on perceived trustworthiness  
######

mm <- cj(data,
         chosen ~ Gender + Age + Area + Profession + Ideology,
         id = ~id,
         estimate = "mm")

mm$variable = paste0("(", mm$feature, ") \n", mm$level)

mm$variable <- fct_reorder(mm$variable, desc(mm$estimate))


p = ggplot(mm)+
  geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable), col='black')+
  ylab("Attribute")+
  xlab("Marginal mean")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13))

p

ggsave(paste0(output_wd,"mm_general_eng.png"), p, height = 12, width = 8)


######
### Marginal means: Figure 7: Effect of Ingroup/Outgroupness 
######


mm <- cj(data,
         chosen ~ c_inoutgr_gender + c_inoutgr_age + c_inoutgr_macroregion + Group,
         id = ~id,
         estimate = "mm")


mm$variable <- fct_reorder(mm$level, desc(mm$estimate))


p = ggplot(mm)+
  geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
  geom_pointrange(aes(x=estimate, xmin=lower, xmax=upper, y=variable), col='black')+
  ylab("(Recoded) Attribute")+
  xlab("Marginal mean")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13))

p

ggsave(paste0(output_wd,"mm_inoutgr_eng.png"), p, height = 12, width = 8)


#################################
#### CJ INTERACTIONS (ACIE) ####
#################################


# #####
# Profession with Gender (cj), MM. Figure 7: Average effect of 
#profession on perceived trustworthiness conditional on the gender of the guest
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
                    col="darkgrey",
                    shape = 17,
                    size=1.3)+
    geom_pointrange(data = data2,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = -1/10),
                    col="black",
                    size=1.3)+
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks=seq(0.1,0.9, by=0.1), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
    theme_minimal()+
    theme(text = element_text(size=15),
          axis.text.x =  element_text(size=15),
          axis.text.y =  element_text(size=15))
  
  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Marginal means")+labs(caption="Triangle = Females (profiles)\n Circles = Males (profiles)")

p = v[[1]]

p

ggsave(paste0(output_wd, "ACIE (mm) Gender_with_Profession_eng.png"), p, height = 8, width = 12)


######
# Profession with ingroup (cj), MM. Figure 8: Average effect of the
#profession on perceived trustworthiness conditional on the ideological position
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
                    col="darkgrey",
                    shape = 17,
                    size=1.3)+
    geom_pointrange(data = data2,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    size=1.3)+
    geom_pointrange(data = data3,
                    aes(x=estimate, xmin=lower, xmax=upper, y=level),
                    position = position_nudge(y = -1/10),
                    col="darkgrey",
                    shape = 15,
                    size=1.3)+
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks=seq(0.1,0.9, by=0.1), limits = c(min(mm$lower)-0.01,max(mm$upper)+0.01))+
    theme_minimal()+
    theme(text = element_text(size=15),
          axis.text.x = element_text(size=15),
          axis.text.y = element_text(size=15)
          )
          
  
  v[[i]] <- p
}

v[[length(v)]] = v[[length(v)]]+xlab("Marginal means")+labs(caption="Circle = Unknown\nTriangle = Ingroup\nSquare = Outgroup")

p = v[[1]]

p

ggsave(paste0(output_wd, "ACIE (mm) Group_with_Profession_eng.png"), p, height = 8, width = 12)

