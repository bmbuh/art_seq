#Coded by: Brian Buh
#Started on: 12.10.2022
#Last Updated: 

library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)



#Load data  art_ukhls (s3)
art_ukhls <- file.choose()
art_ukhls <- readRDS(art_ukhls)

###########################################################################
# Descriptive Analysis ----------------------------------------------------
###########################################################################

#Men have only NA in all pregnancy related questions

artdesc <- art_ukhls %>% 
  mutate(preg = ifelse(preg < 0, NA, preg),
         pregout1 = ifelse(pregout1 < 0, NA, pregout1),
         pregend1 = ifelse(pregend1 < 0, NA, pregend1),
         pregfert1 = ifelse(pregfert1 < 0, NA, pregfert1),
         pregfert2 = ifelse(pregfert2 < 0, NA, pregfert2),
         pregft11 = ifelse(pregft11 < 0, NA, pregft11),
         pregft21 = ifelse(pregft21 < 0, NA, pregft21),
         pregft31 = ifelse(pregft31 < 0, NA, pregft31),
         pregft41 = ifelse(pregft41 < 0, NA, pregft41),
         pregft51 = ifelse(pregft51 < 0, NA, pregft51),
         pregft61 = ifelse(pregft61 < 0, NA, pregft61)) %>% 
  filter(age_dv >= 30 & age_dv <= 50, sex == 2) %>% #Filtering is done here
  group_by(pidp) %>% 
  mutate(numobs = length(wave),
         qfhigh_dv = ifelse(qfhigh_dv < 0, NA, qfhigh_dv)) %>% 
  fill(qfhigh_dv, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(edu_cat = case_when(
    qfhigh_dv <= 6 ~ "high",
    qfhigh_dv <= 12 & qfhigh_dv >=7 ~ "medium",
    qfhigh_dv >=13 & qfhigh_dv <= 15 ~ "low")) %>% 
  mutate(edu_cat = ifelse(is.na(edu_cat), "other", edu_cat)) %>% #other edu_qf for people at 16, 96 or missing
  mutate(hiqual_edit = ifelse(hiqual_dv == 1 | hiqual_dv == 2, 5.1, ifelse(hiqual_dv == 3, 3.1, ifelse(hiqual_dv == 4, 2.1, ifelse(hiqual_dv == 5, 2.1, NA))))) %>% 
  mutate(hiqual_edit = ifelse(hiqual_edit == 2.1, 2, ifelse(hiqual_edit == 3.1, 3, ifelse(hiqual_edit == 5.1, 5, NA)))) %>% 
  # mutate(edu = ifelse(qfhigh_dv == 96 | qfhigh_dv <= 0, hiqual_edit, qfhigh_dv)) %>% 
  mutate(isced97 = case_when(
    qfhigh_dv == 1 ~ "6",
    qfhigh_dv >= 2 & qfhigh_dv <= 4 | qfhigh_dv == 6 ~ "5",
    # qfhigh_dv == 6 ~ "5",
    qfhigh_dv == 5 ~ "4",
    qfhigh_dv >=7 & qfhigh_dv <= 12 ~ "3",
    qfhigh_dv >= 13 & qfhigh_dv <= 16 ~ "2",
    qfhigh_dv == 96 ~"96",
    qfhigh_dv == -8 ~"-8",
    qfhigh_dv == -9 ~"-9")) %>% 
  mutate(isced97 = ifelse(isced97 == 96 | isced97 <= 0, hiqual_edit, isced97)) %>% 
  mutate(isced97 = ifelse(is.na(isced97), hiqual_edit, isced97)) %>% 
  mutate(immedu = ifelse(f_qfhighoth >= 1 & f_qfhighoth <= 3, 6, ifelse(f_qfhighoth == 4, 5, ifelse(f_qfhighoth == 5 | f_qfhighoth == 6, 4, 
                                                                                                    ifelse(f_qfhighoth == 7 | f_qfhighoth == 8, 3, 
                                                                                                           ifelse(f_qfhighoth == 9, 2, ifelse(f_qfhighoth == 10, 1, NA))))))) %>%
  mutate(isced97 = ifelse(is.na(isced97), immedu, isced97)) %>% 
  mutate(isced97 = ifelse(is.na(isced97), NA, isced97)) %>% 
  mutate(edu = case_when(
    isced97 == 2 ~ "low",
    isced97 == 3 | isced97 == 4  ~ "medium",
    isced97 == 5 | isced97 == 6  ~ "high",
    is.na(isced97) ~ "NA"),
    edu = ifelse(edu == "NA", NA, edu),
    edu = ifelse(is.na(edu), edu_cat, edu),
    edu = ifelse(edu == "other", NA, edu)) %>% 
  mutate(partner = ifelse(marstat_dv == 1, "married", ifelse(marstat_dv == 2, "cohab", ifelse(marstat_dv < 0, NA, "single"))), #partnership control
         emp = ifelse(jbstat < 0, NA, ifelse(jbstat == 1 | jbstat == 2, "emp", ifelse(jbstat == 3, "unemp", ifelse(jbstat == 7, "student", "inactive")))), #employment control
         ukborn = ifelse(ukborn == -9, NA, ukborn),
         ukborn = ifelse(ukborn == 5, 0, 1)) %>%  #ukborn control
  group_by(pidp) %>% 
  fill(partner, .direction = "downup") %>% 
  fill(emp, .direction = "downup") %>% 
  fill(ukborn, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(pregfert1 = ifelse(pregfert1 == 1, "yes", ifelse(is.na(pregfert1), NA, "no")),
         pregfert1 = as.character(pregfert1),
         pregfert2 = ifelse(pregfert2 == 1, "yes", ifelse(is.na(pregfert2), NA, "no")),
         pregfert2 = as.character(pregfert2),
         preg = ifelse(preg == 1, "Pregnant last interview", ifelse(preg == 2, "Yes, has had pregnancy", "No pregnancies")),
         preg = as.character(preg),
         pregout1 = ifelse(pregout1 == 1, "Live birth, vaginal", 
                           ifelse(pregout1 == 2, "Live birth, caesarean", 
                                  ifelse(pregout1 == 3, "Not live birth", 
                                         ifelse(is.na(pregout1), NA, "Current pregnancy")))),
         pregout1 = as.character(pregout1),
         ukborn = as.character(ukborn))

saveRDS(artdesc, file = "artdesc.rds")


artoutcome <- artdesc %>% count(pregfert1, pregout1)

artdesc %>% count(edu)

# -------------------------------------------------------------------------
# Output ------------------------------------------------------------------
# -------------------------------------------------------------------------

# artdesc_preg %>% 
  

#Descriptive Statistics Table
mycontrols <- tableby.control(test = FALSE)
artdesc_stats <-arsenal::tableby(preg ~ pregfert1 + pregout1 + age_dv + partner + edu + ukborn + emp, 
                                data = artdesc, 
                                control = mycontrols)
labels(artdesc_stats) <-  c(age_dv = "Age", partner = "Partnership status", edu = "Educational attainment", ukborn = "UK Born",
                           emp = "Activity status", pregfert1 = "Used ART (first pregnancy)", pregout1 = "Pregnancy Result")
summary(artdesc_stats)
write2html(artdesc_stats, "artdesc_s4_12-10-2022.html") #UPDATE DATE
write2word(artdesc_stats, "artdesc_s4_12-10-2022.docx") #UPDATE DATE



















