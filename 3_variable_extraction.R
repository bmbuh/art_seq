#Coded by: Brian Buh
#Started on: 12.10.2022
#Last Updated: 

library(tidyverse)
library(haven)
library(lubridate)


###########################################################################
# UKHLS -------------------------------------------------------------------
###########################################################################
# 
# # -------------------------------------------------------------------------
# # Household datasets ------------------------------------------------------
# # -------------------------------------------------------------------------
# 
# #Sorting out needed variables from indresp
# #Changes in these lists allow for much quick adding and subtracting variables
# wave_varhh <- c("hidp", "fihhmnnet3_dv", "fihhmnnet4_dv", "houscost1_dv", "houscost2_dv", "xpmg", "rent", "rentg", "rentgrs_dv", 
#                 "hsownd", "tenure_dv", "hsrooms", "hsbeds", "hhsize")
# 
# #Add the wave prefix to the variable list
# w1_varhh <- paste0('a_', wave_varhh)
# 
# w2_varhh <- paste0('b_', wave_varhh)
# 
# w3_varhh <- paste0('c_', wave_varhh)
# 
# w4_varhh <- paste0('d_', wave_varhh)
# 
# w5_varhh <- paste0('e_', wave_varhh)
# 
# w6_varhh <- paste0('f_', wave_varhh)
# 
# w7_varhh <- paste0('g_', wave_varhh)
# 
# w8_varhh <- paste0('h_', wave_varhh)
# 
# w9_varhh <- paste0('i_', wave_varhh)
# 
# w10_varhh <- paste0('j_', wave_varhh)
# 
# w11_varhh <- paste0('k_', wave_varhh)
# 
# 
# #Preparing the variables for merging
# a_hh <- a_hhresp %>% 
#   dplyr::select(w1_varhh) %>% 
#   rename_with(~ wave_varhh[which(w1_varhh == .x)], .cols = w1_varhh) %>% 
#   mutate(wave = 1)
# 
# b_hh <- b_hhresp %>% 
#   dplyr::select(w2_varhh)%>% 
#   rename_with(~ wave_varhh[which(w2_varhh == .x)], .cols = w2_varhh) %>% 
#   mutate(wave = 2)
# 
# c_hh <- c_hhresp %>% 
#   dplyr::select(w3_varhh) %>% 
#   rename_with(~ wave_varhh[which(w3_varhh == .x)], .cols = w3_varhh) %>% 
#   mutate(wave = 3)
# 
# d_hh <- d_hhresp %>% 
#   dplyr::select(w4_varhh)%>% 
#   rename_with(~ wave_varhh[which(w4_varhh == .x)], .cols = w4_varhh) %>% 
#   mutate(wave = 4)
# 
# e_hh <- e_hhresp %>% 
#   dplyr::select(w5_varhh) %>% 
#   rename_with(~ wave_varhh[which(w5_varhh == .x)], .cols = w5_varhh) %>% 
#   mutate(wave = 5)
# 
# f_hh <- f_hhresp %>% 
#   dplyr::select(w6_varhh)%>% 
#   rename_with(~ wave_varhh[which(w6_varhh == .x)], .cols = w6_varhh) %>% 
#   mutate(wave = 6)
# 
# g_hh <- g_hhresp %>% 
#   dplyr::select(w7_varhh) %>% 
#   rename_with(~ wave_varhh[which(w7_varhh == .x)], .cols = w7_varhh) %>% 
#   mutate(wave = 7)
# 
# h_hh <- h_hhresp %>% 
#   dplyr::select(w8_varhh)%>% 
#   rename_with(~ wave_varhh[which(w8_varhh == .x)], .cols = w8_varhh) %>% 
#   mutate(wave = 8)
# 
# i_hh <- i_hhresp %>% 
#   dplyr::select(w9_varhh) %>% 
#   rename_with(~ wave_varhh[which(w9_varhh == .x)], .cols = w9_varhh) %>% 
#   mutate(wave = 9)
# 
# j_hh <- j_hhresp %>% 
#   dplyr::select(w10_varhh)%>% 
#   rename_with(~ wave_varhh[which(w10_varhh == .x)], .cols = w10_varhh) %>% 
#   mutate(wave = 10)
# 
# k_hh <- k_hhresp %>% 
#   dplyr::select(w11_varhh)%>% 
#   rename_with(~ wave_varhh[which(w11_varhh == .x)], .cols = w11_varhh) %>% 
#   mutate(wave = 11)
# 
# hh_ukhls <-
#   bind_rows(a_hh, b_hh) %>%
#   bind_rows(., c_hh) %>%
#   bind_rows(., d_hh) %>%
#   bind_rows(., e_hh) %>%
#   bind_rows(., f_hh) %>%
#   bind_rows(., g_hh) %>%
#   bind_rows(., h_hh) %>%
#   bind_rows(., i_hh) %>%
#   bind_rows(., j_hh) %>% 
#   bind_rows(., k_hh) %>%
#   relocate("wave", .after = "hidp") %>%
#   arrange(hidp, wave)
# 
# saveRDS(hh_ukhls, file = "hh_ukhls.rds")


# -------------------------------------------------------------------------
# Individual datasets  ----------------------------------------------------
# -------------------------------------------------------------------------

xwavedat <- xwave %>% 
  select(pidp, ukborn)

#Sorting out needed variables from indresp
#Changes in these lists allow for much quick adding and subtracting variables
wave2_var <- c("hhorig", "hidp", "sex", "birthm", "birthy", "istrtdatm", "istrtdaty", "age_dv", "qfhigh_dv", "hiqual_dv",
              "gor_dv", "marstat_dv", "jbstat", "plbornc", "jbisco88_cc",
              "preg", "pregout1", "pregend1", "pregfert1", "pregfert2")

wave7_var <- c("hhorig", "hidp", "sex", "birthm", "birthy", "istrtdatm", "istrtdaty", "age_dv", "qfhigh_dv", "hiqual_dv",
              "gor_dv", "marstat_dv", "jbstat", "plbornc", "jbisco88_cc",
              "preg", "pregout1", "pregend1", "pregfert1", "pregfert2", "pregft11", "pregft21", "pregft31", "pregft41", "pregft51", "pregft61")

#Add the wave prefix to the variable list
# w1_var <- paste0('a_', wave_var)

w2_var <- paste0('b_', wave2_var)

w3_var <- paste0('c_', wave2_var)

w4_var <- paste0('d_', wave2_var)

w5_var <- paste0('e_', wave2_var)

w6_var <- paste0('f_', wave2_var)

w7_var <- paste0('g_', wave7_var)

w8_var <- paste0('h_', wave7_var)

w9_var <- paste0('i_', wave7_var)

w10_var <- paste0('j_', wave7_var)

w11_var <- paste0('k_', wave7_var)


#Preparing the variables for merging
# a_ind <- a_indresp %>% 
#   dplyr::select("pidp", w1_var) %>% 
#   rename_with(~ wave1_var[which(w1_var == .x)], .cols = w1_var) %>% 
#   mutate(wave = 1) 

b_ind <- b_indresp %>% 
  dplyr::select("pidp", w2_var)%>% 
  rename_with(~ wave2_var[which(w2_var == .x)], .cols = w2_var) %>% 
  mutate(wave = 2) %>% 
  mutate(pregft11 = NA,
         pregft21 = NA,
         pregft31 = NA,
         pregft41 = NA,
         pregft51 = NA,
         pregft61 = NA)

c_ind <- c_indresp %>% 
  dplyr::select("pidp", w3_var) %>% 
  rename_with(~ wave2_var[which(w3_var == .x)], .cols = w3_var) %>% 
  mutate(wave = 3) %>% 
  mutate(pregft11 = NA,
         pregft21 = NA,
         pregft31 = NA,
         pregft41 = NA,
         pregft51 = NA,
         pregft61 = NA)

d_ind <- d_indresp %>% 
  dplyr::select("pidp", w4_var)%>% 
  rename_with(~ wave2_var[which(w4_var == .x)], .cols = w4_var) %>% 
  mutate(wave = 4) %>% 
  mutate(pregft11 = NA,
         pregft21 = NA,
         pregft31 = NA,
         pregft41 = NA,
         pregft51 = NA,
         pregft61 = NA)

e_ind <- e_indresp %>% 
  dplyr::select("pidp", w5_var) %>% 
  rename_with(~ wave2_var[which(w5_var == .x)], .cols = w5_var) %>% 
  mutate(wave = 5) %>% 
  mutate(pregft11 = NA,
         pregft21 = NA,
         pregft31 = NA,
         pregft41 = NA,
         pregft51 = NA,
         pregft61 = NA)

f_ind <- f_indresp %>% 
  dplyr::select("pidp", "f_qfhighoth", w6_var)%>% 
  rename_with(~ wave2_var[which(w6_var == .x)], .cols = w6_var) %>% 
  mutate(wave = 6) %>% 
  mutate(pregft11 = NA,
         pregft21 = NA,
         pregft31 = NA,
         pregft41 = NA,
         pregft51 = NA,
         pregft61 = NA)

g_ind <- g_indresp %>% 
  dplyr::select("pidp", w7_var) %>% 
  rename_with(~ wave7_var[which(w7_var == .x)], .cols = w7_var) %>% 
  mutate(wave = 7)

h_ind <- h_indresp %>% 
  dplyr::select("pidp", w8_var)%>% 
  rename_with(~ wave7_var[which(w8_var == .x)], .cols = w8_var) %>% 
  mutate(wave = 8)

i_ind <- i_indresp %>% 
  dplyr::select("pidp", w9_var) %>% 
  rename_with(~ wave7_var[which(w9_var == .x)], .cols = w9_var) %>% 
  mutate(wave = 9)

j_ind <- j_indresp %>% 
  dplyr::select("pidp", w10_var)%>% 
  rename_with(~ wave7_var[which(w10_var == .x)], .cols = w10_var) %>% 
  mutate(wave = 10)

k_ind <- k_indresp %>% 
  dplyr::select("pidp", w11_var)%>% 
  rename_with(~ wave7_var[which(w11_var == .x)], .cols = w11_var) %>% 
  mutate(wave = 11)

art_ukhls <-
  bind_rows(b_ind, c_ind) %>%
  bind_rows(., d_ind) %>%
  bind_rows(., e_ind) %>%
  bind_rows(., f_ind) %>%
  bind_rows(., g_ind) %>%
  bind_rows(., h_ind) %>%
  bind_rows(., i_ind) %>%
  bind_rows(., j_ind) %>% 
  bind_rows(., k_ind) %>%
  relocate("wave", .after = "pidp") %>%
  relocate("hhorig", .after = "wave") %>%
  arrange(pidp, wave) %>% 
  left_join(., xwavedat, by = "pidp")

saveRDS(art_ukhls, file = "art_ukhls.rds")

# # -------------------------------------------------------------------------
# # Combined dataset --------------------------------------------------------
# # -------------------------------------------------------------------------
# 
# 
# indhhukhls <- left_join(ind_ukhls, hh_ukhls, by= c("hidp","wave")) %>% 
#   arrange(pidp, wave) %>%
#   rename("ukhlswave" = "wave") %>%
#   mutate(wave = ukhlswave + 18) %>%
#   relocate(wave, .after = "pidp")
# 
# saveRDS(indhhukhls, file = "indhhukhls.rds")














