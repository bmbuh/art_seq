#Coded by: Brian Buh
#Started on: 12.10.2022
#Last Updated: 

library(tidyverse)
library(haven)
library(lubridate)

###########################################################################
# Extract the UKHLS fertility histories -----------------------------------
###########################################################################

# Fertility Histories in the UKHLS have two parts, births that occur before the first interview (retrospective) and births that occur during the observation period (prospecitve)
# The first code section combines the retrospective birth data
# The second code section extracts the prospective birth data

# -------------------------------------------------------------------------
# Retrospective fertility history -----------------------------------------
# -------------------------------------------------------------------------


#This was updates 09.02.2021 to reflect that people enter waves later
# a_sample <- a_indall %>% 
#   dplyr::select(pidp, pid, a_hhorig, a_sex, a_doby_dv, a_dobm_dv) %>% 
#   rename("mpidp" = "pidp") %>% 
#   rename("mdoby" = "a_doby_dv") %>% 
#   rename("mdobm" = "a_dobm_dv") %>% 
#   unite(mdob, c(mdobm, mdoby), sep = "-") %>% 
#   mutate(mdob = parse_date_time(mdob, "my"))

#Using xwave data to catch all possible respondents
x_sample <- xwave %>% 
  dplyr::select(pidp, pid, hhorig, sex, birthm, birthy, anychild_dv) %>% 
  rename("doby" = "birthy") %>% 
  rename("dobm" = "birthm") %>% 
  unite(dob, c(dobm, doby), sep = "-") %>% 
  mutate(dob = parse_date_time(dob, "my"))

#The DF a_parent comes from the dataset Stata code from Alita Nandi
parent_sample <- a_parent %>% 
  filter(a_mnpid >0) %>% 
  rename("kpidp" = "pidp") %>% 
  rename("mpidp" = "a_mnpid") %>%
  rename("kbirthm" = "a_birthm") %>% 
  rename("kbirthy" = "a_birthy")


res_child <- a_natchild %>% 
  dplyr::select(pidp, a_lchlv, a_lchdoby, a_lchdobm, a_childno) %>%
  filter(a_lchlv == 2) %>% 
  rename("mpidp" = "pidp") %>% 
  rename("kbirthm" = "a_lchdobm") %>% 
  rename("kbirthy" = "a_lchdoby") %>% 
  mutate(kbirthm = ifelse(kbirthm < 0, NA, kbirthm)) %>% 
  mutate(kbirthy = ifelse(kbirthy < 0, NA, kbirthy)) %>% 
  dplyr::select(-a_lchlv)

combined_child <-
  bind_rows(res_child, parent_sample) %>% 
  dplyr::select(mpidp, kbirthy, kbirthm, a_childno) %>% 
  mutate(wave = 1) %>% 
  mutate(fpidp = NA)

# -------------------------------------------------------------------------
# Building prospective fertility histories --------------------------------
# -------------------------------------------------------------------------


b_newchild <- b_child %>% 
  filter(b_ynew == 1) %>% 
  dplyr::select(pidp, pid, b_ynew, b_birthy, b_birthm, b_mnpid, b_fnpid) %>% 
  rename("fpidp" = "b_fnpid") %>% 
  rename("ynew" =  "b_ynew") %>% 
  rename("kbirthy" =  "b_birthy") %>% 
  rename("kbirthm" =  "b_birthm") %>% 
  rename("mpidp" =  "b_mnpid") %>% 
  mutate(wave = 2)

c_newchild <- c_child %>% 
  filter(c_ynew == 1) %>% 
  dplyr::select(pidp, pid, c_ynew, c_birthy, c_birthm, c_mnpid, c_fnpid) %>% 
  rename("fpidp" = "c_fnpid") %>%  
  rename("ynew" =  "c_ynew") %>% 
  rename("kbirthy" =  "c_birthy") %>% 
  rename("kbirthm" =  "c_birthm") %>% 
  rename("mpidp" =  "c_mnpid") %>% 
  mutate(wave = 3)

d_newchild <- d_child %>% 
  filter(d_ynew == 1) %>% 
  dplyr::select(pidp, pid, d_ynew, d_birthy, d_birthm, d_mnpid, d_fnpid) %>% 
  rename("fpidp" = "d_fnpid") %>% 
  rename("ynew" =  "d_ynew") %>% 
  rename("kbirthy" =  "d_birthy") %>% 
  rename("kbirthm" =  "d_birthm") %>% 
  rename("mpidp" =  "d_mnpid") %>% 
  mutate(wave = 4)

e_newchild <- e_child %>% 
  filter(e_ynew == 1) %>% 
  dplyr::select(pidp, pid, e_ynew, e_birthy, e_birthm, e_mnpid, e_fnpid) %>% 
  rename("fpidp" = "e_fnpid") %>%  
  rename("ynew" =  "e_ynew") %>% 
  rename("kbirthy" =  "e_birthy") %>% 
  rename("kbirthm" =  "e_birthm") %>% 
  rename("mpidp" =  "e_mnpid") %>% 
  mutate(wave = 5)

f_newchild <- f_child %>% 
  filter(f_ynew == 1) %>% 
  dplyr::select(pidp, pid, f_ynew, f_birthy, f_birthm, f_mnpid, f_fnpid) %>% 
  rename("fpidp" = "f_fnpid") %>%  
  rename("ynew" =  "f_ynew") %>% 
  rename("kbirthy" =  "f_birthy") %>% 
  rename("kbirthm" =  "f_birthm") %>% 
  rename("mpidp" =  "f_mnpid") %>% 
  mutate(wave = 6)

g_newchild <- g_child %>% 
  filter(g_ynew == 1) %>% 
  dplyr::select(pidp, pid, g_ynew, g_birthy, g_birthm, g_mnpid, g_fnpid) %>% 
  rename("fpidp" = "g_fnpid") %>%  
  rename("ynew" =  "g_ynew") %>% 
  rename("kbirthy" =  "g_birthy") %>% 
  rename("kbirthm" =  "g_birthm") %>% 
  rename("mpidp" =  "g_mnpid") %>% 
  mutate(wave = 7)

h_newchild <- h_child %>% 
  filter(h_ynew == 1) %>% 
  dplyr::select(pidp, pid, h_ynew, h_birthy, h_birthm, h_mnpid, h_fnpid) %>% 
  rename("fpidp" = "h_fnpid") %>% 
  rename("ynew" =  "h_ynew") %>% 
  rename("kbirthy" =  "h_birthy") %>% 
  rename("kbirthm" =  "h_birthm") %>% 
  rename("mpidp" =  "h_mnpid") %>% 
  mutate(wave = 8)

i_newchild <- i_child %>% 
  filter(i_ynew == 1) %>% 
  dplyr::select(pidp, pid, i_ynew, i_birthy, i_birthm, i_mnpid, i_fnpid) %>% 
  rename("fpidp" = "i_fnpid") %>% 
  rename("ynew" =  "i_ynew") %>% 
  rename("kbirthy" =  "i_birthy") %>% 
  rename("kbirthm" =  "i_birthm") %>% 
  rename("mpidp" =  "i_mnpid") %>% 
  mutate(wave = 9)

j_newchild <- j_child %>% 
  filter(j_ynew == 1) %>% 
  dplyr::select(pidp, pid, j_ynew, j_birthy, j_birthm, j_mnpid, j_fnpid) %>% 
  rename("fpidp" = "j_fnpid") %>% 
  rename("ynew" =  "j_ynew") %>% 
  rename("kbirthy" =  "j_birthy") %>% 
  rename("kbirthm" =  "j_birthm") %>% 
  rename("mpidp" =  "j_mnpid") %>% 
  mutate(wave = 10)

k_newchild <- k_child %>% 
  filter(k_ynew == 1) %>%  
  dplyr::select(pidp, pid, k_ynew, k_birthy, k_birthm, k_mnpid, k_fnpid) %>% 
  rename("fpidp" = "k_fnpid") %>% 
  rename("ynew" =  "k_ynew") %>% 
  rename("kbirthy" =  "k_birthy") %>% 
  rename("kbirthm" =  "k_birthm") %>% 
  rename("mpidp" =  "k_mnpid") %>% 
  mutate(wave = 11)

child <- 
  bind_rows(b_newchild, c_newchild) %>% 
  bind_rows(., d_newchild) %>% 
  bind_rows(., e_newchild) %>% 
  bind_rows(., f_newchild) %>% 
  bind_rows(., g_newchild) %>% 
  bind_rows(., h_newchild) %>% 
  bind_rows(., i_newchild) %>% 
  bind_rows(., j_newchild) %>% 
  bind_rows(., k_newchild) %>% 
  dplyr::select(-ynew, -pidp)

# -------------------------------------------------------------------------
# Combined fertility histories --------------------------------------------
# -------------------------------------------------------------------------

combined_child2 <- 
  bind_rows(combined_child, child) %>% 
  rename("pidp" = "mpidp") %>% 
  arrange(pidp, kbirthy)

fathers_child <- combined_child2 %>% 
  filter(fpidp > 0) %>% 
  dplyr::select(2:7) %>% 
  rename("pidp" = "fpidp")

combined_child3 <- 
  bind_rows(combined_child2, fathers_child)

combined_child4 <- 
  left_join(combined_child3, x_sample, by = "pidp") %>% 
  unite(kdob, c(kbirthm, kbirthy), sep = "-") %>% 
  mutate(kdob = parse_date_time(kdob, "my")) %>% 
  arrange(pidp, kdob) %>% 
  group_by(pidp) %>% 
  mutate(bno = row_number()) %>% 
  mutate(check = a_childno - bno) %>% 
  ungroup()

saveRDS(combined_child4, file = "fertukhls.rds")
fertukhls <- file.choose()
fertukhls <- readRDS(fertukhls)

#########################################################################
# Descriptives of the data frame ----------------------------------------
#########################################################################


xtabs(~employed + status, emp_his)


###########################################################################
# First born children -----------------------------------------------------
###########################################################################

first_born <- fert_his %>% 
  filter(bno == 1) %>% number 
  ungroup() %>% 
  filter(check == 0 | is.na(check)) %>% 
  dplyr::select(pidp, kdob, sex, hhorig)

test_first_born <- first_born %>%
  mutate(year = year(kdob)) %>% 
  filter(year >= 2008, hhorig == 1 | hhorig == 2 | hhorig == 7)

first_born_ukhls <- first_born %>%
  mutate(year = year(kdob)) %>% 
  filter(year >= 2008, hhorig == 1 | hhorig == 2 | hhorig == 7)

count_firstborn <- test_first_born %>% 
  count(year)