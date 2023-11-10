library(readxl)
library(tidyverse)
library(labdsv)


feltskjema_kaldvassmyra <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "Kaldvassmyra2023", col_names = TRUE)
feltskjema_aurstadmosan <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "Aurstadmåsan2023", col_names = TRUE)
feltskjema_trysil <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "Trysil2023", col_names = TRUE)

# DATA CLEANING

#KALDVASSMYRA
kaldvassmyra2023 <- feltskjema_kaldvassmyra %>%
  gather("CM", "presence", -year, -site, -field_analyst, -transect_id, -species) %>% 
  mutate("cm"= paste(CM, 0, sep = "")) %>% 
  drop_na() %>% 
  select(-CM, -presence)

#ÅURSTADMÅSAN
aurstadmosan2023 <- feltskjema_aurstadmosan %>%
  gather("cm", "presence", -year, -site, -field_analyst, -transect_id, -species) %>% 
  drop_na() %>% 
  select(-presence)

#TRYSIL
trysil2023 <- feltskjema_trysil %>%
  gather("cm", "presence", -transect_id, -species) %>% 
  mutate(presence = as.numeric(gsub("x", 1, presence))) %>% 
  drop_na() %>% 
  select(-presence)


  filter(AAR == "2018") %>% 
  rename(Artslinje_id_old = Artslinje_id) %>% 
  mutate(linje1 = str_sub(Artslinje_id_old, start = 1, end = 3)) %>%
  mutate(linje2 = str_sub(Artslinje_id_old, start = 5, end = 5)) %>%
  unite("Artslinje_id", linje1,linje2, sep = "")

  
