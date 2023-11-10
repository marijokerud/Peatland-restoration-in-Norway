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
  mutate(cm = as.numeric(cm)) %>% 
  mutate(species = gsub("Betula nana B.", "Betula nana", species)) %>% 
  mutate(species = gsub("Betula nana C.", "Betula nana", species)) %>%
  mutate(species = gsub("Betula nana F.", "Betula nana", species)) %>%
  mutate(species = gsub("Picea abies B.", "Picea abies", species)) %>%
  mutate(species = gsub("Pinus sylvestris B.", "Pinus sylvestris", species)) %>%
  mutate(species = gsub("Pinus sylvestris C.", "Pinus sylvestris", species)) %>%
  mutate(AAR2 = 6) %>% 
  mutate(Treatment = "after3") %>% 
  drop_na() %>% 
  select(-CM, -presence, -field_analyst) %>% 
  rename(AAR = year, OMRADE = site, Artslinje_id = transect_id, Art = species)

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

  
