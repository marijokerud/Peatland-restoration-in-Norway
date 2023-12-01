#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(xlsx)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

artslinjer.raw <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Data", col_names = TRUE)
plassering <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Plassering", col_names = TRUE)
#artsnavn <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "artsnavn1", col_names = TRUE)
#tv_verdi <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Ind.verdi_GAD_TV",range = "A1:L53" , col_names = TRUE)


# DATA CLEANING
#Kaldvassmyra
artslinjer <- artslinjer.raw %>% 
  select(AAR, OMRADE, Artslinje_id, Art, cm, AAR2, Treatment)


#### COMMUNITY MATRIX every 10 m
pinpoint_matrix<- artslinjer %>% 
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>%
  as.data.frame

pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  select(-dead_sph,-dead_wood,-litter,-Litter,-peat, -water) %>% 
  filter(!row_number() %in% c(46)) # #REMOVE column with only 0s: K3_40_2018


#DO NMDS

write_csv2(artslinjer, file = "Kaldvassmyra.csv", na = "NA")
  