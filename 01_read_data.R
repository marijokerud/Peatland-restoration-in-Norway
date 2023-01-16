library(readxl)


artslinjer <- read_excel(path = "Data Kaldvassmyra.xlsx", sheet = "Data", col_names = TRUE)
tv_verdi <- read_excel(path = "Data Kaldvassmyra.xlsx", sheet = "Ind.verdi_GAD_TV",range = "A1:L53" , col_names = TRUE)
plassering <- read_excel(path = "Data Kaldvassmyra.xlsx", sheet = "Plassering", col_names = TRUE)

# DATA CLEANING
library(tidyverse)
library(labdsv)

# COMMUNITY MATRIX every 10 m
pinpoint_matrix<- artslinjer %>% 
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>% 
  as.data.frame

pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  select(-Litter,-litter,-dead_sph,-dead_wood,-peat, -water) %>% 
  filter(!row_number() %in% c(46)) #REMOVE column with only 0.


# COMMUNITY MATRIX per line
community_matrixK<- artslinjer %>% 
  unite("community", LINJE, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>% 
  as.data.frame

com_matK<- matrify(community_matrixK) 

plassering_short <- plassering %>% 
  select(Artslinje_id, Meter_from_ditch)

#SITE SCORES
site.scores$site <- rownames(Site.scores)  # create a column of site names, from the rownames of data.scores
site.scores <- site.scores %>% 
  mutate(Artslinje_id = site) %>% 
  mutate(Artslinje_id = gsub("_2015", "", Artslinje_id)) %>% #remove _2015
  mutate(Artslinje_id = gsub("_2018", "", Artslinje_id)) %>% #remove _2018
  mutate(Artslinje_id = gsub("_2021", "", Artslinje_id)) %>%  #remove _2021
  left_join(plassering_short) %>% 
  slice(1:69)
